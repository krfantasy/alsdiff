import type {
	FieldView,
	ItemView,
	ViewNode,
	CollectionView,
	AutomationEvent,
	AutomationRange,
	CurveControls,
} from "../types";

function clamp01(v: number): number {
	return Math.min(1, Math.max(0, v));
}

// Ableton writes `Time="-63072000"` (exactly -2 years, in seconds) as a
// "no time" sentinel on the initial envelope point of an automation. Such a
// point sits at the start of the arrangement, so normalize it to 0 both for
// the visible range and for rendering positions.
const SENTINEL_TIME = -63072000;

function normalizeTime(t: number | undefined): number | undefined {
	if (t === undefined) return undefined;
	return t <= SENTINEL_TIME + 1 ? 0 : t;
}

// --- Old format: parse from name string ---

const RE_ADDED_REMOVED = /Time=([\d.-]+),\s*Value=([\d.]+)/;
const RE_MODIFIED_TIME = /Time:\s*([\d.-]+)->([\d.]+)/;
const RE_MODIFIED_VALUE = /Value:\s*([\d.]+)->([\d.]+)/;
const RE_CURVE = /Curve1=\(([\d.]+),([\d.]+)\)\s*Curve2=\(([\d.]+),([\d.]+)\)/;

function parseCurve(name: string): CurveControls | undefined {
	const cm = name.match(RE_CURVE);
	if (!cm) return undefined;
	return {
		curve1X: clamp01(parseFloat(cm[1])),
		curve1Y: clamp01(parseFloat(cm[2])),
		curve2X: clamp01(parseFloat(cm[3])),
		curve2Y: clamp01(parseFloat(cm[4])),
	};
}

function parseOldFormat(child: ItemView): AutomationEvent | undefined {
	const name = child.name;

	if (child.change === "Added" || child.change === "Removed") {
		const m = name.match(RE_ADDED_REMOVED);
		if (m) {
			return {
				// The name's time may be Ableton's "no time" sentinel; run it
				// through normalizeTime like the structured parser does.
				time: normalizeTime(parseFloat(m[1])) ?? 0,
				value: parseFloat(m[2]),
				change: child.change,
				curve: parseCurve(name),
			};
		}
	} else if (child.change === "Modified") {
		const tm = name.match(RE_MODIFIED_TIME);
		const vm = name.match(RE_MODIFIED_VALUE);
		if (tm || vm) {
			let time: number;
			if (tm) {
				time = normalizeTime(parseFloat(tm[2])) ?? 0;
			} else {
				const fallback = name.match(/Time=([\d.]+)/);
				time = fallback ? normalizeTime(parseFloat(fallback[1])) ?? 0 : 0;
			}
			return {
				time,
				value: vm ? parseFloat(vm[2]) : 0,
				change: "Modified",
				// The old time can carry the sentinel (the event "moved" from
				// the arrangement start); normalize it too so the ghost marker
				// and the fitted range stay sane.
				oldTime: tm ? normalizeTime(parseFloat(tm[1])) : undefined,
				oldValue: vm ? parseFloat(vm[1]) : undefined,
				curve: parseCurve(name),
			};
		}
	}

	return undefined;
}

// --- New format: parse from structured children ---

function getField(
	children: ViewNode[],
	name: string,
): { old: number | undefined; new: number | undefined } {
	const f = children.find((c) => c.type === "field" && c.name === name);
	if (!f) return { old: undefined, new: undefined };
	const field = f as FieldView;
	return {
		old: typeof field.old_value === "number" ? field.old_value : undefined,
		new: typeof field.new_value === "number" ? field.new_value : undefined,
	};
}

function getCurveControls(
	children: ViewNode[],
	useOld: boolean,
): CurveControls | undefined {
	const curveItem = children.find(
		(c) => c.type === "item" && c.name === "Curve",
	);
	if (!curveItem || !("children" in curveItem) || !curveItem.children)
		return undefined;
	const cc = curveItem.children;
	const c1x = getField(cc, "Curve1 X");
	const c1y = getField(cc, "Curve1 Y");
	const c2x = getField(cc, "Curve2 X");
	const c2y = getField(cc, "Curve2 Y");
	const val = useOld ? c1x.old : (c1x.new ?? c1x.old);
	if (val === undefined) return undefined;
	const y1 = useOld ? (c1y.old ?? 0) : (c1y.new ?? c1y.old ?? 0);
	const x2 = useOld ? (c2x.old ?? 0) : (c2x.new ?? c2x.old ?? 0);
	const y2 = useOld ? (c2y.old ?? 0) : (c2y.new ?? c2y.old ?? 0);
	return {
		curve1X: clamp01(val),
		curve1Y: clamp01(y1),
		curve2X: clamp01(x2),
		curve2Y: clamp01(y2),
	};
}

function parseNewFormat(child: ItemView): AutomationEvent | undefined {
	const fields = child.children ?? [];
	if (fields.length === 0) return undefined;

	const timeF = getField(fields, "Time");
	const valueF = getField(fields, "Value");

	if (child.change === "Added" || child.change === "Removed") {
		// Removed events only carry `old_value`, so fall back to it.
		const t = timeF.new ?? timeF.old;
		if (t === undefined) return undefined;
		return {
			time: normalizeTime(t) ?? 0,
			value: valueF.new ?? valueF.old ?? 0,
			change: child.change,
			curve: getCurveControls(fields, false),
		};
	} else if (child.change === "Modified") {
		const t = timeF.new ?? timeF.old;
		if (t === undefined) return undefined;
		return {
			time: normalizeTime(t) ?? 0,
			value: valueF.new ?? valueF.old ?? 0,
			change: "Modified",
			oldTime: normalizeTime(timeF.old),
			oldValue: valueF.old,
			curve: getCurveControls(fields, false),
			oldCurve: getCurveControls(fields, true),
		};
	}

	return undefined;
}

// --- Main parser ---

// The OCaml worker wraps an Automation's events in a nested
// `{ type: "collection", name: "Events" }` node (see change_projector.ml
// `wrap_events`, driven by automation.ml `[@view.label "Events"]`). Resolve
// the candidate event nodes by descending into that collection when present;
// fall back to the legacy flat shape (events as direct children) so old data
// keeps working.
function resolveEventNodes(automationItem: ItemView): ViewNode[] {
	const children = automationItem.children ?? [];
	const eventsCollection = children.find(
		(c): c is CollectionView => c.type === "collection" && c.name === "Events",
	);
	return eventsCollection ? (eventsCollection.items ?? []) : children;
}

export function parseAutomationEvents(
	automationItem: ItemView,
): AutomationEvent[] {
	const events: AutomationEvent[] = [];
	const children = resolveEventNodes(automationItem);
	if (children.length === 0) return events;

	for (const child of children) {
		if (child.type !== "item" || child.domain_type !== "Event") continue;

		// Try new structured format first (has field/item children with Time/Value)
		const hasStructuredFields = (child.children ?? []).some(
			(c) => c.type === "field" && (c.name === "Time" || c.name === "Value"),
		);

		const event = hasStructuredFields
			? parseNewFormat(child)
			: parseOldFormat(child);

		if (event) events.push(event);
	}

	return events;
}

export function computeAutomationRange(
	events: AutomationEvent[],
): AutomationRange {
	if (events.length === 0) {
		return { minValue: 0, maxValue: 1, minTime: 0, maxTime: 4 };
	}

	let minValue = Infinity;
	let maxValue = -Infinity;
	let minTime = Infinity;
	let maxTime = -Infinity;

	// Times are already normalized by parseAutomationEvents; this loop only
	// needs to handle the default range when there are no events at all.
	for (const e of events) {
		// Bound across BOTH old and new coordinates: a modified event renders
		// at its new position plus a ghost at its old one, so fitting only one
		// side could leave the other outside the canvas.
		for (const v of [e.oldValue, e.value]) {
			if (v === undefined) continue;
			if (v < minValue) minValue = v;
			if (v > maxValue) maxValue = v;
		}
		for (const t of [e.oldTime, e.time]) {
			if (t === undefined) continue;
			if (t < minTime) minTime = t;
			if (t > maxTime) maxTime = t;
		}
	}

	const valuePad = Math.max(0.1, (maxValue - minValue) * 0.15);
	const timePad = Math.max(1, (maxTime - minTime) * 0.1);

	return {
		minValue: minValue - valuePad,
		maxValue: maxValue + valuePad,
		// Negative minTime is intentional — it pads the canvas left edge so
		// t=0 events (the sentinel-normalized master Tempo change) render
		// their markers fully instead of half-clipped.
		minTime: minTime - timePad,
		maxTime: maxTime + timePad,
	};
}
