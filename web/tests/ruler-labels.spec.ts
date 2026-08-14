import { test, expect } from "@playwright/test";
import { planRulerLabels, type RulerMarker } from "../src/lib/canvas-utils";

// Monospace-ish fake measurer: 6px per character.
const w = (s: string) => s.length * 6;

function m(pos: number, label = "", isMajor = true): RulerMarker {
	return { pos, label, isMajor };
}

test.describe("planRulerLabels", () => {
	test("keeps labels when spacing is comfortable", () => {
		const markers = [m(0, "1:00"), m(200, "2:00"), m(400, "3:00")];
		expect(planRulerLabels(markers, w)).toEqual([true, true, true]);
	});

	test("drops every other label when they would overlap", () => {
		// Labels are 24px wide + 3px offset; spacing 21px < width + gap → alternate.
		const markers = [m(0, "1:00"), m(21, "1:02"), m(42, "1:04"), m(63, "1:06")];
		expect(planRulerLabels(markers, w)).toEqual([true, false, true, false]);
	});

	test("unlabeled markers never claim space and stay false", () => {
		const markers = [m(0), m(10, "1:00"), m(20), m(500, "5:00")];
		expect(planRulerLabels(markers, w)).toEqual([false, true, false, true]);
	});

	test("long labels push the next keep point further", () => {
		// First label 48px wide (8 chars); a short label 40px later still overlaps.
		const markers = [m(0, "0:00.000"), m(40, "1"), m(200, "2")];
		expect(planRulerLabels(markers, w)).toEqual([true, false, true]);
	});

	test("keeps a marker exactly at the lastKeptEnd + gap boundary", () => {
		// First label: starts at 0 + 3, "1:00" is 24px wide → occupies [3, 27);
		// the next label needs start >= 27 + 12 (gap) = 39.
		// m(36)'s start is exactly 39 — the >= rule KEEPS it (a strict >
		// would drop it). m(39) is the same boundary in marker-position
		// terms (3 + 24 + 12 = 39) and stays kept; m(35) starts one pixel
		// short and is dropped.
		const exactStart = [m(0, "1:00"), m(36, "2")];
		expect(planRulerLabels(exactStart, w)).toEqual([true, true]);
		const exactPos = [m(0, "1:00"), m(39, "2")];
		expect(planRulerLabels(exactPos, w)).toEqual([true, true]);
		const below = [m(0, "1:00"), m(35, "1:58"), m(39, "2")];
		expect(planRulerLabels(below, w)).toEqual([true, false, true]);
	});

	test("explicit gap widens the required clearance", () => {
		const markers = [m(0, "1:00"), m(39, "2"), m(100, "3")];
		// Default gap 12: m(39) clears (start 42 >= 27 + 12).
		expect(planRulerLabels(markers, w)).toEqual([true, true, true]);
		// gap 50: the next label must start at >= 27 + 50 = 77, so m(39)
		// drops and m(100) (start 103) keeps.
		expect(planRulerLabels(markers, w, 50)).toEqual([true, false, true]);
	});

	test("empty input", () => {
		expect(planRulerLabels([], w)).toEqual([]);
	});
});
