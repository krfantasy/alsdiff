import { test, expect } from "@playwright/test";
import {
	computeAutomationRange,
	parseAutomationEvents,
} from "../src/lib/automation-events";
import type { AutomationEvent, ItemView } from "../src/types";

test.describe("computeAutomationRange old+new bounds", () => {
	test("moved event: range covers both old and new time/value extents", () => {
		const events: AutomationEvent[] = [
			{ time: 500, value: 1, oldTime: 0, oldValue: 0.2, change: "Modified" },
		];
		const r = computeAutomationRange(events);
		// Old time 0 and new time 500 must both fit inside the padded range.
		expect(r.minTime).toBeLessThanOrEqual(0);
		expect(r.maxTime).toBeGreaterThanOrEqual(500);
		// Old value 0.2 and new value 1 must both fit inside the padded range.
		expect(r.minValue).toBeLessThanOrEqual(0.2);
		expect(r.maxValue).toBeGreaterThanOrEqual(1);
	});

	test("moved-closer event: old far position still bounds the range", () => {
		const events: AutomationEvent[] = [
			{ time: 0, value: 0.5, oldTime: 500, oldValue: 0.5, change: "Modified" },
		];
		const r = computeAutomationRange(events);
		// Old time 500 must fit; the new time alone (0) would not cover it.
		expect(r.maxTime).toBeGreaterThanOrEqual(500);
	});

	test("value-only change: both old and new values bound the range", () => {
		const events: AutomationEvent[] = [
			{ time: 4, value: 128, oldValue: 100, change: "Modified" },
		];
		const r = computeAutomationRange(events);
		expect(r.minValue).toBeLessThanOrEqual(100);
		expect(r.maxValue).toBeGreaterThanOrEqual(128);
	});
});

/** Legacy flat automation item whose events parse from their item names. */
function legacyAutomation(names: { name: string; change: ItemView["change"] }[]): ItemView {
	return {
		type: "item",
		name: "Automation",
		change: "Modified",
		domain_type: "Automation",
		children: names.map((n) => ({
			type: "item" as const,
			name: n.name,
			change: n.change,
			domain_type: "Event" as const,
		})),
	};
}

test.describe("legacy name parsing normalizes sentinel times", () => {
	test("Added legacy name parses time and value", () => {
		const events = parseAutomationEvents(
			legacyAutomation([{ name: "Event[1] Added: Time=100.0, Value=0.5", change: "Added" }]),
		);
		expect(events).toHaveLength(1);
		expect(events[0].time).toBe(100);
		expect(events[0].value).toBe(0.5);
		expect(events[0].change).toBe("Added");
	});

	test("Added legacy name with -63072000 sentinel normalizes time to 0", () => {
		const events = parseAutomationEvents(
			legacyAutomation([{ name: "Event[0] Added: Time=-63072000, Value=0.5", change: "Added" }]),
		);
		expect(events).toHaveLength(1);
		expect(events[0].time).toBe(0);
	});

	test("Modified legacy name with sentinel old time normalizes oldTime to 0", () => {
		const events = parseAutomationEvents(
			legacyAutomation([
				{ name: "Event[2]: Time: -63072000->4.0, Value: 0.5->0.7", change: "Modified" },
			]),
		);
		expect(events).toHaveLength(1);
		expect(events[0].time).toBe(4);
		expect(events[0].oldTime).toBe(0);
		expect(events[0].oldValue).toBe(0.5);
		expect(events[0].value).toBe(0.7);
	});

	test("sentinel legacy data does not blow up the fitted range", () => {
		const events = parseAutomationEvents(
			legacyAutomation([
				{ name: "Event[0] Added: Time=-63072000, Value=0.5", change: "Added" },
				{ name: "Event[1] Added: Time=100.0, Value=0.5", change: "Added" },
			]),
		);
		expect(events).toHaveLength(2);
		const r = computeAutomationRange(events);
		// Normalized times span 0..100 -> pad 10 -> [-10, 110].
		expect(r.minTime).toBe(-10);
		expect(r.maxTime).toBe(110);
	});
});
