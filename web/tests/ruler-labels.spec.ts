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
		// First label 60px wide (10 chars); a short label 40px later still overlaps.
		const markers = [m(0, "0:00.000"), m(40, "1"), m(200, "2")];
		expect(planRulerLabels(markers, w)).toEqual([true, false, true]);
	});

	test("empty input", () => {
		expect(planRulerLabels([], w)).toEqual([]);
	});
});
