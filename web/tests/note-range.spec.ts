import { test, expect } from "@playwright/test";
import { computeNoteRange } from "../src/lib/midi-notes";
import type { MidiNoteData } from "../src/types";

function note(overrides: Partial<MidiNoteData>): MidiNoteData {
	return {
		pitch: 60,
		time: 0,
		duration: 1,
		velocity: 100,
		offVelocity: 0,
		change: "Unchanged",
		...overrides,
	};
}

test.describe("computeNoteRange old+new bounds", () => {
	test("moved note: range covers both old and new pitch/time extents", () => {
		const r = computeNoteRange([
			note({
				pitch: 72,
				time: 100,
				duration: 4,
				oldPitch: 36,
				oldTime: 0,
				oldDuration: 4,
				change: "Modified",
			}),
		]);
		// Old pitch 36 and new pitch 72 must both fit inside the padded range.
		expect(r.minPitch).toBeLessThanOrEqual(36);
		expect(r.maxPitch).toBeGreaterThanOrEqual(72);
		// Old start 0 and new end 104 must both fit inside the padded range.
		expect(r.minTime).toBeLessThanOrEqual(0);
		expect(r.maxTime).toBeGreaterThanOrEqual(104);
	});

	test("t=0 note: minTime goes negative (left pad, no clamp)", () => {
		const r = computeNoteRange([note({ pitch: 60, time: 0, duration: 1, change: "Added" })]);
		expect(r.minTime).toBeLessThan(0);
	});

	test("positive minima keep their pad (unchanged behavior)", () => {
		const r = computeNoteRange([note({ pitch: 60, time: 10, duration: 2, change: "Added" })]);
		// span 2 -> pad 1 -> minTime 9 (was Math.max(0, 9) = 9, same).
		expect(r.minTime).toBe(9);
		expect(r.maxTime).toBe(13);
	});

	test("shortened note: old end still bounds maxTime", () => {
		const r = computeNoteRange([
			note({
				pitch: 60,
				time: 1,
				duration: 1,
				oldTime: 1,
				oldDuration: 50,
				change: "Modified",
			}),
		]);
		// Old end 51 (1 + 50) must fit; the new end alone (2) would not cover it.
		expect(r.maxTime).toBeGreaterThanOrEqual(51);
	});
});
