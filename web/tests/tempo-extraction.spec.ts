import { test, expect } from "@playwright/test";
import { extractTempo, extractTimeSignature } from "../src/lib/diff-parser";
import type { ViewNode } from "../src/types";

function field(
	name: string,
	value: number,
	change = "Modified",
): ViewNode {
	return {
		type: "field",
		name,
		change: change as any,
		domain_type: "Param",
		old_value: change === "Modified" ? 0 : undefined,
		new_value: value,
	} as any;
}

function masterItem(tempoValue?: ViewNode, tsValue?: ViewNode): ViewNode[] {
	return [
		{
			type: "item",
			name: "MainTrack: Main",
			change: "Modified",
			domain_type: "Track",
			children: [
				{
					type: "item",
					name: "Mixer",
					change: "Modified",
					domain_type: "Mixer",
					children: [
						{
							type: "item",
							name: "Mixer",
							change: "Unchanged",
							domain_type: "Mixer",
							children: [],
						},
						{
							type: "item",
							name: "Tempo",
							change: "Modified",
							domain_type: "Mixer",
							children: tempoValue ? [tempoValue] : [],
						},
						{
							type: "item",
							name: "Time Signature",
							change: "Unchanged",
							domain_type: "Mixer",
							children: tsValue ? [tsValue] : [],
						},
					],
				} as any,
			],
		} as any,
	];
}

/** A regular track item with its own Mixer strip — in real diffs it comes
 *  BEFORE the master whenever only regular tracks changed. The decoy Tempo
 *  child (regular mixers don't really carry one) proves the master is matched
 *  positively by name, not as "the first track with a Mixer child". */
function regularTrackItem(tempoDecoy?: number): ViewNode {
	return {
		type: "item",
		name: "MidiTrack (#14): 1-Tela",
		change: "Modified",
		domain_type: "Track",
		children: [
			{
				type: "item",
				name: "Mixer",
				change: "Unchanged",
				domain_type: "Mixer",
				children:
					tempoDecoy !== undefined
						? [
								{
									type: "item",
									name: "Tempo",
									change: "Unchanged",
									domain_type: "Mixer",
									children: [field("Value", tempoDecoy, "Unchanged")],
								} as any,
							]
						: [],
			} as any,
		],
	} as any;
}

test.describe("tempo/time-signature extraction", () => {
	test("extracts tempo from the master's Mixer item", () => {
		const children = masterItem(
			field("Value", 138),
			field("Value", 201, "Unchanged"),
		);
		expect(extractTempo(children)).toBe(138);
	});

	test("extracts and decodes the time-signature code", () => {
		const children = masterItem(
			field("Value", 138),
			field("Value", 201, "Unchanged"),
		);
		// 201: numer = 201%99 + 1 = 4, denom = 1 << (201/99) = 4
		expect(extractTimeSignature(children)).toEqual({ numer: 4, denom: 4 });
	});

	test("decodes 6/8 (code 302)", () => {
		const children = masterItem(undefined, field("Value", 302, "Unchanged"));
		expect(extractTimeSignature(children)).toEqual({ numer: 6, denom: 8 });
	});

	test("falls back when values are absent", () => {
		const children = masterItem(undefined, undefined);
		expect(extractTempo(children)).toBe(120);
		expect(extractTimeSignature(children)).toEqual({ numer: 4, denom: 4 });
	});

	test("reads the master's tempo even when a regular track comes first", () => {
		// Under the old first-Mixer-child match this returned the decoy 999.
		const children = [
			regularTrackItem(999),
			...masterItem(field("Value", 138), field("Value", 201, "Unchanged")),
		];
		expect(extractTempo(children)).toBe(138);
		expect(extractTimeSignature(children)).toEqual({ numer: 4, denom: 4 });
	});

	test("falls back to 120 when no MainTrack item exists", () => {
		// Documents the remaining limitation: under Summary/Compact presets the
		// backend level-drops the (Unchanged) master item, so no master is found.
		const children = [regularTrackItem(999)];
		expect(extractTempo(children)).toBe(120);
		expect(extractTimeSignature(children)).toEqual({ numer: 4, denom: 4 });
	});
});
