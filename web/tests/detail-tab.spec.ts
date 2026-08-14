import { test, expect } from "@playwright/test";
import { firstAvailableDetailTab } from "../src/lib/diff-parser";
import type { TrackData, ViewNode } from "../src/types";

function track(sections: Record<string, ViewNode[]>): TrackData {
	return {
		name: "MidiTrack (#1): T",
		change: "Modified",
		domainType: "Track",
		trackId: 1,
		groupId: -1,
		children: Object.values(sections).flat(),
	} as any;
}

function devicesCol(items: ViewNode[] = []): ViewNode {
	return {
		type: "collection",
		name: "Devices",
		change: "Modified",
		domain_type: "Device",
		...(items.length
			? { items }
			: { counts: { added: 1, removed: 0, modified: 0 } }),
	} as any;
}
function automationsCol(items: ViewNode[] = []): ViewNode {
	return {
		type: "collection",
		name: "Automations",
		change: "Modified",
		domain_type: "Automation",
		...(items.length
			? { items }
			: { counts: { added: 2, removed: 0, modified: 0 } }),
	} as any;
}
function clipsCol(items: ViewNode[]): ViewNode {
	return {
		type: "collection",
		name: "Clips",
		change: "Modified",
		domain_type: "Clip",
		items,
	} as any;
}
function midiClip(name: string, withNotes: boolean): ViewNode {
	return {
		type: "item",
		name,
		change: "Modified",
		domain_type: "Clip",
		children: withNotes
			? [
					{
						type: "collection",
						name: "Notes",
						change: "Modified",
						domain_type: "Note",
						items: [
							{
								type: "item",
								name: "Note C4 (60)",
								change: "Added",
								domain_type: "Note",
								children: [
									{
										type: "field",
										name: "Note",
										change: "Added",
										domain_type: "Note",
										new_value: 60,
									},
								],
							},
						],
					},
				]
			: [],
	} as any;
}

test.describe("firstAvailableDetailTab", () => {
	test("prefers devices when present", () => {
		const t = track({ d: [devicesCol()], a: [automationsCol()] });
		expect(firstAvailableDetailTab(t)).toEqual({ tab: "devices" });
	});

	test("falls back to pianoRoll for the first note-bearing clip", () => {
		const t = track({
			c: [clipsCol([midiClip("MidiClip (#1)", true)])],
			a: [automationsCol()],
		});
		expect(firstAvailableDetailTab(t)).toEqual({
			tab: "pianoRoll",
			clipName: "MidiClip (#1)",
		});
	});

	test("falls back to the clip tab when no clip has notes", () => {
		const t = track({ c: [clipsCol([midiClip("MidiClip (#2)", false)])] });
		expect(firstAvailableDetailTab(t)).toEqual({
			tab: "clip",
			clipName: "MidiClip (#2)",
		});
	});

	test("falls back to automation (Master-like track, counts-only)", () => {
		const t = track({ a: [automationsCol()] });
		expect(firstAvailableDetailTab(t)).toEqual({ tab: "automation" });
	});

	test("counts-only devices still count as data", () => {
		const t = track({ d: [devicesCol()] });
		expect(firstAvailableDetailTab(t)).toEqual({ tab: "devices" });
	});

	test("null when nothing renderable", () => {
		expect(firstAvailableDetailTab(track({}))).toBeNull();
	});
});

test.describe("detail tab default (browser)", () => {
	test("Master opens on Automation; device tracks on Devices", async ({
		page,
	}) => {
		const pageErrors: string[] = [];
		page.on("pageerror", (e) => pageErrors.push(e.message));

		await page.goto("/", { waitUntil: "networkidle" });
		await page.setInputFiles(
			'[data-testid="file-input-a"]',
			"../Middle v2.als",
		);
		await page.setInputFiles(
			'[data-testid="file-input-b"]',
			"../Middle v3.als",
		);
		await page.click('[data-testid="compare-btn"]');
		await page.waitForSelector('[data-testid="compare-btn"]:not(:disabled)', {
			timeout: 120_000,
		});
		await page.waitForTimeout(1500);

		// The Master track has no Devices collection — the pane must open on
		// its Automation tab instead of an empty Devices pane.
		await page
			.locator('[data-testid="track-header"]')
			.filter({ hasText: /Main/ })
			.first()
			.click();
		await page.waitForTimeout(300);
		const autoTab = page.locator('[data-testid="detail-tab-automation"]');
		await expect(autoTab).toBeVisible();
		await expect(autoTab).toHaveClass(/active/);
		await expect(
			page.locator('[data-testid="detail-tab-devices"]'),
		).toHaveCount(0);

		// A regular track still defaults to Devices.
		await page
			.locator('[data-testid="track-header"]')
			.filter({ hasText: /Soprano Sax/ })
			.first()
			.click();
		await page.waitForTimeout(300);
		await expect(
			page.locator('[data-testid="detail-tab-devices"]'),
		).toHaveClass(/active/);

		expect(pageErrors).toEqual([]);
	});
});
