import { test, expect, type Page } from "@playwright/test";
import {
	sumCounts,
	extractTracks,
	firstAvailableDetailTab,
} from "../src/lib/diff-parser";
import type { CollectionView, TrackData, ViewNode } from "../src/types";

test.describe("counts-only surfaces", () => {
	test("sumCounts sums a breakdown", () => {
		expect(sumCounts({ added: 1, removed: 2, modified: 3 })).toBe(6);
		expect(sumCounts(null)).toBe(0);
	});

	test("extractTracks carries item-level counts onto TrackData", () => {
		const livesetChildren: ViewNode[] = [
			{
				type: "item",
				name: "AudioTrack (#5): 5-Audio",
				change: "Removed",
				domain_type: "Track",
				counts: { added: 0, removed: 4, modified: 1 },
				children: [
					{
						type: "field",
						name: "TrackId",
						change: "Removed",
						domain_type: "Track",
						old_value: 5,
					},
				],
			} as any,
		];
		const tracks = extractTracks(livesetChildren);
		expect(tracks).toHaveLength(1);
		expect(tracks[0].counts).toEqual({ added: 0, removed: 4, modified: 1 });
	});

	test("firstAvailableDetailTab treats counts-only Clips as clip content", () => {
		// Counts-only collections as emitted under Compact (verified live on
		// Thick Air v2 vs v6, e.g. "MidiTrack (#16): 6-Bowed Guitar").
		const countsOnlyClips: CollectionView = {
			type: "collection",
			name: "Clips",
			change: "Modified",
			domain_type: "Track",
			counts: { added: 1, removed: 1, modified: 2 },
		};
		const countsOnlyAutomations: CollectionView = {
			type: "collection",
			name: "Automations",
			change: "Modified",
			domain_type: "Track",
			counts: { added: 0, removed: 0, modified: 6 },
		};
		const track: TrackData = {
			name: "MidiTrack (#16): 6-Bowed Guitar",
			change: "Modified",
			domainType: "Track",
			trackId: 16,
			groupId: -1,
			children: [countsOnlyClips, countsOnlyAutomations],
		};
		const first = firstAvailableDetailTab(track);
		// Counts-only clips select the clip tab WITHOUT a clipName (nothing is
		// selectable); DetailView shows the Clips counts tab in that state.
		// Clip content also outranks Automations in the tab order.
		expect(first).toEqual({ tab: "clip" });
		expect(first ? "clipName" in first : true).toBe(false);
	});
});

test.describe("compact preset surfacing (browser)", () => {
	test("counts-only mixer, tab counts, and track counts are visible", async ({
		page,
	}) => {
		const pageErrors: string[] = [];
		page.on("pageerror", (e) => pageErrors.push(e.message));
		await page.addInitScript(() =>
			localStorage.setItem(
				"alsdiff-settings",
				JSON.stringify({
					preset: "compact",
					customConfig: null,
					customConfigName: null,
				}),
			),
		);

		await page.goto("/", { waitUntil: "networkidle" });
		await page.setInputFiles(
			'[data-testid="file-input-a"]',
			"../Thick Air v2.als",
		);
		await page.setInputFiles(
			'[data-testid="file-input-b"]',
			"../Thick Air v6.als",
		);
		await page.click('[data-testid="compare-btn"]');
		await page.waitForSelector('[data-testid="compare-btn"]:not(:disabled)', {
			timeout: 120_000,
		});
		await page.waitForTimeout(1500);

		// 1. Modified track with a counts-only Mixer change shows a banner
		//    instead of rendering nothing.
		const kaivo = page
			.locator('[data-testid="track-header"]')
			.filter({ hasText: /Kaivo/ })
			.first();
		await expect(
			kaivo.locator('[data-testid="mixer-counts-banner"]'),
		).toBeVisible();

		// 2. Tab labels show the counts total, not "(0)".
		await page
			.locator('[data-testid="track-header"]')
			.filter({ hasText: /Main/ })
			.first()
			.click();
		await page.waitForTimeout(300);
		const autoTab = page.locator('[data-testid="detail-tab-automation"]');
		await expect(autoTab).toBeVisible();
		await expect(autoTab).toContainText(/Automation \([1-9]\d*\)/);

		// 3. A summary-level removed track surfaces its track counts.
		await page
			.locator('[data-testid="track-header"]')
			.filter({ hasText: /5-Audio/ })
			.first()
			.click();
		await page.waitForTimeout(300);
		const banner = page.locator('[data-testid="counts-banner"]');
		await expect(banner).toBeVisible();
		await expect(banner).toContainText("Track:");
		await expect(banner).toContainText("removed");

		expect(pageErrors).toEqual([]);
	});

	test("counts-only Clips surface as a Clips tab with the counts banner", async ({
		page,
	}) => {
		const pageErrors: string[] = [];
		page.on("pageerror", (e) => pageErrors.push(e.message));
		await page.addInitScript(() =>
			localStorage.setItem(
				"alsdiff-settings",
				JSON.stringify({
					preset: "compact",
					customConfig: null,
					customConfigName: null,
				}),
			),
		);

		await page.goto("/", { waitUntil: "networkidle" });
		await page.setInputFiles(
			'[data-testid="file-input-a"]',
			"../Thick Air v2.als",
		);
		await page.setInputFiles(
			'[data-testid="file-input-b"]',
			"../Thick Air v6.als",
		);
		await page.click('[data-testid="compare-btn"]');
		await page.waitForSelector('[data-testid="compare-btn"]:not(:disabled)', {
			timeout: 120_000,
		});
		await page.waitForTimeout(1500);

		// A Modified track whose Clips collection is counts-only (Compact) gets
		// a Clips tab; clip content outranks the track's counts-only
		// Automations in the devices → clips → automation order, so selecting
		// the track lands on the Clips tab.
		await page
			.locator('[data-testid="track-header"]')
			.filter({ hasText: /6-Bowed Guitar/ })
			.first()
			.click();
		await page.waitForTimeout(300);
		const clipsTab = page.locator('[data-testid="detail-tab-clips"]');
		await expect(clipsTab).toBeVisible();
		await expect(clipsTab).toContainText(/Clips \([1-9]\d*\)/);
		await expect(clipsTab).toHaveClass(/active/);
		let banner = page.locator('[data-testid="counts-banner"]');
		await expect(banner).toBeVisible();
		await expect(banner).toContainText("Clips:");

		// Clicking the tab shows the banner again after switching away.
		await page.click('[data-testid="detail-tab-automation"]');
		await page.click('[data-testid="detail-tab-clips"]');
		await expect(banner).toBeVisible();
		await expect(banner).toContainText("Clips:");

		// A track with ONLY counts-only Clips (no devices/automations) also
		// gets the tab — previously its detail pane was entirely empty.
		await page
			.locator('[data-testid="track-header"]')
			.filter({ hasText: /23-Metal Scrapes Texture 01/ })
			.first()
			.click();
		await page.waitForTimeout(300);
		await expect(clipsTab).toBeVisible();
		await expect(clipsTab).toContainText(/Clips \([1-9]\d*\)/);
		banner = page.locator('[data-testid="counts-banner"]');
		await expect(banner).toBeVisible();
		await expect(banner).toContainText("Clips:");

		expect(pageErrors).toEqual([]);
	});
});

// Nested counts-only collections the way Summary/Compact detail levels emit
// them: `counts` and no `items`. Verified live via
// `alsdiff --mode json --preset compact` on Thick Air v2/v6: under Compact
// every counts-only collection sits at track level (Devices/Clips/Automations
// are Summary), so device cards never materialize and no live fixture pair
// reaches a nested counts-only collection — synthesize one with the
// stub-worker pattern from device-params.spec.ts instead.
function syntheticNestedCountsDiff() {
	return {
		diff: [
			{
				type: "item",
				name: "LiveSet: Wiggle v2",
				change: "Modified",
				domain_type: "Liveset",
				children: [
					{
						type: "item",
						name: "AudioTrack (#1): Wiggle Synth",
						change: "Modified",
						domain_type: "Track",
						children: [
							{
								type: "collection",
								name: "Devices",
								change: "Modified",
								domain_type: "Device",
								items: [
									{
										type: "item",
										name: "StereoGain (#6): StereoGain",
										change: "Modified",
										domain_type: "Device",
										children: [
											{
												type: "collection",
												name: "Parameters",
												change: "Modified",
												domain_type: "Param",
												counts: { added: 2, removed: 0, modified: 1 },
											},
										],
									},
								],
							},
						],
					},
					{
						type: "item",
						name: "MidiTrack (#2): Noteworthy",
						change: "Modified",
						domain_type: "Track",
						children: [
							{
								type: "collection",
								name: "Clips",
								change: "Modified",
								domain_type: "Clip",
								items: [
									{
										type: "item",
										name: "MidiClip (#3): Pluck",
										change: "Modified",
										domain_type: "Clip",
										children: [
											{
												type: "collection",
												name: "Notes",
												change: "Modified",
												domain_type: "Note",
												counts: { added: 4, removed: 0, modified: 2 },
											},
										],
									},
								],
							},
						],
					},
				],
			},
		],
	};
}

// Replaces the real diff worker with one that always answers with `diff`
// (same contract as device-params.spec.ts).
function stubWorkerWithDiff(page: Page, diff: object) {
	return page.route("**/alsdiff-worker.js", (route) =>
		route.fulfill({
			contentType: "application/javascript",
			body: `self.onmessage = function (e) {
				const msg = e.data;
				if (msg.type === "diff") {
					self.postMessage({
						type: "result",
						requestId: msg.requestId,
						result: ${JSON.stringify(JSON.stringify(diff))},
					});
				}
			};`,
		}),
	);
}

test.describe("nested counts-only collections (browser)", () => {
	test("counts-only nested Parameters and clip Notes render a counts banner", async ({
		page,
	}) => {
		const pageErrors: string[] = [];
		page.on("pageerror", (e) => pageErrors.push(e.message));

		await stubWorkerWithDiff(page, syntheticNestedCountsDiff());

		await page.goto("/", { waitUntil: "networkidle" });
		await page.setInputFiles('[data-testid="file-input-a"]', "../Thick Air v2.als");
		await page.setInputFiles('[data-testid="file-input-b"]', "../Thick Air v6.als");
		await page.click('[data-testid="compare-btn"]');
		await page.waitForSelector('[data-testid="compare-btn"]:not(:disabled)', {
			timeout: 120_000,
		});
		await page.waitForTimeout(1500);

		// 1. A device card whose Parameters collection is counts-only must show
		//    the counts banner inside the card, not a bare "Parameters" header
		//    (ViewNodeRow's collection branch).
		await page
			.locator('[data-testid="track-header"]')
			.filter({ hasText: /Wiggle Synth/ })
			.first()
			.click();
		await page.waitForTimeout(300);
		const devicesTab = page.locator('[data-testid="detail-tab-devices"]');
		await expect(devicesTab).toBeVisible();
		await devicesTab.click();
		await page.waitForTimeout(400);

		const card = page
			.locator('[data-testid="device-card"]')
			.filter({ hasText: /StereoGain/ })
			.first();
		await expect(card).toBeVisible();
		await card.locator("> .device-name").click(); // expand
		await page.waitForTimeout(300);

		const paramBanner = card.locator('[data-testid="counts-banner"]');
		await expect(paramBanner).toBeVisible();
		await expect(paramBanner).toContainText("Parameters:");
		await expect(paramBanner).toContainText("2 added, 1 modified");
		await expect(paramBanner).toContainText("switch to Verbose/Full");

		// 2. Same for a clip's counts-only Notes collection rendered through the
		//    default CollectionList export: selecting the track lands on the
		//    clip tab (no devices, listable Clips, no notes).
		await page
			.locator('[data-testid="track-header"]')
			.filter({ hasText: /Noteworthy/ })
			.first()
			.click();
		await page.waitForTimeout(300);
		const clipTab = page.locator('[data-testid="detail-tab-clip"]');
		await expect(clipTab).toBeVisible();
		await expect(clipTab).toContainText("Pluck");

		const notesBanner = page.locator(
			'[data-testid="clip-detail"] [data-testid="counts-banner"]',
		);
		await expect(notesBanner).toBeVisible();
		await expect(notesBanner).toContainText("Notes:");
		await expect(notesBanner).toContainText("4 added, 2 modified");
		await expect(notesBanner).toContainText("switch to Verbose/Full");

		expect(pageErrors).toEqual([]);
	});
});
