import { test, expect, type Page } from "@playwright/test";

// Builds the synthetic diff skeleton shared by the stub-worker tests below:
// one Modified device ("StereoGain") on one Modified audio track, whose
// Parameters collection contains exactly `paramItems`.
function syntheticDeviceDiff(paramItems: object[]) {
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
								type: "field",
								name: "TrackId",
								change: "Modified",
								domain_type: "Track",
								old_value: 1,
								new_value: 1,
							},
							{
								type: "field",
								name: "GroupId",
								change: "Modified",
								domain_type: "Track",
								old_value: -1,
								new_value: -1,
							},
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
												items: paramItems,
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

// Replaces the real diff worker with one that always answers with `diff`.
function stubWorkerWithDiff(page: Page, diff: object) {
	return page.route("**/alsdiff-worker.js", (route) =>
		route.fulfill({
			contentType: "application/javascript",
			// Mirror the real worker's message contract: the result is a
			// JSON *string* parsed by alsdiff-api.ts.
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

test.describe("device parameter rows", () => {
	test("Parameters collection renders as compact param rows", async ({ page }) => {
		const pageErrors: string[] = [];
		page.on("pageerror", (e) => pageErrors.push(e.message));

		await page.goto("/", { waitUntil: "networkidle" });
		await page.setInputFiles('[data-testid="file-input-a"]', "../Thick Air v2.als");
		await page.setInputFiles('[data-testid="file-input-b"]', "../Thick Air v6.als");
		await page.click('[data-testid="compare-btn"]');
		await page.waitForSelector('[data-testid="compare-btn"]:not(:disabled)', { timeout: 120_000 });
		await page.waitForTimeout(1500);

		// The master track hosts StereoGain with a modified "On" param (false -> true).
		await page
			.locator('[data-testid="track-header"]')
			.filter({ hasText: /MasterMain/ })
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

		const row = card.locator(".param-change", { hasText: "On" }).first();
		await expect(row).toBeVisible();
		await expect(row.locator(".old-value")).toHaveText("false");
		await expect(row.locator(".new-value")).toHaveText("true");

		expect(pageErrors).toEqual([]);
	});

	test("param modified only via automation stays visible when Value is unchanged", async ({ page }) => {
		const pageErrors: string[] = [];
		page.on("pageerror", (e) => pageErrors.push(e.message));

		// A GenericParam (lib/live/device.ml) can be Modified because its
		// automation/modulation target changed while Value stayed put. No
		// shipped fixture pair exhibits that shape (every Modified param in
		// the Middle/Thick Air pairs also changed Value), so stub the diff
		// worker with a synthetic result carrying exactly one such param.
		const syntheticDiff = syntheticDeviceDiff([
			{
				type: "item",
				name: "Cutoff",
				change: "Modified",
				domain_type: "Param",
				children: [
					{
						type: "field",
						name: "Value",
						change: "Unchanged",
						domain_type: "Param",
					},
					{
						type: "field",
						name: "Automation",
						change: "Modified",
						domain_type: "Param",
						old_value: 12,
						new_value: 34,
					},
				],
			},
		]);

		await stubWorkerWithDiff(page, syntheticDiff);

		await page.goto("/", { waitUntil: "networkidle" });
		await page.setInputFiles('[data-testid="file-input-a"]', "../Thick Air v2.als");
		await page.setInputFiles('[data-testid="file-input-b"]', "../Thick Air v6.als");
		await page.click('[data-testid="compare-btn"]');
		await page.waitForSelector('[data-testid="compare-btn"]:not(:disabled)', { timeout: 120_000 });
		await page.waitForTimeout(1500);

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

		// The param's Value did not move, so its only change is summarized
		// in a compact meta row ("Automation 12 → 34") — the param must stay
		// visible, not vanish because no Value row existed for it.
		const cutoff = card.locator(".param-change", { hasText: "Cutoff" }).first();
		await expect(cutoff).toBeVisible();
		await expect(cutoff).toContainText("Automation 12 → 34");

		expect(pageErrors).toEqual([]);
	});

	test("mixed Parameters collection keeps automation-only param beside Value-changed param", async ({ page }) => {
		const pageErrors: string[] = [];
		page.on("pageerror", (e) => pageErrors.push(e.message));

		// Regression (review I2): when one param in a Parameters collection
		// changed Value and a sibling only changed its automation target,
		// the Value row made the component drop the WHOLE collection from
		// the generic path — and with it the automation-only sibling, whose
		// change became invisible. Both params must stay visible.
		const syntheticDiff = syntheticDeviceDiff([
			{
				type: "item",
				name: "Volume",
				change: "Modified",
				domain_type: "Param",
				children: [
					{
						type: "field",
						name: "Value",
						change: "Modified",
						domain_type: "Param",
						old_value: 1,
						new_value: 2,
					},
					{
						type: "field",
						name: "Automation",
						change: "Unchanged",
						domain_type: "Param",
					},
				],
			},
			{
				type: "item",
				name: "Cutoff",
				change: "Modified",
				domain_type: "Param",
				children: [
					{
						type: "field",
						name: "Value",
						change: "Unchanged",
						domain_type: "Param",
					},
					{
						type: "field",
						name: "Automation",
						change: "Modified",
						domain_type: "Param",
						old_value: 12,
						new_value: 34,
					},
				],
			},
		]);

		await stubWorkerWithDiff(page, syntheticDiff);

		await page.goto("/", { waitUntil: "networkidle" });
		await page.setInputFiles('[data-testid="file-input-a"]', "../Thick Air v2.als");
		await page.setInputFiles('[data-testid="file-input-b"]', "../Thick Air v6.als");
		await page.click('[data-testid="compare-btn"]');
		await page.waitForSelector('[data-testid="compare-btn"]:not(:disabled)', { timeout: 120_000 });
		await page.waitForTimeout(1500);

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

		// The Value-changed param keeps its compact old -> new row...
		const volume = card.locator(".param-change", { hasText: "Volume" }).first();
		await expect(volume).toBeVisible();
		await expect(volume.locator(".old-value")).toHaveText("1");
		await expect(volume.locator(".new-value")).toHaveText("2");

		// ...and the automation-only sibling gets a compact meta row instead
		// of disappearing with the dropped collection.
		const cutoff = card.locator(".param-change", { hasText: "Cutoff" }).first();
		await expect(cutoff).toBeVisible();
		await expect(cutoff).toContainText("Automation 12 → 34");

		expect(pageErrors).toEqual([]);
	});
});
