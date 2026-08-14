import { test, expect } from "@playwright/test";

const TRACK_HEIGHT = 64;

async function compare(page: import("@playwright/test").Page, a: string, b: string) {
	await page.setInputFiles('[data-testid="file-input-a"]', a);
	await page.setInputFiles('[data-testid="file-input-b"]', b);
	await page.click('[data-testid="compare-btn"]');
	await page.waitForSelector('[data-testid="compare-btn"]:not(:disabled)', { timeout: 120_000 });
	await page.waitForTimeout(1500);
}

/** Click along a track's row until a note-bearing clip is selected —
 *  selectClip auto-opens the Piano Roll tab (mirrors piano-roll-notes.spec.ts).
 *  Returns true when a piano roll is showing. */
async function openPianoRoll(
	page: import("@playwright/test").Page,
	rowText: string,
): Promise<boolean> {
	const texts = await page.locator('[data-testid="track-header"]').allInnerTexts();
	const rowIdx = texts.findIndex((t) => t.includes(rowText));
	expect(rowIdx, `no track header matching ${rowText}`).toBeGreaterThanOrEqual(0);
	// Scroll the timeline pane so the row is on screen (the boundingBox
	// below already reflects the scroll position).
	await page.evaluate(
		({ top }) => {
			const area = document.querySelector(".timeline-area");
			if (area) area.scrollTop = Math.max(0, top);
		},
		{ top: rowIdx * TRACK_HEIGHT - 120 },
	);
	await page.waitForTimeout(300);
	const box = await page.locator('[data-testid="arrangement-canvas"]').boundingBox();
	expect(box).not.toBeNull();
	const rowY = box!.y + rowIdx * TRACK_HEIGHT + TRACK_HEIGHT / 2;
	for (let dx = 40; dx < box!.width - 20 && dx < 2000; dx += 80) {
		await page.mouse.click(box!.x + dx, rowY);
		await page.waitForTimeout(150);
		if (await page.locator('[data-testid="piano-roll-canvas"]').count()) {
			return true;
		}
	}
	return false;
}

test.describe("state reset on new comparison", () => {
	test("selection, lane, collapse and piano zoom reset when re-comparing", async ({ page }) => {
		const pageErrors: string[] = [];
		page.on("pageerror", (e) => pageErrors.push(e.message));

		await page.goto("/", { waitUntil: "networkidle" });

		// First pair: dirty up the state.
		await compare(page, "../Thick Air v2.als", "../Thick Air v6.als");
		// Select the master. Neither master auto-opens its automation tab
		// (verified live: Thick Air's master defaults to Devices, Middle's
		// master has a single lane), so reveal the master's automation tab
		// explicitly — Thick Air's master has 2 lanes — and move the
		// selector off 0.
		await page.locator('[data-testid="track-header"]').filter({ hasText: /MasterMain/ }).first().click();
		await page.waitForTimeout(300);
		const selector = page.locator(".automation-selector");
		await page.locator('[data-testid="detail-tab-automation"]').click();
		await page.waitForTimeout(200);
		if (await selector.count()) {
			await selector.selectOption("1");
		}
		// Collapse the Bass group.
		await page.locator('[data-testid="group-toggle"]').first().click();
		await page.waitForTimeout(200);

		// Second pair: same session, different files.
		await compare(page, "../Middle v2.als", "../Middle v3.als");

		// Selection reset: no selected row, detail pane shows the fallback.
		await expect(page.locator('[data-testid="track-header"].selected')).toHaveCount(0);
		await expect(page.locator('[data-testid="detail-pane"]')).toContainText(
			"Select a track to view details",
		);
		// Collapse reset: Middle's "15 L" group is expanded (its members at depth 1).
		const member = page
			.locator('[data-testid="track-header"]')
			.filter({ hasText: /16 L Gem/ })
			.first();
		await expect(member).toBeVisible();

		// Automation lane reset: selecting the master again starts at lane 0.
		await page.locator('[data-testid="track-header"]').filter({ hasText: /MasterMain/ }).first().click();
		await page.waitForTimeout(400);
		await expect(page.locator(".automation-selector")).toHaveValue("0");

		// Piano-roll zoom reset: move the slider, re-compare, zoom label shows 1.0x.
		// Soprano Sax is device-bearing (lands on Devices, no auto piano roll)
		// and exposes no note-bearing clip on its row, so use the first Middle
		// track whose clip click opens the piano roll (verified live: Tela).
		const opened = await openPianoRoll(page, "Tela");
		expect(opened, "no note-bearing clip found on the track row").toBe(true);
		await page.waitForTimeout(200);
		const pianoSlider = page.locator('[data-testid="piano-zoom-slider"]');
		if (await pianoSlider.count()) {
			await pianoSlider.fill("90");
			await pianoSlider.dispatchEvent("input");
			await page.waitForTimeout(200);
		}
		await compare(page, "../Middle v2.als", "../Middle v3.als");
		await expect(
			page.locator('[data-testid="piano-zoom-label"]'),
		).toHaveCount(0); // pane closed after reset — no stale zoom UI
		// Re-open the piano roll and confirm the zoom reset to 1.0x.
		const reopened = await openPianoRoll(page, "Tela");
		expect(reopened, "no note-bearing clip found on the track row").toBe(true);
		await page.waitForTimeout(400);
		const label = page.locator('[data-testid="piano-zoom-label"]');
		if (await label.count()) {
			await expect(label).toHaveText("1.0x");
		}

		expect(pageErrors).toEqual([]);
	});
});
