import { test, expect } from "@playwright/test";

async function loadDiff(
	page: import("@playwright/test").Page,
	fileA: string,
	fileB: string,
) {
	await page.goto("/", { waitUntil: "networkidle" });
	await page.setInputFiles('[data-testid="file-input-a"]', fileA);
	await page.setInputFiles('[data-testid="file-input-b"]', fileB);
	await page.click('[data-testid="compare-btn"]');
	await page.waitForSelector('[data-testid="compare-btn"]:not(:disabled)', {
		timeout: 120_000,
	});
	// Let the track list settle.
	await page.waitForTimeout(1500);
}

test.describe("mixer strip toggles", () => {
	test("S toggle is lit only for tracks with Solo=true", async ({ page }) => {
		const pageErrors: string[] = [];
		page.on("pageerror", (e) => pageErrors.push(e.message));

		// Middle pair: exactly one track (22-Soprano Sax (C3-C5)) has Solo=true.
		await loadDiff(page, "../Middle v2.als", "../Middle v3.als");
		const activeSolo = page.locator(
			'[data-testid="track-header"] [data-testid="mixer-toggle-S"].active',
		);
		await expect(activeSolo).toHaveCount(1);
		const soloedHeader = page
			.locator('[data-testid="track-header"]')
			.filter({
				has: page.locator('[data-testid="mixer-toggle-S"].active'),
			});
		await expect(soloedHeader).toContainText("Soprano Sax");

		// Every other rendered S toggle must be unlit (Solo=false renders dark).
		const allSoloToggles = page.locator(
			'[data-testid="track-header"] [data-testid="mixer-toggle-S"]',
		);
		expect(await allSoloToggles.count()).toBeGreaterThan(1);

		// Thick Air pair: no track has Solo=true.
		await loadDiff(page, "../Thick Air v2.als", "../Thick Air v6.als");
		await expect(activeSolo).toHaveCount(0);

		expect(pageErrors).toEqual([]);
	});
});
