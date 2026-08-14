import { test, expect } from "@playwright/test";

test.describe("master header", () => {
	test("no empty mixer strip; long names not truncated", async ({ page }) => {
		const pageErrors: string[] = [];
		page.on("pageerror", (e) => pageErrors.push(e.message));

		await page.goto("/", { waitUntil: "networkidle" });
		await page.setInputFiles('[data-testid="file-input-a"]', "../Middle v2.als");
		await page.setInputFiles('[data-testid="file-input-b"]', "../Middle v3.als");
		await page.click('[data-testid="compare-btn"]');
		await page.waitForSelector('[data-testid="compare-btn"]:not(:disabled)', { timeout: 120_000 });
		await page.waitForTimeout(1500);

		// Master row: its Mixer item has Tempo/TS/Crossfade/Groove — no
		// Volume/Pan/Mute/Solo — so no (empty) strip may render.
		const master = page
			.locator('[data-testid="track-header"]')
			.filter({ hasText: /MasterMain/ })
			.first();
		await expect(master).toBeVisible();
		await expect(master.locator('[data-testid="mixer-strip"]')).toHaveCount(0);

		// Regular tracks keep their strips (spot check: a row with a strip).
		expect(
			await page.locator('[data-testid="track-header"] [data-testid="mixer-strip"]').count(),
		).toBeGreaterThan(0);

		// Long name fits: not ellipsized.
		const longName = page
			.locator('[data-testid="track-header"] .track-name')
			.filter({ hasText: /Industrial FM Kick/ })
			.first();
		await expect(longName).toBeVisible();
		const fits = await longName.evaluate(
			(el) => el.scrollWidth <= el.clientWidth + 1,
		);
		expect(fits, "track name still ellipsized").toBe(true);

		expect(pageErrors).toEqual([]);
	});
});
