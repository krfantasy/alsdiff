import { test, expect } from "@playwright/test";

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
});
