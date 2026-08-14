import { test, expect } from "@playwright/test";

async function loadDiff(page: import("@playwright/test").Page) {
	await page.goto("/", { waitUntil: "networkidle" });
	await page.setInputFiles('[data-testid="file-input-a"]', "../Thick Air v2.als");
	await page.setInputFiles('[data-testid="file-input-b"]', "../Thick Air v6.als");
	await page.click('[data-testid="compare-btn"]');
	await page.waitForSelector('[data-testid="compare-btn"]:not(:disabled)', {
		timeout: 120_000,
	});
	await page.waitForTimeout(1500);
}

/** padding-left encodes nesting depth: 10px + depth*20px (TrackHeader.tsx). */
async function headerPadding(
	page: import("@playwright/test").Page,
	text: RegExp,
	hasGroupToggle = false,
): Promise<number> {
	let header = page
		.locator('[data-testid="track-header"]')
		.filter({ hasText: text })
		.first();
	if (hasGroupToggle) {
		header = header.filter({
			has: page.locator('[data-testid="group-toggle"]'),
		});
	}
	await expect(header).toBeVisible();
	return header.evaluate((el) => parseFloat(getComputedStyle(el).paddingLeft));
}

test.describe("track grouping", () => {
	test("Modified group members nest under their group track", async ({ page }) => {
		const pageErrors: string[] = [];
		page.on("pageerror", (e) => pageErrors.push(e.message));

		await loadDiff(page);

		// Group (#91): Bass is the parent (depth 0). The header shows only the
		// label ("Bass"), so disambiguate group rows via the group-toggle.
		expect(await headerPadding(page, /Bass/, true)).toBe(10);

		// Modified members of group 91 (TrackGroupId=91 in both files) must be
		// nested at depth 1, not rendered top-level.
		expect(await headerPadding(page, /Dark Art Bass/)).toBe(30);
		expect(await headerPadding(page, /6-Bowed Guitar/)).toBe(30);

		expect(pageErrors).toEqual([]);
	});
});
