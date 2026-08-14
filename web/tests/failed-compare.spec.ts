import { test, expect } from "@playwright/test";

async function compare(
	page: import("@playwright/test").Page,
	a: string,
	b: string,
) {
	await page.setInputFiles('[data-testid="file-input-a"]', a);
	await page.setInputFiles('[data-testid="file-input-b"]', b);
	await page.click('[data-testid="compare-btn"]');
	await page.waitForSelector('[data-testid="compare-btn"]:not(:disabled)', {
		timeout: 120_000,
	});
	await page.waitForTimeout(1000);
}

test.describe("failed re-comparison clears the previous one", () => {
	test("error shown and previous comparison gone when file B is corrupt", async ({
		page,
	}) => {
		const pageErrors: string[] = [];
		page.on("pageerror", (e) => pageErrors.push(e.message));

		await page.goto("/", { waitUntil: "networkidle" });

		// First: a successful comparison renders tracks.
		await compare(page, "../Middle v2.als", "../Middle v3.als");
		const headers = page.locator('[data-testid="track-header"]');
		await expect(headers.first()).toBeVisible();
		expect(await headers.count()).toBeGreaterThan(0);

		// Second: re-compare with a corrupt file B (not gzip, not XML).
		// The worker fails the diff, the error message shows, and the
		// previous pair must NOT linger next to it.
		await compare(page, "../Middle v2.als", "../test/data/not_an_als.als");

		const errorMsg = page.locator('[data-testid="error-msg"]');
		await expect(errorMsg).toBeVisible();
		// Failure path (verified live): the worker's OCaml diff raises
		// File_error("not_an_als.als", "...Unknown decompression error"),
		// posted as {type:"error"} which rejects diffFilesJson and lands
		// in handleCompare's catch — a parse error, not a worker crash.

		// Stale tracks gone; the app-level empty state is back because
		// diffResult was cleared (ArrangementView unmounted).
		await expect(headers).toHaveCount(0);
		await expect(page.locator("body")).toContainText(
			"Upload two .als files to compare",
		);

		expect(pageErrors).toEqual([]);
	});
});
