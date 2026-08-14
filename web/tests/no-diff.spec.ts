import { test, expect } from "@playwright/test";
import { hasAnyChange } from "../src/lib/diff-parser";
import type { ViewNode } from "../src/types";

test.describe("hasAnyChange", () => {
	test("false for empty list", () => {
		expect(hasAnyChange([])).toBe(false);
	});

	test("false for a lone Unchanged LiveSet item (self-diff shape)", () => {
		const diff: ViewNode[] = [
			{
				type: "item",
				name: "LiveSet",
				change: "Unchanged",
				domain_type: "Liveset",
			} as any,
		];
		expect(hasAnyChange(diff)).toBe(false);
	});

	test("true when a nested child changed", () => {
		const diff: ViewNode[] = [
			{
				type: "item",
				name: "LiveSet",
				change: "Modified",
				domain_type: "Liveset",
				children: [
					{
						type: "field",
						name: "Name",
						change: "Unchanged",
						domain_type: "Liveset",
					},
					{
						type: "item",
						name: "MidiTrack (#1): A",
						change: "Unchanged",
						domain_type: "Track",
						children: [
							{
								type: "field",
								name: "TrackId",
								change: "Unchanged",
								domain_type: "Track",
								new_value: 1,
							},
							{
								type: "field",
								name: "GroupId",
								change: "Modified",
								domain_type: "Track",
								old_value: -1,
								new_value: 2,
							},
						],
					},
				],
			} as any,
		];
		expect(hasAnyChange(diff)).toBe(true);
	});
});

test.describe("no-diff message (browser)", () => {
	test("self-diff shows 'No differences found'", async ({ page }) => {
		const pageErrors: string[] = [];
		page.on("pageerror", (e) => pageErrors.push(e.message));

		await page.goto("/", { waitUntil: "networkidle" });
		await page.setInputFiles(
			'[data-testid="file-input-a"]',
			"../Middle v2.als",
		);
		await page.setInputFiles(
			'[data-testid="file-input-b"]',
			"../Middle v2.als",
		);
		await page.click('[data-testid="compare-btn"]');
		await page.waitForSelector('[data-testid="compare-btn"]:not(:disabled)', {
			timeout: 120_000,
		});
		await page.waitForTimeout(1000);
		await expect(page.locator('[data-testid="error-msg"]')).toHaveText(
			"No differences found between files.",
		);
		await expect(page.locator("body")).not.toContainText(
			"No track changes detected between files.",
		);

		expect(pageErrors).toEqual([]);
	});
});
