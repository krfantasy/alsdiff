import { test, expect } from "@playwright/test";
import { computeAutomationRange } from "../src/lib/automation-events";
import type { AutomationEvent } from "../src/types";

test.describe("computeAutomationRange left padding", () => {
	test("events at t=0 keep negative-time padding (no left clipping)", () => {
		const events: AutomationEvent[] = [
			{ time: 0, value: 125, change: "Added" },
			{ time: 0, value: 112, change: "Removed", oldValue: 112 },
		];
		const r = computeAutomationRange(events);
		expect(r.minTime).toBeLessThan(0);
		// The 25%-of-span sanity: min value pads symmetrically.
		expect(r.maxTime).toBeGreaterThan(0);
	});

	test("positive minima keep their pad (unchanged behavior)", () => {
		const events: AutomationEvent[] = [
			{ time: 10, value: 1, change: "Unchanged" },
			{ time: 20, value: 2, change: "Unchanged" },
		];
		const r = computeAutomationRange(events);
		// span 10 -> pad 1 -> minTime 9 (was Math.max(0, 9) = 9, same).
		expect(r.minTime).toBe(9);
	});
});

async function loadDiff(page: import("@playwright/test").Page) {
	await page.goto("/", { waitUntil: "networkidle" });
	await page.setInputFiles('[data-testid="file-input-a"]', "../Thick Air v2.als");
	await page.setInputFiles('[data-testid="file-input-b"]', "../Thick Air v6.als");
	await page.click('[data-testid="compare-btn"]');
	await page.waitForSelector('[data-testid="compare-btn"]:not(:disabled)', { timeout: 120_000 });
	await page.waitForTimeout(1500);
}

/** Count canvas pixels of a CSS-var color within columns [x0, x1). */
function countColorPixelsInColumns(page: import("@playwright/test").Page, cssVar: string, x0: number, x1: number) {
	return page.evaluate(([varName, cx0, cx1]) => {
		const canvas = document.querySelector('[data-testid="automation-canvas"]') as HTMLCanvasElement | null;
		if (!canvas) return -1;
		const ctx = canvas.getContext("2d");
		if (!ctx) return 0;
		const hex = getComputedStyle(document.documentElement).getPropertyValue(varName).trim();
		if (!hex.startsWith("#")) return -1;
		const s = hex.slice(1);
		const rgb = s.length === 3
			? [0, 1, 2].map((i) => parseInt(s[i] + s[i], 16))
			: [parseInt(s.slice(0, 2), 16), parseInt(s.slice(2, 4), 16), parseInt(s.slice(4, 6), 16)];
		const img = ctx.getImageData(0, 0, canvas.width, canvas.height).data;
		let count = 0;
		for (let y = 0; y < canvas.height; y += 2) {
			for (let x = cx0; x < Math.min(cx1, canvas.width); x += 2) {
				const i = (y * canvas.width + x) * 4;
				if (
					Math.abs(img[i] - rgb[0]) <= 12 &&
					Math.abs(img[i + 1] - rgb[1]) <= 12 &&
					Math.abs(img[i + 2] - rgb[2]) <= 12 &&
					img[i + 3] >= 40
				)
					count++;
			}
		}
		return count;
	}, [cssVar, x0, x1]);
}

test.describe("tempo markers not clipped (browser)", () => {
	test("t=0 tempo markers keep left padding", async ({ page }) => {
		const pageErrors: string[] = [];
		page.on("pageerror", (e) => pageErrors.push(e.message));

		await loadDiff(page);
		const header = page
			.locator('[data-testid="track-header"]')
			.filter({ hasText: /MasterMain/ })
			.first();
		await header.click();
		const tab = page.locator('[data-testid="detail-tab-automation"]');
		await tab.click();
		await page.waitForTimeout(600);

		// Markers must exist somewhere on the canvas…
		expect(await countColorPixelsInColumns(page, "--color-added", 0, 10000)).toBeGreaterThan(0);
		expect(await countColorPixelsInColumns(page, "--color-removed", 0, 10000)).toBeGreaterThan(0);
		// …but not within the first 2 columns (pre-fix: markers centered at x=0).
		expect(await countColorPixelsInColumns(page, "--color-added", 0, 2)).toBe(0);
		expect(await countColorPixelsInColumns(page, "--color-removed", 0, 2)).toBe(0);

		expect(pageErrors).toEqual([]);
	});
});
