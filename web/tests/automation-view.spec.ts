import { test, expect } from "@playwright/test";

const FILE_A = "../Thick Air v2.als";
const FILE_B = "../Thick Air v6.als";

async function loadDiff(page: import("@playwright/test").Page) {
	await page.goto("/", { waitUntil: "networkidle" });
	await page.setInputFiles('[data-testid="file-input-a"]', FILE_A);
	await page.setInputFiles('[data-testid="file-input-b"]', FILE_B);
	await page.click('[data-testid="compare-btn"]');
	await page.waitForSelector('[data-testid="compare-btn"]:not(:disabled)', {
		timeout: 120_000,
	});
	// Let canvases settle.
	await page.waitForTimeout(1500);
}

async function openTrackAutomation(
	page: import("@playwright/test").Page,
	trackText: RegExp,
) {
	const header = page
		.locator('[data-testid="track-header"]')
		.filter({ hasText: trackText })
		.first();
	await header.click();
	const tab = page.locator('[data-testid="detail-tab-automation"]');
	await expect(tab).toBeVisible();
	await tab.click();
	await page.waitForTimeout(400);
}

/** Count canvas pixels of a given change color (sampled every 2px). */
async function countColorPixels(
	page: import("@playwright/test").Page,
	cssVar: string,
): Promise<number> {
	return page.evaluate((varName) => {
		const canvas = document.querySelector(
			'[data-testid="automation-canvas"]',
		) as HTMLCanvasElement | null;
		if (!canvas) return -1;
		const ctx = canvas.getContext("2d");
		if (!ctx || canvas.width === 0) return 0;
		const hex = getComputedStyle(document.documentElement)
			.getPropertyValue(varName)
			.trim();
		if (!hex.startsWith("#")) return -1;
		const s = hex.slice(1);
		const rgb =
			s.length === 3
				? [
						parseInt(s[0] + s[0], 16),
						parseInt(s[1] + s[1], 16),
						parseInt(s[2] + s[2], 16),
					]
				: [
						parseInt(s.slice(0, 2), 16),
						parseInt(s.slice(2, 4), 16),
						parseInt(s.slice(4, 6), 16),
					];
		const img = ctx.getImageData(0, 0, canvas.width, canvas.height).data;
		let count = 0;
		for (let y = 0; y < canvas.height; y += 2) {
			for (let x = 0; x < canvas.width; x += 2) {
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
	}, cssVar);
}

async function automationCanvasWidth(
	page: import("@playwright/test").Page,
): Promise<number> {
	return page.evaluate(() => {
		const canvas = document.querySelector(
			'[data-testid="automation-canvas"]',
		) as HTMLCanvasElement | null;
		return canvas ? parseFloat(canvas.style.width) : -1;
	});
}

/** Width of the scroll container the canvas lives in (visible width). */
async function automationViewportWidth(
	page: import("@playwright/test").Page,
): Promise<number> {
	return page.evaluate(() => {
		const canvas = document.querySelector(
			'[data-testid="automation-canvas"]',
		) as HTMLCanvasElement | null;
		return canvas ? canvas.parentElement!.clientWidth : -1;
	});
}

test.describe("automation view", () => {
	test("renders removed events and clamps Ableton sentinel times", async ({
		page,
	}) => {
		const pageErrors: string[] = [];
		page.on("pageerror", (e) => pageErrors.push(e.message));

		await loadDiff(page);

		// --- MainTrack: Main: Tempo automation ---
		// Both events (Removed at -63072000, Added at -63072000) must be parsed
		// and rendered at time 0, not silently dropped or pushed off-canvas.
		await openTrackAutomation(page, /MasterMain/);
		const selector = page.locator(".automation-selector");
		await expect(selector).toHaveValue("0");
		await expect(page.locator(".automation-controls span").last()).toHaveText(
			"2 events",
		);
		// Canvas must be non-blank and contain both a removed and an added marker.
		expect(await automationCanvasWidth(page)).toBeGreaterThan(0);
		expect(await countColorPixels(page, "--color-removed")).toBeGreaterThan(0);
		expect(await countColorPixels(page, "--color-added")).toBeGreaterThan(0);

		// --- StereoWidth: sentinel first event + curve events ---
		await selector.selectOption("1");
		await expect(page.locator(".automation-controls span").last()).toHaveText(
			"4 events",
		);
		// Adaptive default zoom: the full curve must fit the visible width.
		const viewportW = await automationViewportWidth(page);
		expect(await automationCanvasWidth(page)).toBeGreaterThan(viewportW - 2);
		expect(await automationCanvasWidth(page)).toBeLessThan(viewportW + 2);
		expect(await countColorPixels(page, "--color-added")).toBeGreaterThan(0);

		// --- 6-Bowed Guitar: Volume automation (real multi-point changes) ---
		await openTrackAutomation(page, /6-Bowed Guitar/);
		await expect(page.locator(".automation-controls span").last()).toHaveText(
			"9 events",
		);
		// Removed events must draw markers + dashed ghost path.
		expect(await countColorPixels(page, "--color-removed")).toBeGreaterThan(0);
		expect(await countColorPixels(page, "--color-added")).toBeGreaterThan(0);

		// Macro controls with heavy modifications parse fully.
		await selector.selectOption("1");
		await expect(page.locator(".automation-controls span").last()).toHaveText(
			"129 events",
		);
		await selector.selectOption("2");
		await expect(page.locator(".automation-controls span").last()).toHaveText(
			"18 events",
		);
		await selector.selectOption("3");
		await expect(page.locator(".automation-controls span").last()).toHaveText(
			"110 events",
		);
		await selector.selectOption("4");
		await expect(page.locator(".automation-controls span").last()).toHaveText(
			"42 events",
		);
		await selector.selectOption("5");
		await expect(page.locator(".automation-controls span").last()).toHaveText(
			"38 events",
		);

		// --- 29-Dark Forces: M4L Draw X (460 events, ~442 beats) ---
		// Needs a fit zoom below the old 0.2 floor to fit the visible width.
		await openTrackAutomation(page, /29-Dark Forces/);
		await selector.selectOption("1");
		await expect(page.locator(".automation-controls span").last()).toHaveText(
			"460 events",
		);
		const viewportW2 = await automationViewportWidth(page);
		expect(await automationCanvasWidth(page)).toBeGreaterThan(viewportW2 - 2);
		expect(await automationCanvasWidth(page)).toBeLessThan(viewportW2 + 2);

		expect(pageErrors).toEqual([]);
	});
});
