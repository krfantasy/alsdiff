import { test, expect } from "@playwright/test";

const TRACK_HEIGHT = 64;

test.describe("piano roll note data", () => {
	test("Modified notes render at their real pitch (E3, not default C4)", async ({
		page,
	}) => {
		const pageErrors: string[] = [];
		page.on("pageerror", (e) => pageErrors.push(e.message));

		await page.goto("/", { waitUntil: "networkidle" });
		await page.setInputFiles('[data-testid="file-input-a"]', "../Thick Air v2.als");
		await page.setInputFiles('[data-testid="file-input-b"]', "../Thick Air v6.als");
		await page.click('[data-testid="compare-btn"]');
		await page.waitForSelector('[data-testid="compare-btn"]:not(:disabled)', {
			timeout: 120_000,
		});
		await page.waitForTimeout(1500);

		// Row order in the canvas matches the header order (visibleNodes).
		const texts = await page
			.locator('[data-testid="track-header"]')
			.allInnerTexts();
		const rowIdx = texts.findIndex((t) => t.includes("Galaxy Voices"));
		expect(rowIdx).toBeGreaterThanOrEqual(0);

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

		const canvasBox = await page
			.locator('[data-testid="arrangement-canvas"]')
			.boundingBox();
		expect(canvasBox).not.toBeNull();
		const rowY = canvasBox!.y + rowIdx * TRACK_HEIGHT + TRACK_HEIGHT / 2;

		// Click along the track's row until a clip with MIDI notes is selected —
		// selectClip auto-opens the Piano Roll tab for note-bearing clips.
		let pianoRollFound = false;
		for (let dx = 40; dx < canvasBox!.width - 20 && dx < 2000; dx += 80) {
			await page.mouse.click(canvasBox!.x + dx, rowY);
			await page.waitForTimeout(150);
			if (await page.locator('[data-testid="piano-roll-canvas"]').count()) {
				pianoRollFound = true;
				break;
			}
		}
		expect(pianoRollFound, "no note-bearing clip found on the track row").toBe(true);

		// The piano keyboard reflects the parsed pitch range: the Modified note
		// is E3 (52) and an Added note is B3 (59) — not the C4 default collapse.
		await page.waitForTimeout(400);
		const keyRows = await page.locator(".piano-key-row").allInnerTexts();
		expect(keyRows.length).toBeGreaterThan(4);
		expect(keyRows).toContain("E3");
		expect(keyRows).toContain("B3");

		expect(pageErrors).toEqual([]);
	});
});
