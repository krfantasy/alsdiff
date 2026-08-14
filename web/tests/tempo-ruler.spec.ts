import { test, expect } from "@playwright/test";

test.describe("realtime ruler tempo", () => {
	test("ruler labels reflect the project tempo (Middle: 138)", async ({
		page,
	}) => {
		const pageErrors: string[] = [];
		page.on("pageerror", (e) => pageErrors.push(e.message));

		await page.goto("/", { waitUntil: "networkidle" });
		await page.setInputFiles(
			'[data-testid="file-input-a"]',
			"../Middle v2.als",
		);
		await page.setInputFiles(
			'[data-testid="file-input-b"]',
			"../Middle v3.als",
		);
		await page.click('[data-testid="compare-btn"]');
		await page.waitForSelector('[data-testid="compare-btn"]:not(:disabled)', {
			timeout: 120_000,
		});
		await page.waitForTimeout(1500);

		const labels = await page
			.locator('[data-testid="timeline-ruler-bottom"] > div')
			.evaluateAll((els) =>
				els
					.map((el) => (el as HTMLElement).innerText.trim())
					.filter((t) => t !== ""),
			);
		// Major markers only, labeled m:ss[.t]; need a long-enough span for the
		// 0.1s label quantization to average out.
		expect(labels.length).toBeGreaterThanOrEqual(3);

		const toSeconds = (t: string) => {
			const m = t.match(/^(\d+):(\d{2})(?:\.(\d+))?$/);
			if (!m) throw new Error(`unparseable label: ${t}`);
			return (
				Number(m[1]) * 60 + Number(m[2]) + (m[3] ? Number(`0.${m[3]}`) : 0)
			);
		};

		// Adjacent major markers are one major grid interval apart (minor*2
		// beats, minor from GRID_INTERVALS = 0.5..512). Recover the implied BPM
		// from the total span and require consistency with the real tempo 138
		// for some interval the grid can emit. The 120 fallback cannot satisfy
		// this: interval ratios are powers of two, and no power-of-two ratio
		// maps 120 to 138.
		const n = labels.length;
		const dt = toSeconds(labels[n - 1]) - toSeconds(labels[0]);
		expect(dt).toBeGreaterThan(10);
		const majors = [1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024];
		const consistent = majors.some(
			(beats) => Math.abs((beats * (n - 1) * 60) / dt - 138) < 2,
		);
		expect(
			consistent,
			`span ${dt}s over ${n} labels not consistent with 138 BPM`,
		).toBe(true);

		expect(pageErrors).toEqual([]);
	});
});
