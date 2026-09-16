import { test, expect } from "@playwright/test";
import { computeVisibleSlice } from "../src/lib/canvas-utils";

// 65180cc: the visible-slice clamp shared by the arrangement, piano-roll, and
// automation renderers — overscroll must clamp so clearRect never gets a
// negative origin or width, and the slice must stop at the content end.
test.describe("computeVisibleSlice", () => {
	test("normal slice passes through", () => {
		expect(computeVisibleSlice({ scrollLeft: 100, visibleWidth: 500 }, 2000)).toEqual({
			vx: 100,
			vw: 500,
		});
	});

	test("negative overscroll clamps the origin to zero, keeps full width", () => {
		// Rubber-banding / momentum can report scrollLeft < 0; a negative
		// clearRect origin is undefined behavior on some browsers.
		expect(computeVisibleSlice({ scrollLeft: -50, visibleWidth: 500 }, 2000)).toEqual({
			vx: 0,
			vw: 500,
		});
	});

	test("slice stops at the content end", () => {
		expect(computeVisibleSlice({ scrollLeft: 1800, visibleWidth: 500 }, 2000)).toEqual({
			vx: 1800,
			vw: 200,
		});
	});

	test("scrolled past the end yields an empty slice, not negative width", () => {
		expect(computeVisibleSlice({ scrollLeft: 2500, visibleWidth: 500 }, 2000)).toEqual({
			vx: 2500,
			vw: 0,
		});
	});

	test("narrower content than the viewport caps the width", () => {
		expect(computeVisibleSlice({ scrollLeft: 0, visibleWidth: 500 }, 300)).toEqual({
			vx: 0,
			vw: 300,
		});
	});
});
