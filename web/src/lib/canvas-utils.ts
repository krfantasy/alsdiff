// Guarded so the module can be imported in Node (Playwright spec runner);
// in the browser this is exactly `window.devicePixelRatio || 1`.
const dpr = typeof window !== "undefined" ? window.devicePixelRatio || 1 : 1;

export function setupCanvas(
  canvas: HTMLCanvasElement,
  width: number,
  height: number,
): CanvasRenderingContext2D {
  canvas.width = width * dpr;
  canvas.height = height * dpr;
  canvas.style.width = `${width}px`;
  canvas.style.height = `${height}px`;
  const ctx = canvas.getContext("2d");
  if (!ctx) throw new Error("Failed to get 2D rendering context");
  ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
  return ctx;
}

const cssColorCache = new Map<string, string>();

export function getCSSColor(varName: string): string {
  const cached = cssColorCache.get(varName);
  if (cached) return cached;
  const value = getComputedStyle(document.documentElement)
    .getPropertyValue(varName)
    .trim();
  cssColorCache.set(varName, value);
  return value;
}

export function clearCSSColorCache(): void {
  cssColorCache.clear();
}

export function computeGridInterval(
  ppb: number,
  intervals: number[],
  minPx: number,
): number {
  let minor = intervals[intervals.length - 1];
  for (const iv of intervals) {
    if (iv * ppb >= minPx) {
      minor = iv;
      break;
    }
  }
  return minor;
}

export interface RulerMarker {
  pos: number;
  label: string;
  isMajor: boolean;
}

/** Decide which ruler labels to draw so they never overlap at low zoom.
 *  Returns one boolean per marker (same order); ticks are unaffected. A
 *  label is kept when it starts at least `gap` px after the previous kept
 *  label's right edge (label offset 3 + measured width). */
export function planRulerLabels(
  markers: RulerMarker[],
  textWidth: (label: string) => number,
  gap = 12,
): boolean[] {
  let lastKeptEnd = -Infinity;
  return markers.map((m) => {
    if (!m.label) return false;
    const start = m.pos + 3;
    if (start < lastKeptEnd + gap) return false;
    lastKeptEnd = start + textWidth(m.label);
    return true;
  });
}

export function drawRuler(
  ctx: CanvasRenderingContext2D,
  markers: RulerMarker[],
  width: number,
  height: number,
  alignBottom: boolean,
): void {
  ctx.fillStyle = getCSSColor("--bg-header");
  ctx.fillRect(0, 0, width, height);

  const borderColor = getCSSColor("--border");
  ctx.strokeStyle = borderColor;
  ctx.lineWidth = 1;
  if (alignBottom) {
    ctx.beginPath();
    ctx.moveTo(0, 0.5);
    ctx.lineTo(width, 0.5);
    ctx.stroke();
  } else {
    ctx.beginPath();
    ctx.moveTo(0, height - 0.5);
    ctx.lineTo(width, height - 0.5);
    ctx.stroke();
  }

  const textDim = getCSSColor("--text-dim");
  const borderLight = getCSSColor("--border-light");

  ctx.font = "10px -apple-system, BlinkMacSystemFont, sans-serif";
  const drawLabel = planRulerLabels(markers, (s) => ctx.measureText(s).width);
  markers.forEach((m, i) => {
    const x = Math.round(m.pos) + 0.5;
    ctx.strokeStyle = m.isMajor ? borderLight : borderColor;
    ctx.lineWidth = m.isMajor ? 1.5 : 1;
    ctx.beginPath();
    ctx.moveTo(x, 0);
    ctx.lineTo(x, height);
    ctx.stroke();

    if (m.label && drawLabel[i]) {
      ctx.fillStyle = textDim;
      ctx.textBaseline = alignBottom ? "top" : "bottom";
      ctx.fillText(m.label, m.pos + 3, alignBottom ? 2 : height - 2);
    }
  });
}

export function getChangeColor(change: string): string {
  switch (change) {
    case "Added":
      return getCSSColor("--color-added");
    case "Removed":
      return getCSSColor("--color-removed");
    case "Modified":
      return getCSSColor("--color-modified");
    default:
      return getCSSColor("--color-unchanged");
  }
}
