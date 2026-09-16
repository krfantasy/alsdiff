export interface TimeSignature {
  numer: number;
  denom: number;
}

export function quarterNoteToPosition(
  qn: number,
  ts: TimeSignature,
): { bar: number; beat: number; sixteenth: number } {
  if (qn <= 0) return { bar: 1, beat: 1, sixteenth: 1 };
  if (ts.denom === 0 || ts.numer === 0) return { bar: 1, beat: 1, sixteenth: 1 };
  const qnPerBar = (ts.numer * 4) / ts.denom;
  const qnPerBeat = 4 / ts.denom;
  const barCount = Math.floor(qn / qnPerBar);
  const bar = barCount + 1;
  const remBar = qn - barCount * qnPerBar;
  const beatCount = Math.floor(remBar / qnPerBeat);
  const beat = beatCount + 1;
  const remBeat = remBar - beatCount * qnPerBeat;
  const sixteenth = Math.floor(remBeat * 4) + 1;
  return { bar, beat, sixteenth };
}

export function formatPosition(
  bar: number,
  beat: number,
  sixteenth: number,
): string {
  if (beat === 1 && sixteenth === 1) return String(bar);
  if (sixteenth === 1) return `${bar}.${beat}`;
  return `${bar}.${beat}.${sixteenth}`;
}

export function quarterNoteToRealtime(
  qn: number,
  bpm: number,
): { min: number; sec: number; ms: number } {
  // Compute total milliseconds once and decompose with carries: rounding the
  // remainder per field could yield ms === 1000 (e.g. float noise just below
  // an integer second).
  const totalMs = Math.round((qn * 60 * 1000) / bpm);
  const min = Math.floor(totalMs / 60000);
  const remMs = totalMs - min * 60000;
  const sec = Math.floor(remMs / 1000);
  const ms = remMs - sec * 1000;
  return { min, sec, ms };
}

export function formatRealtime(
  min: number,
  sec: number,
  ms: number,
): string {
  // Tenths derived from the total: rounding ms = 950..999 per field produced
  // a "10th tenth" like 0:01.10 instead of carrying into the second.
  const totalTenths = Math.round(min * 600 + sec * 10 + ms / 100);
  const tMin = Math.floor(totalTenths / 600);
  const tSec = Math.floor((totalTenths - tMin * 600) / 10);
  const tenths = totalTenths - tMin * 600 - tSec * 10;
  const ss = String(tSec).padStart(2, "0");
  return tenths > 0 ? `${tMin}:${ss}.${tenths}` : `${tMin}:${ss}`;
}
