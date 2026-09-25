import { test, expect } from "@playwright/test";
import { extractTempo, extractTracks } from "../src/lib/diff-parser";

test("extractTracks resolves ids from Identity-stamped riders", () => {
  const tracks = extractTracks([
    {
      type: "item",
      name: "MidiTrack (#17): Bell",
      change: "Modified",
      domain_type: "Track",
      children: [
        { type: "field", name: "TrackId", change: "Unchanged", domain_type: "Track", kind: "Identity", new_value: 17 },
        { type: "field", name: "GroupId", change: "Unchanged", domain_type: "Track", kind: "Identity", new_value: 2 },
      ],
    } as any,
  ]);
  expect(tracks).toHaveLength(1);
  expect(tracks[0].trackId).toBe(17);
  expect(tracks[0].groupId).toBe(2);
});

test("resolves a Modified GroupId emitted as Content (the changed id is the diff)", () => {
  const tracks = extractTracks([
    {
      type: "item",
      name: "AudioTrack (#5): Pad",
      change: "Modified",
      domain_type: "Track",
      children: [
        { type: "field", name: "GroupId", change: "Modified", domain_type: "Track", old_value: 1, new_value: 3 },
      ],
    } as any,
  ]);
  expect(tracks[0].groupId).toBe(3);
});

test("parses artifacts without the kind key", () => {
  const tracks = extractTracks([
    {
      type: "item",
      name: "AudioTrack (#5): 5-Audio",
      change: "Removed",
      domain_type: "Track",
      children: [
        { type: "field", name: "TrackId", change: "Removed", domain_type: "Track", old_value: 5 },
      ],
    } as any,
  ]);
  expect(tracks[0].trackId).toBe(5);
});

test("defaults trackId to 0 without any id field (regex hack retired)", () => {
  const tracks = extractTracks([
    { type: "item", name: "MainTrack: Master", change: "Modified", domain_type: "Track", children: [] } as any,
  ]);
  expect(tracks[0].trackId).toBe(0);
});

test("Summary-shaped Added/Removed tracks keep ids via Identity riders", () => {
  // The projector re-stamps the value-side TrackId/GroupId of Added/Removed
  // tracks as Identity, so they ride counts-only Summary levels — the ids
  // no longer need the retired display-name regex.
  const tracks = extractTracks([
    {
      type: "item",
      name: "AudioTrack (#5): Pad",
      change: "Removed",
      domain_type: "Track",
      counts: { added: 0, removed: 4, modified: 1 },
      children: [
        { type: "field", name: "TrackId", change: "Removed", domain_type: "Track", kind: "Identity", old_value: 5 },
        { type: "field", name: "GroupId", change: "Removed", domain_type: "Track", kind: "Identity", old_value: 2 },
      ],
    } as any,
    {
      type: "item",
      name: "AudioTrack (#9): Lead",
      change: "Added",
      domain_type: "Track",
      counts: { added: 4, removed: 0, modified: 0 },
      children: [
        { type: "field", name: "TrackId", change: "Added", domain_type: "Track", kind: "Identity", new_value: 9 },
        { type: "field", name: "GroupId", change: "Added", domain_type: "Track", kind: "Identity", new_value: 2 },
      ],
    } as any,
  ]);
  expect(tracks[0].trackId).toBe(5);
  expect(tracks[0].groupId).toBe(2);
  expect(tracks[1].trackId).toBe(9);
  expect(tracks[1].groupId).toBe(2);
});

test("extractTempo reads Context-stamped liveset fields", () => {
  expect(
    extractTempo([
      { type: "field", name: "Tempo", change: "Unchanged", domain_type: "Liveset", kind: "Context", new_value: 124 } as any,
    ]),
  ).toBe(124);
});
