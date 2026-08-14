import type {
	ViewNode,
	ItemView,
	CollectionView,
	TrackData,
	TrackNode,
	ClipData,
	TimelineRange,
} from "../types";
import type { TimeSignature } from "./time-format";
import { extractMidiNotes } from "./midi-notes";

function isItem(node: ViewNode): node is ItemView {
  return node.type === "item";
}

function isCollection(node: ViewNode): node is CollectionView {
  return node.type === "collection";
}

function findCollection(
  children: ViewNode[],
  name: string,
): CollectionView | undefined {
  return children.find(
    (c): c is CollectionView => isCollection(c) && c.name === name,
  );
}

function getNumericField(
  children: ViewNode[],
  fieldName: string,
): number | undefined {
  const field = children.find(
    (c) => c.type === "field" && c.name === fieldName,
  );
  if (field && field.type === "field") {
    return (field.new_value ?? field.old_value) as number | undefined;
  }
  return undefined;
}

function getTrackIntField(children: ViewNode[], fieldName: string, defaultVal: number): number {
  const field = children.find(c => c.type === "field" && c.name === fieldName);
  if (field && field.type === "field") return ((field.new_value ?? field.old_value) as number) ?? defaultVal;
  return defaultVal;
}

/** Extract track ID from item name like "AudioTrack (#17): Bell" → 17 */
function extractTrackIdFromName(name: string): number {
  const m = name.match(/\(#(\d+)\)/);
  return m ? parseInt(m[1], 10) : 0;
}

export function extractTracks(livesetChildren: ViewNode[]): TrackData[] {
  const tracks: TrackData[] = [];

  const collectFromItem = (child: ItemView) => {
    if (child.domain_type !== "Track") return;
    let tc = child.children ?? [];
    // The Main Track is rendered as a section wrapper: its direct children is
    // a single `MainTrack: <name>` item whose OWN children hold the real
    // sections (Automations/Devices/Mixer/Routings). Unwrap it so the
    // per-track extractors (extractDevices/extractMixer/...) find the real
    // children instead of seeing only the wrapper.
    if (
      tc.length === 1 &&
      tc[0].type === "item" &&
      tc[0].name.startsWith("MainTrack:")
    ) {
      tc = tc[0].children ?? [];
    }
    const fieldTrackId = getTrackIntField(tc, "TrackId", 0);
    tracks.push({
      name: child.name,
      change: child.change,
      domainType: child.domain_type,
      trackId: fieldTrackId || extractTrackIdFromName(child.name),
      groupId: getTrackIntField(tc, "GroupId", -1),
      counts: child.counts,
      children: tc,
    });
  };

  for (const child of livesetChildren) {
    // Tracks/Returns are flat direct children of the LiveSet (tracks are
    // structural, never wrapped in a collection nor capped by
    // max_collection_items — see change_projector.ml create_liveset_item).
    if (isItem(child)) {
      collectFromItem(child);
    }
  }

  return tracks;
}

export function buildTrackHierarchy(tracks: TrackData[]): TrackNode[] {
  const nodes: TrackNode[] = tracks.map((track, i) => ({
    track,
    trackIndex: i,
    depth: 0,
    children: [],
  }));

  // Build index from trackId to node (first occurrence wins for lookups)
  const idToNode = new Map<number, TrackNode>();
  for (const node of nodes) {
    if (!idToNode.has(node.track.trackId)) {
      idToNode.set(node.track.trackId, node);
    }
  }

  const topNodes: TrackNode[] = [];
  for (const node of nodes) {
    if (node.track.groupId !== -1 && idToNode.has(node.track.groupId)) {
      idToNode.get(node.track.groupId)!.children.push(node);
    } else {
      topNodes.push(node);
    }
  }

  const setDepth = (ns: TrackNode[], depth: number) => {
    for (const n of ns) {
      n.depth = depth;
      setDepth(n.children, depth + 1);
    }
  };
  setDepth(topNodes, 0);

  return topNodes;
}

export function flattenVisibleTracks(
  rootNodes: TrackNode[],
  collapsedGroups: Set<number>,
): TrackNode[] {
  const result: TrackNode[] = [];
  const walk = (nodes: TrackNode[]) => {
    for (const node of nodes) {
      result.push(node);
      const isGroup = node.children.length > 0;
      if (isGroup && !collapsedGroups.has(node.track.trackId)) {
        walk(node.children);
      }
    }
  };
  walk(rootNodes);
  return result;
}

export function extractClips(track: TrackData): ClipData[] {
  const clips: ClipData[] = [];

  const clipsCollection = findCollection(track.children, "Clips");
  if (!clipsCollection) return clips;

  for (const node of clipsCollection.items ?? []) {
    if (!isItem(node) || node.domain_type !== "Clip") continue;

    const children = node.children ?? [];
    const startTime = getNumericField(children, "Start Time") ?? 0;
    const endTime = getNumericField(children, "End Time") ?? startTime + 4;

    const clipType = node.name.startsWith("AudioClip") ? "audio" : "midi";

    clips.push({
      name: node.name,
      change: node.change,
      startTime,
      endTime,
      children,
      clipType,
    });
  }

  return clips;
}

export function extractDevices(track: TrackData): ItemView[] {
  const devicesCollection = findCollection(track.children, "Devices");
  if (!devicesCollection) return [];
  return (devicesCollection.items ?? []).filter(isItem);
}

export function extractMixer(track: TrackData): ItemView | undefined {
  return track.children.find(
    (c): c is ItemView => isItem(c) && (c.name === "Mixer" || c.name === "Main Mixer"),
  );
}

export function extractAutomations(track: TrackData): ItemView[] {
  const autoCollection = findCollection(track.children, "Automations");
  if (!autoCollection) return [];
  return (autoCollection.items ?? []).filter(isItem);
}

/**
 * Return the `{added, removed, modified}` counts for a named counts-only
 * collection, or `null` if the collection is absent or carries items (i.e. is
 * not in the counts-only Summary/Compact shape). Used to surface a "switch to
 * Verbose/Full to view" banner when extractors return empty due to detail level.
 */
export function extractCollectionCounts(
  children: ViewNode[],
  name: string,
): { added: number; removed: number; modified: number } | null {
  const col = findCollection(children, name);
  if (!col) return null;
  // If items are present, the collection is listable; no counts banner needed.
  if (col.items && col.items.length > 0) return null;
  return col.counts ?? null;
}

export function extractRoutings(track: TrackData): ItemView | undefined {
	return track.children.find(
		(c): c is ItemView => isItem(c) && c.name === "Routings",
	);
}

/** Total number of changes in a counts breakdown (0 for null/undefined). */
export function sumCounts(
	counts:
		| { added: number; removed: number; modified: number }
		| null
		| undefined,
): number {
	return counts ? counts.added + counts.removed + counts.modified : 0;
}

export type DetailTabName = "devices" | "clip" | "pianoRoll" | "automation";

/** First detail tab with data for a freshly selected track (no clip chosen
 *  yet): devices → clip (pianoRoll when the first note-bearing clip exists,
 *  and counts-only Clips when no clip is selectable) → automation. Mirrors
 *  DetailView's tab visibility, including counts-only collections
 *  (Summary/Compact). Returns null when nothing renders. */
export function firstAvailableDetailTab(
	track: TrackData,
): { tab: DetailTabName; clipName?: string } | null {
	const hasDevices =
		extractDevices(track).length > 0 ||
		extractCollectionCounts(track.children, "Devices") !== null;
	if (hasDevices) return { tab: "devices" };

	const clips = extractClips(track);
	const clipCounts = extractCollectionCounts(track.children, "Clips");
	if (clips.length > 0 || clipCounts !== null) {
		if (clips.length > 0) {
			const withNotes = clips.find(
				(c) =>
					c.clipType === "midi" && extractMidiNotes(c.children).length > 0,
			);
			if (withNotes) return { tab: "pianoRoll", clipName: withNotes.name };
			return { tab: "clip", clipName: clips[0].name };
		}
		// Counts-only Clips (Summary/Compact): nothing is selectable, so no
		// clipName — DetailView shows the Clips counts tab in that state.
		return { tab: "clip" };
	}

	const hasAutomations =
		extractAutomations(track).length > 0 ||
		extractCollectionCounts(track.children, "Automations") !== null;
	if (hasAutomations) return { tab: "automation" };

	return null;
}

/**
 * Find the master track's MainMixer item. The master is identified POSITIVELY
 * by its item name ("MainTrack: ..."), never as "the first track with a Mixer
 * child" — regular tracks changed earlier in the list also carry Mixer
 * children, and reading theirs would attribute a track's volume to the
 * project tempo. The backend labels the master's MainMixer "Mixer"
 * (MainMixer.base label); keep the "Main Mixer" alias for compatibility.
 */
function findMasterMixer(children: ViewNode[]): ItemView | undefined {
	for (const child of children) {
		if (
			isItem(child) &&
			child.domain_type === "Track" &&
			child.name.startsWith("MainTrack")
		) {
			const mixer = child.children?.find(
				(c): c is ItemView =>
					isItem(c) && (c.name === "Mixer" || c.name === "Main Mixer"),
			);
			if (mixer?.children) return mixer;
		}
	}
	return undefined;
}

/** Decode Ableton's time-signature code: numer = code%99 + 1, denom = 2^(code/99). */
function decodeTimeSignatureCode(code: number): TimeSignature {
	if (code < 0 || Math.floor(code / 99) > 5) return { numer: 4, denom: 4 };
	return { numer: (code % 99) + 1, denom: 1 << Math.floor(code / 99) };
}

export function extractTempo(diffChildren: ViewNode[]): number {
	const mixer = findMasterMixer(diffChildren);
	if (!mixer) return 120;
	const mchildren = mixer.children ?? [];
	const tempo = mchildren.find(
		(c): c is ItemView => isItem(c) && c.name === "Tempo",
	);
	if (!tempo?.children) return 120;
	const value = getNumericField(tempo.children, "Value");
	return value ?? 120;
}

export function extractTimeSignature(
	diffChildren: ViewNode[],
): TimeSignature {
	const mixer = findMasterMixer(diffChildren);
	if (!mixer) return { numer: 4, denom: 4 };
	const mchildren = mixer.children ?? [];
	const ts = mchildren.find(
		(c): c is ItemView => isItem(c) && c.name === "Time Signature",
	);
	if (!ts?.children) return { numer: 4, denom: 4 };
	const code = getNumericField(ts.children, "Value");
	if (code === undefined) return { numer: 4, denom: 4 };
	return decodeTimeSignatureCode(Math.round(code));
}

export function computeTimelineRange(tracks: TrackData[]): TimelineRange {
  let minStart = Infinity;
  let maxEnd = -Infinity;

  const allClips: ClipData[] = [];
  for (const track of tracks) {
    allClips.push(...extractClips(track));
  }

  const changedClips = allClips.filter((c) => c.change !== "Unchanged");
  const source = changedClips.length > 0 ? changedClips : allClips;

  for (const clip of source) {
    if (clip.startTime < minStart) minStart = clip.startTime;
    if (clip.endTime > maxEnd) maxEnd = clip.endTime;
  }

  if (minStart === Infinity) {
    minStart = 0;
    maxEnd = 32;
  }

  const range = maxEnd - minStart;
  const padding = Math.max(4, range * 0.1);

  return {
    minStart: Math.max(0, minStart - padding),
    maxEnd: maxEnd + padding,
    totalBeats: Math.max(1, (maxEnd + padding) - Math.max(0, minStart - padding)),
  };
}

export function getChangeColor(change: string): string {
  switch (change) {
    case "Added":
      return "var(--color-added)";
    case "Removed":
      return "var(--color-removed)";
    case "Modified":
      return "var(--color-modified)";
    default:
      return "var(--color-unchanged)";
  }
}
