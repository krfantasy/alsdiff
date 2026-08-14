import { Show } from "solid-js";
import {
  tracks,
  selectedTrackIdx,
  selectedClipName,
  detailTab,
  setDetailTab,
  detailHeight,
} from "../stores/diff-store";
import {
	extractDevices,
	extractClips,
	extractAutomations,
	extractCollectionCounts,
	sumCounts,
} from "../lib/diff-parser";
import { extractMidiNotes } from "../lib/midi-notes";
import DeviceChain from "./DeviceChain";
import ClipDetail from "./ClipDetail";
import PianoRollView from "./PianoRollView";
import AutomationView from "./AutomationView";
import CountsBanner from "./CountsBanner";
import type { ClipData } from "../types";

export default function DetailView() {
  const selectedTrack = () => {
    const idx = selectedTrackIdx();
    if (idx == null) return null;
    return tracks()[idx] ?? null;
  };

  const selectedClip = (): ClipData | null => {
    const track = selectedTrack();
    if (!track) return null;
    const name = selectedClipName();
    if (!name) return null;
    return extractClips(track).find((c) => c.name === name) ?? null;
  };

  const devices = () => {
    const track = selectedTrack();
    return track ? extractDevices(track) : [];
  };

  // A tab is shown when the collection has items OR is counts-only (Summary/
  // Compact). In the counts-only case the body renders a "switch to
  // Verbose/Full" banner instead of the list.
  const deviceCounts = () => {
    const track = selectedTrack();
    return track ? extractCollectionCounts(track.children, "Devices") : null;
  };
  const hasDevices = () => devices().length > 0 || deviceCounts() !== null;

  // Clips are reachable in two shapes: a selected clip (listable Clips
  // collection, chosen via the arrangement or firstAvailableDetailTab) or a
  // counts-only Clips collection (Summary/Compact), where no clip is
  // selectable and the pane shows the counts banner instead.
  const clipCounts = () => {
    const track = selectedTrack();
    return track ? extractCollectionCounts(track.children, "Clips") : null;
  };
  const hasClip = () => selectedClip() !== null || clipCounts() !== null;
  const hasNotes = () => {
    const clip = selectedClip();
    if (!clip || clip.clipType !== "midi") return false;
    return extractMidiNotes(clip.children).length > 0;
  };

  const automations = () => {
    const track = selectedTrack();
    return track ? extractAutomations(track) : [];
  };
  const automationCounts = () => {
    const track = selectedTrack();
    return track ? extractCollectionCounts(track.children, "Automations") : null;
  };
  const hasAutomations = () => automations().length > 0 || automationCounts() !== null;
  // Count-aware tab labels: counts-only collections (Summary/Compact) show
  // the counts total instead of a misleading "(0)".
  const deviceCount = () => devices().length || sumCounts(deviceCounts());
  const automationCount = () => automations().length || sumCounts(automationCounts());

  return (
    <div class="detail-pane" data-testid="detail-pane" style={{ height: `${detailHeight()}px` }}>
      <Show
        when={selectedTrack()}
        fallback={
          <div
            style={{
              display: "flex",
              "align-items": "center",
              "justify-content": "center",
              height: "100%",
              color: "var(--text-dim)",
            }}
          >
            Select a track to view details
          </div>
        }
      >
        <div class="detail-tabs">
          <Show when={hasDevices()}>
            <div
              class={`detail-tab ${detailTab() === "devices" ? "active" : ""}`}
              data-testid="detail-tab-devices"
              onClick={() => setDetailTab("devices")}
            >
              Devices ({deviceCount()})
            </div>
          </Show>
          <Show when={hasClip()}>
            <div
              class={`detail-tab ${detailTab() === "clip" ? "active" : ""}`}
              data-testid={selectedClip() ? "detail-tab-clip" : "detail-tab-clips"}
              onClick={() => setDetailTab("clip")}
            >
              <Show when={selectedClip()} fallback={`Clips (${sumCounts(clipCounts())})`}>
                Clip: {selectedClip()?.name}
              </Show>
            </div>
          </Show>
          <Show when={hasNotes()}>
            <div
              class={`detail-tab ${detailTab() === "pianoRoll" ? "active" : ""}`}
              data-testid="detail-tab-pianoRoll"
              onClick={() => setDetailTab("pianoRoll")}
            >
              Piano Roll
            </div>
          </Show>
          <Show when={hasAutomations()}>
            <div
              class={`detail-tab ${detailTab() === "automation" ? "active" : ""}`}
              data-testid="detail-tab-automation"
              onClick={() => setDetailTab("automation")}
            >
              Automation ({automationCount()})
            </div>
          </Show>
        </div>
        <div class="detail-content">
          <Show when={detailTab() === "devices" && hasDevices()}>
            <Show when={devices().length > 0} fallback={<CountsBanner label="Devices" counts={deviceCounts()} />}>
              <DeviceChain devices={devices()} />
            </Show>
          </Show>
          <Show when={detailTab() === "clip" && hasClip()}>
            <Show
              when={selectedClip()}
              fallback={<CountsBanner label="Clips" counts={clipCounts()} />}
            >
              <ClipDetail clipChildren={selectedClip()?.children ?? []} />
            </Show>
          </Show>
          <Show when={detailTab() === "pianoRoll" && hasNotes()}>
            <PianoRollView clipChildren={selectedClip()?.children ?? []} />
          </Show>
          <Show when={detailTab() === "automation" && hasAutomations()}>
            <Show when={automations().length > 0} fallback={<CountsBanner label="Automations" counts={automationCounts()} />}>
              <AutomationView automationItems={automations()} />
            </Show>
          </Show>
          {/* Summary-level track items carry no collections at all: surface
              their counts so the pane isn't empty. */}
          <Show
            when={
              !hasDevices() &&
              !hasClip() &&
              !hasNotes() &&
              !hasAutomations() &&
              selectedTrack()?.counts
            }
          >
            <CountsBanner label="Track" counts={selectedTrack()?.counts ?? null} />
          </Show>
        </div>
      </Show>
    </div>
  );
}
