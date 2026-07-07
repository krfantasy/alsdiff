import { For } from "solid-js";
import { loadFileNameA, loadFileNameB } from "../stores/diff-store";

// A loading overlay for the content area while two .als files are diffed.
//
// The signature element is the equalizer/spectrum bar — drawn from the
// audio-tool world this app lives in (mixer meters, spectrum analysers) —
// rather than a generic spinner. One animated motif, amber accent, nothing
// else competes with it.
//
// Ambient feedback: a small card sits bottom-right the whole time, naming the
// two files being compared. Active feedback: hovering the dimmed content
// brightens a centered status hint, so the user always has a clear "loading"
// cue no matter where their cursor is.
//
// Styles live in styles/loading-overlay.css (imported in main.tsx) — kept out
// of an inline <style> string because the interleaved @keyframes made Vite
// drop rules silently.
export default function LoadingOverlay() {
  return (
    <div class="loading-overlay" data-testid="loading-spinner">
      <div class="loading-hover-hint" data-testid="loading-hint">
        Comparing files…
      </div>
      <div class="loading-card" data-testid="loading-card">
        <div class="eq-bars" aria-hidden="true">
          <For each={[0, 1, 2, 3, 4]}>
            {(i) => <i style={{ "animation-delay": `${i * 0.13}s` }} />}
          </For>
        </div>
        <div class="loading-card-text">
          <span class="eyebrow">COMPARING</span>
          <span class="files" data-testid="loading-files">
            {loadFileNameA() || "file A"}
            <span class="arrow"> → </span>
            {loadFileNameB() || "file B"}
          </span>
        </div>
      </div>
    </div>
  );
}
