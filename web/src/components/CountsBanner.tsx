// Shared "switch to Verbose/Full" banner for counts-only collections
// (Summary/Compact detail levels: the backend emits `counts` and no `items`).
// Used by DetailView's Devices/Clips/Automations tabs and by the generic
// collection rows in CollectionList.
export default function CountsBanner(props: {
  label: string;
  counts: { added: number; removed: number; modified: number } | null;
}) {
  return (
    <div
      data-testid="counts-banner"
      style={{
        display: "flex",
        "align-items": "center",
        "justify-content": "center",
        height: "100%",
        color: "var(--text-dim)",
        "font-size": "13px",
        padding: "16px",
      }}
    >
      {/* Computed inside the JSX so `props.counts` stays tracked — <Show>
          caches children across truthy→truthy switches, and a one-shot
          copy at setup showed the previous track's counts. */}
      {props.label}:{" "}
      {(() => {
        const counts = props.counts;
        const parts: string[] = [];
        if (counts) {
          if (counts.added) parts.push(`${counts.added} added`);
          if (counts.removed) parts.push(`${counts.removed} removed`);
          if (counts.modified) parts.push(`${counts.modified} modified`);
        }
        return parts.length > 0 ? parts.join(", ") : "no changes";
      })()}{" "}
      — switch to Verbose/Full to view.
    </div>
  );
}
