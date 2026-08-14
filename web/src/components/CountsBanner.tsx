// Shared "switch to Verbose/Full" banner for counts-only collections
// (Summary/Compact detail levels: the backend emits `counts` and no `items`).
// Used by DetailView's Devices/Clips/Automations tabs and by the generic
// collection rows in CollectionList.
export default function CountsBanner(props: {
  label: string;
  counts: { added: number; removed: number; modified: number } | null;
}) {
  const parts: string[] = [];
  if (props.counts) {
    if (props.counts.added) parts.push(`${props.counts.added} added`);
    if (props.counts.removed) parts.push(`${props.counts.removed} removed`);
    if (props.counts.modified) parts.push(`${props.counts.modified} modified`);
  }
  const summary = parts.length > 0 ? parts.join(", ") : "no changes";
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
      {props.label}: {summary} — switch to Verbose/Full to view.
    </div>
  );
}
