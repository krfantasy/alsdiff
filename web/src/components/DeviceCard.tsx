import type { CollectionView, FieldView, ItemView, ViewNode } from "../types";
import DiffIndicator from "./DiffIndicator";
import { ViewNodeRow } from "./CollectionList";
import { For, Show, createMemo, createSignal } from "solid-js";

interface Props {
  device: ItemView;
}

function isItemView(node: ViewNode): node is ItemView {
  return node.type === "item";
}

function isFieldView(node: ViewNode): node is FieldView {
  return node.type === "field";
}

// A compact row for one param: either "name old → new" (Value moved) or
// "name meta" (only Automation/Modulation moved).
interface ParamRow {
  name: string;
  oldVal?: string;
  newVal?: string;
  change: string;
  meta?: string;
}

export default function DeviceCard(props: Props) {
  const [collapsed, setCollapsed] = createSignal(true);

  const deviceName = () => {
    const match = props.device.name.match(/:\s*(.+)/);
    return match ? match[1] : props.device.name;
  };

  const isGroupDevice = () => {
    return props.device.children?.some(
      (c) => c.type === "collection" && c.name === "Branches"
    );
  };

  const branchDevices = (): ItemView[] => {
    if (!isGroupDevice()) return [];
    const branches = props.device.children?.find(
      (c) => c.type === "collection" && c.name === "Branches"
    );
    if (!branches || branches.type !== "collection") return [];
    return (branches.items ?? [])
      .filter(isItemView)
      .flatMap((branch) => (branch.children ?? []).filter(isItemView));
  };

  // Builds the compact row for one param item, or undefined when the param
  // has nothing to summarize compactly (nothing moved at all).
  const buildParamRow = (param: ItemView): ParamRow | undefined => {
    const fields = (param.children ?? []).filter(isFieldView);
    const value = fields.find((f) => f.name === "Value");
    if (value && (value.old_value != null || value.new_value != null)) {
      return {
        name: param.name,
        oldVal: value.old_value != null ? String(value.old_value) : undefined,
        newVal: value.new_value != null ? String(value.new_value) : undefined,
        change: param.change,
      };
    }
    // Value did not move, but the param can still be Modified because its
    // automation/modulation target did. Summarize that as meta text.
    const metaParts = fields
      .filter(
        (f) =>
          (f.name === "Automation" || f.name === "Modulation") &&
          (f.old_value != null || f.new_value != null)
      )
      .map((f) => {
        const parts = [f.old_value, f.new_value]
          .filter((v) => v != null)
          .map((v) => String(v));
        return `${f.name} ${parts.join(" → ")}`;
      });
    if (metaParts.length === 0) return undefined;
    return { name: param.name, change: param.change, meta: metaParts.join(" ") };
  };

  // Computed once and shared by the compact rows and nonParamChildren so the
  // drop decision below always matches what was actually rendered. For each
  // Parameters collection we record its rows and whether EVERY param item in
  // it got a compact row — the collection may only leave the generic path
  // when nothing in it would be lost.
  const paramCollections = createMemo(() => {
    const out: { node: CollectionView; rows: ParamRow[]; allRendered: boolean }[] = [];
    for (const child of props.device.children ?? []) {
      // The backend emits Parameters as a collection of param items; the
      // param's identity is its item name (no "Name" field).
      if (child.type !== "collection" || child.name !== "Parameters") continue;
      const rawItems = child.items ?? [];
      const paramItems = rawItems.filter(isItemView);
      const rows = paramItems
        .map(buildParamRow)
        .filter((r): r is ParamRow => r !== undefined);
      const allRendered =
        rawItems.length > 0 &&
        paramItems.length === rawItems.length &&
        rows.length === paramItems.length;
      out.push({ node: child, rows, allRendered });
    }
    return out;
  });

  const paramRows = createMemo(() => paramCollections().flatMap((c) => c.rows));

  const nonParamChildren = () => {
    // Drop the Parameters collection only when every one of its param items
    // was rendered as a compact row. A param can be Modified because its
    // automation/modulation changed while Value stayed put — if such an item
    // (or any non-item entry) has no compact row, the collection must stay
    // on the generic ViewNodeRow path or its change would disappear.
    const dropped = new Set<ViewNode>(
      paramCollections()
        .filter((c) => c.allRendered)
        .map((c) => c.node)
    );
    return (props.device.children ?? []).filter(
      (c) =>
        !(c.type === "collection" && c.name === "Branches") && !dropped.has(c)
    );
  };

  return (
    <div class={`device-card${isGroupDevice() ? " group-device" : ""}`} data-testid="device-card">
      <div class="device-name" onClick={() => setCollapsed((c) => !c)}>
        <span class="collapse-icon">{collapsed() ? "▶" : "▼"}</span>
        {deviceName()}
        <DiffIndicator change={props.device.change} showLabel={false} />
      </div>
      <Show when={!collapsed()}>
        {paramRows().map((p) => (
          <div class="param-change">
            <span class="param-name">{p.name}</span>
            {p.meta && <span>{p.meta}</span>}
            {p.oldVal && <span class="old-value">{p.oldVal}</span>}
            {p.oldVal && p.newVal && <span class="arrow">&rarr;</span>}
            {p.newVal && <span class="new-value">{p.newVal}</span>}
          </div>
        ))}
        <For each={nonParamChildren()}>
          {(child) => <ViewNodeRow node={child} depth={1} />}
        </For>
        {branchDevices().length > 0 && (
          <div class="nested-devices">
            <For each={branchDevices()}>
              {(device) => <DeviceCard device={device} />}
            </For>
          </div>
        )}
      </Show>
    </div>
  );
}
