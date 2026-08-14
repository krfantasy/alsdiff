import type { ItemView, ViewNode } from "../types";
import DiffIndicator from "./DiffIndicator";
import { ViewNodeRow } from "./CollectionList";
import { For, Show, createSignal } from "solid-js";

interface Props {
  device: ItemView;
}

function isItemView(node: ViewNode): node is ItemView {
  return node.type === "item";
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

  const paramFields = () => {
    const params: {
      name: string;
      oldVal?: string;
      newVal?: string;
      change: string;
    }[] = [];
    const children = props.device.children ?? [];

    for (const child of children) {
      // The backend emits Parameters as a collection of param items; the
      // param's identity is its item name (no "Name" field).
      if (child.type !== "collection" || child.name !== "Parameters") continue;
      for (const param of child.items ?? []) {
        if (param.type !== "item") continue;
        const value = (param.children ?? []).find(
          (f) => f.type === "field" && f.name === "Value"
        );
        if (!value || value.type !== "field") continue;
        const oldVal = value.old_value != null ? String(value.old_value) : undefined;
        const newVal = value.new_value != null ? String(value.new_value) : undefined;
        if (oldVal !== undefined || newVal !== undefined) {
          params.push({ name: param.name, oldVal, newVal, change: param.change });
        }
      }
    }

    return params;
  };

  const nonParamChildren = () => {
    const children = props.device.children ?? [];
    return children.filter(
      (c) =>
        !(c.type === "collection" && c.name === "Branches") &&
        !(
          c.type === "collection" &&
          c.name === "Parameters" &&
          (c.items?.length ?? 0) > 0
        )
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
        {paramFields().map((p) => (
          <div class="param-change">
            <span class="param-name">{p.name}</span>
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
