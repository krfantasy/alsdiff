import type { ViewNode, ItemView, FieldView } from "../types";
import { For, Show } from "solid-js";
import FieldChange from "./FieldChange";
import CollectionList from "./CollectionList";

interface Props {
  clipChildren: ViewNode[];
}

function getFields(children: ViewNode[]): FieldView[] {
  return children.filter((c): c is FieldView => c.type === "field");
}

function findChild(children: ViewNode[], name: string): ItemView | undefined {
  return children.find(
    (c): c is ItemView => c.type === "item" && c.name === name,
  );
}

function findCollection(
  children: ViewNode[],
  name: string,
): ViewNode | undefined {
  return children.find(
    (c) => c.type === "collection" && c.name === name,
  );
}

// A section only renders when it has field rows to show: detail presets that
// keep an item but drop its (unchanged) fields would otherwise render a bare
// heading box.
function sectionFields(node: ItemView | undefined): FieldView[] {
  return node ? getFields(node.children ?? []) : [];
}

export default function ClipDetail(props: Props) {
  const fields = () => getFields(props.clipChildren);
  const loop = () => findChild(props.clipChildren, "Loop");
  const sig = () => findChild(props.clipChildren, "TimeSignature");
  const sampleRef = () => findChild(props.clipChildren, "SampleRef");
  const fade = () => findChild(props.clipChildren, "Fade");
  const notes = () => findCollection(props.clipChildren, "Notes");

  return (
    <div class="clip-detail" data-testid="clip-detail">
      <Show when={fields().length > 0}>
        <div class="clip-detail-section">
          <h4>Properties</h4>
          <For each={fields()}>
            {(f) => <FieldChange field={f} />}
          </For>
        </div>
      </Show>

      <Show when={sectionFields(loop()).length > 0}>
        <div class="clip-detail-section">
          <h4>Loop</h4>
          <For each={sectionFields(loop())}>
            {(f) => <FieldChange field={f} />}
          </For>
        </div>
      </Show>

      <Show when={sectionFields(sig()).length > 0}>
        <div class="clip-detail-section">
          <h4>Time Signature</h4>
          <For each={sectionFields(sig())}>
            {(f) => <FieldChange field={f} />}
          </For>
        </div>
      </Show>

      <Show when={sectionFields(sampleRef()).length > 0}>
        <div class="clip-detail-section">
          <h4>Sample Reference</h4>
          <For each={sectionFields(sampleRef())}>
            {(f) => <FieldChange field={f} />}
          </For>
        </div>
      </Show>

      <Show when={sectionFields(fade()).length > 0}>
        <div class="clip-detail-section">
          <h4>Fade</h4>
          <For each={sectionFields(fade())}>
            {(field) => <FieldChange field={field} />}
          </For>
        </div>
      </Show>

      <Show when={notes()}>
        {(n) =>
          n().type === "collection" ? (
            <div class="clip-detail-section" style={{ "grid-column": "1 / -1" }}>
              <CollectionList collection={n() as any} />
            </div>
          ) : null
        }
      </Show>
    </div>
  );
}
