(* Unit tests for Json_renderer detail-level handling.
   These lock in the per-level output shape fixed by the Summary/Compact/Inline/Full
   bug: previously the JSON renderer recursed into all children for every non-Ignore
   level. Now Summary shows change counts (no children), Compact drops Field children,
   and only Inline/Full expand fully. Mirrors text_renderer semantics. *)

open Alsdiff_output.Text_renderer
open Alsdiff_output.View_model

(* A detail_config where every change-type maps to the given level. Built from the
   `full` preset so prefix/style fields stay populated. *)
let cfg_of level =
  {
    full with
    added = level;
    removed = level;
    modified = level;
    unchanged = Ignore;
    type_overrides = [];
  }

let has_key key = function
  | `Assoc fields -> List.mem_assoc key fields
  | _ -> false

(* Render a single top-level view; return the list of objects under "diff". *)
let render_one cfg view =
  let str = Alsdiff_output.Json_renderer.render cfg [ view ] in
  match Yojson.Safe.from_string str with
  | `Assoc [ ("diff", `List items) ] -> items
  | _ -> Alcotest.failf "unexpected top-level structure: %s" str

let one items =
  match items with
  | [ x ] -> x
  | _ -> Alcotest.failf "expected exactly one rendered object, got %d" (List.length items)

(* element-like item: only Field children *)
let elem_item =
  Item
    {
      name = "Param";
      change = Modified;
      domain_type = DTOther;
      children =
        [
          Field
            {
              name = "Value";
              change = Modified;
              domain_type = DTOther;
              oldval = Some (Fint 1);
              newval = Some (Fint 2);
            };
        ];
    }

(* section-like item: nested Item child, non-LiveSet *)
let section_item =
  Item
    {
      name = "Track";
      change = Modified;
      domain_type = DTTrack;
      children =
        [
          Item
            {
              name = "Mixer";
              change = Modified;
              domain_type = DTMixer;
              children = [];
            };
        ];
    }

(* LiveSet item (root container) *)
let liveset_item =
  Item
    {
      name = "LiveSet";
      change = Modified;
      domain_type = DTLiveset;
      children =
        [
          Item
            {
              name = "Track";
              change = Modified;
              domain_type = DTTrack;
              children = [];
            };
        ];
    }

let one_item_collection =
  Collection
    {
      name = "Devices";
      change = Modified;
      domain_type = DTDevice;
      items =
        [
          Item
            { name = "Dev"; change = Added; domain_type = DTDevice; children = [] };
        ];
    }

let empty_collection =
  Collection
    {
      name = "Devices";
      change = Modified;
      domain_type = DTDevice;
      items = [];
    }

(* element-like Summary: counts present, no children *)
let test_elem_summary () =
  let item = one (render_one (cfg_of Summary) elem_item) in
  Alcotest.(check bool) "elem Summary has counts" (has_key "counts" item) true;
  Alcotest.(check bool) "elem Summary no children" (not (has_key "children" item)) true

(* element-like Full: children present (fields), no counts *)
let test_elem_full () =
  let item = one (render_one (cfg_of Full) elem_item) in
  Alcotest.(check bool) "elem Full has children" (has_key "children" item) true;
  Alcotest.(check bool) "elem Full no counts" (not (has_key "counts" item)) true

(* element-like Compact: no children (fields dropped), no counts *)
let test_elem_compact () =
  let item = one (render_one (cfg_of Compact) elem_item) in
  Alcotest.(check bool) "elem Compact no children" (not (has_key "children" item)) true;
  Alcotest.(check bool) "elem Compact no counts" (not (has_key "counts" item)) true

(* section-like Summary (non-LiveSet): counts via count_sub_views_breakdown, no children *)
let test_section_summary () =
  let item = one (render_one (cfg_of Summary) section_item) in
  Alcotest.(check bool) "section Summary has counts" (has_key "counts" item) true;
  Alcotest.(check bool) "section Summary no children" (not (has_key "children" item)) true

(* LiveSet Summary: renders sub-views (root container special case), no counts *)
let test_liveset_summary () =
  let item = one (render_one (cfg_of Summary) liveset_item) in
  Alcotest.(check bool) "liveset Summary has children" (has_key "children" item) true;
  Alcotest.(check bool) "liveset Summary no counts" (not (has_key "counts" item)) true

(* collection Summary: always renders with counts, no items *)
let test_collection_summary () =
  let col = one (render_one (cfg_of Summary) one_item_collection) in
  Alcotest.(check bool) "collection Summary has counts" (has_key "counts" col) true;
  Alcotest.(check bool) "collection Summary no items" (not (has_key "items" col)) true

(* collection Compact with no renderable items: omitted entirely (None) *)
let test_collection_compact_empty () =
  let items = render_one (cfg_of Compact) empty_collection in
  Alcotest.(check int) "empty collection Compact omitted" 0 (List.length items)

(* Ignore level: nothing renders *)
let test_ignore () =
  let items = render_one (cfg_of Ignore) elem_item in
  Alcotest.(check int) "ignore renders nothing" 0 (List.length items)

let tests =
  [
    "element Summary", `Quick, test_elem_summary;
    "element Full", `Quick, test_elem_full;
    "element Compact", `Quick, test_elem_compact;
    "section Summary", `Quick, test_section_summary;
    "liveset Summary", `Quick, test_liveset_summary;
    "collection Summary", `Quick, test_collection_summary;
    "collection Compact empty", `Quick, test_collection_compact_empty;
    "ignore", `Quick, test_ignore;
  ]

let () = Alcotest.run "Json renderer detail levels" [ "json_renderer", tests ]
