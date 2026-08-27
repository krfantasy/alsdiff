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

(* collection Compact: header+counts, no items (Compact == Summary for collections) *)
let test_collection_compact () =
  let col = one (render_one (cfg_of Compact) one_item_collection) in
  Alcotest.(check bool) "collection Compact has counts" (has_key "counts" col) true;
  Alcotest.(check bool) "collection Compact no items" (not (has_key "items" col)) true

(* collection Compact with no renderable items: still renders a counts node (mirrors
   empty Summary), not omitted — the header+counts form always surfaces. *)
let test_collection_compact_empty () =
  let items = render_one (cfg_of Compact) empty_collection in
  let col = one items in
  Alcotest.(check int) "empty collection Compact renders one node" 1 (List.length items);
  Alcotest.(check bool) "empty collection Compact has counts" (has_key "counts" col) true;
  Alcotest.(check bool) "empty collection Compact no items" (not (has_key "items" col)) true

(* Ignore level: nothing renders *)
let test_ignore () =
  let items = render_one (cfg_of Ignore) elem_item in
  Alcotest.(check int) "ignore renders nothing" 0 (List.length items)

(* Track items carry TrackId/GroupId identity fields at EVERY detail level:
   consumers (the web app) nest tracks by them, including in counts-only
   Summary and Field-dropping Compact shapes where no other children render. *)
let track_identity_item change =
  Item
    {
      name = "MidiTrack (#14): Lead";
      change;
      domain_type = DTTrack;
      children =
        [
          Field
            {
              name = "TrackId"; change = Unchanged; domain_type = DTTrack;
              oldval = None; newval = Some (Fint 14);
            };
          Field
            {
              name = "GroupId"; change = Unchanged; domain_type = DTTrack;
              oldval = None; newval = Some (Fint 91);
            };
          Item { name = "Mixer"; change = Modified; domain_type = DTMixer; children = [] };
        ];
    }

let children_of = function
  | `Assoc fields -> (List.assoc "children" fields : Yojson.Safe.t)
  | other -> Alcotest.failf "expected object with children, got %s" (Yojson.Safe.to_string other)

let is_field_named name = function
  | `Assoc fields ->
    (match List.assoc_opt "name" fields with
     | Some (`String n) -> n = name
     | _ -> false)
  | _ -> false

(* Summary is counts-only for a section-like track item — identity still rides
   along as the only children. *)
let test_track_identity_at_summary () =
  let item = one (render_one (cfg_of Summary) (track_identity_item Removed)) in
  Alcotest.(check bool) "counts still present" true (has_key "counts" item);
  (match children_of item with
   | `List fields ->
     Alcotest.(check bool) "TrackId in children" true (List.exists (is_field_named "TrackId") fields);
     Alcotest.(check bool) "GroupId in children" true (List.exists (is_field_named "GroupId") fields);
     Alcotest.(check int) "only identity children" 2 (List.length fields)
   | other -> Alcotest.failf "expected children list, got %s" (Yojson.Safe.to_string other))

(* Compact keeps Item children but drops Fields — identity is the exception. *)
let test_track_identity_at_compact_no_duplicate () =
  let item = one (render_one (cfg_of Compact) (track_identity_item Modified)) in
  (match children_of item with
   | `List fields ->
     Alcotest.(check int) "TrackId exactly once" 1 (List.length (List.filter (is_field_named "TrackId") fields));
     Alcotest.(check int) "GroupId exactly once" 1 (List.length (List.filter (is_field_named "GroupId") fields));
     Alcotest.(check int) "Mixer child still present" 1 (List.length (List.filter (is_field_named "Mixer") fields))
   | other -> Alcotest.failf "expected children list, got %s" (Yojson.Safe.to_string other))

(* Full renders everything — identity must not be duplicated even though the
   normal path would also render these Unchanged fields at this level. *)
let test_track_identity_at_full () =
  let cfg = { (cfg_of Full) with unchanged = Full } in
  let item = one (render_one cfg (track_identity_item Modified)) in
  (match children_of item with
   | `List fields ->
     Alcotest.(check int) "TrackId exactly once" 1 (List.length (List.filter (is_field_named "TrackId") fields));
     Alcotest.(check int) "GroupId exactly once" 1 (List.length (List.filter (is_field_named "GroupId") fields))
   | other -> Alcotest.failf "expected children list, got %s" (Yojson.Safe.to_string other))

(* A non-track item with same-named fields gets NO special treatment: at
   Summary it stays counts-only. *)
let test_non_track_item_no_identity_hoisting () =
  let other =
    Item
      {
        name = "Foo"; change = Modified; domain_type = DTOther;
        children =
          [
            Field
              {
                name = "TrackId"; change = Unchanged; domain_type = DTOther;
                oldval = None; newval = Some (Fint 7);
              };
          ];
      }
  in
  let item = one (render_one (cfg_of Summary) other) in
  Alcotest.(check bool) "no children key for non-track Summary item" true (not (has_key "children" item))

(* LiveSet items carry the project's Tempo/Time Signature context fields at
   EVERY detail level (mirroring the track identity fields): the backend
   emits them as Unchanged fields on the LiveSet item so the web app's
   realtime ruler works even when the whole MainTrack item is level-dropped
   under Summary/Compact presets. They must ride the identity channel exactly
   once each — including the counts-only Summary root special case, which
   previously dropped identity. *)
let liveset_tempo_item =
  Item
    {
      name = "LiveSet: Demo";
      change = Modified;
      domain_type = DTLiveset;
      children =
        [
          Field
            {
              name = "Tempo"; change = Unchanged; domain_type = DTLiveset;
              oldval = None; newval = Some (Ffloat 138.0);
            };
          Field
            {
              name = "Time Signature"; change = Unchanged; domain_type = DTLiveset;
              oldval = None; newval = Some (Fint 201);
            };
          Field
            {
              name = "Creator"; change = Unchanged; domain_type = DTLiveset;
              oldval = None; newval = Some (Fstring "Ableton");
            };
          Item
            {
              name = "MidiTrack (#14): Lead";
              change = Modified;
              domain_type = DTTrack;
              children = [];
            };
        ];
    }

(* Summary: LiveSet keeps its sub-views AND its context fields (this branch
   previously dropped identity entirely). Normal Unchanged fields (Creator)
   still ride nothing — they are not special. *)
let test_liveset_tempo_context_at_summary () =
  let item = one (render_one (cfg_of Summary) liveset_tempo_item) in
  Alcotest.(check bool) "liveset Summary no counts" true (not (has_key "counts" item));
  (match children_of item with
   | `List fields ->
     Alcotest.(check int) "Tempo exactly once" 1
       (List.length (List.filter (is_field_named "Tempo") fields));
     Alcotest.(check int) "Time Signature exactly once" 1
       (List.length (List.filter (is_field_named "Time Signature") fields));
     (* sub-view (the track item) still renders in Summary *)
     Alcotest.(check int) "track child renders" 1
       (List.length (List.filter (is_field_named "MidiTrack (#14): Lead") fields));
     (* non-context Unchanged field does not ride along *)
     Alcotest.(check int) "Creator stays dropped" 0
       (List.length (List.filter (is_field_named "Creator") fields))
   | other -> Alcotest.failf "expected children list, got %s" (Yojson.Safe.to_string other))

(* Compact: normal Fields drop, context fields ride, Item children stay. *)
let test_liveset_tempo_context_at_compact () =
  let item = one (render_one (cfg_of Compact) liveset_tempo_item) in
  (match children_of item with
   | `List fields ->
     Alcotest.(check int) "Tempo exactly once" 1
       (List.length (List.filter (is_field_named "Tempo") fields));
     Alcotest.(check int) "Time Signature exactly once" 1
       (List.length (List.filter (is_field_named "Time Signature") fields));
     Alcotest.(check int) "track child stays at Compact" 1
       (List.length (List.filter (is_field_named "MidiTrack (#14): Lead") fields));
     Alcotest.(check int) "Creator dropped at Compact" 0
       (List.length (List.filter (is_field_named "Creator") fields))
   | other -> Alcotest.failf "expected children list, got %s" (Yojson.Safe.to_string other))

(* Full renders everything — context fields must not be duplicated even
   though the normal path would also render these Unchanged fields. *)
let test_liveset_tempo_context_at_full () =
  let cfg = { (cfg_of Full) with unchanged = Full } in
  let item = one (render_one cfg liveset_tempo_item) in
  (match children_of item with
   | `List fields ->
     Alcotest.(check int) "Tempo exactly once" 1
       (List.length (List.filter (is_field_named "Tempo") fields));
     Alcotest.(check int) "Time Signature exactly once" 1
       (List.length (List.filter (is_field_named "Time Signature") fields))
   | other -> Alcotest.failf "expected children list, got %s" (Yojson.Safe.to_string other))

(* A non-LiveSet item with same-named fields gets NO tempo hoisting. *)
let test_non_liveset_item_no_tempo_hoisting () =
  let other =
    Item
      {
        name = "Mixer"; change = Modified; domain_type = DTMixer;
        children =
          [
            Field
              {
                name = "Tempo"; change = Unchanged; domain_type = DTMixer;
                oldval = None; newval = Some (Ffloat 138.0);
              };
          ];
      }
  in
  let item = one (render_one (cfg_of Summary) other) in
  Alcotest.(check bool) "no children key for non-LiveSet Summary item" true (not (has_key "children" item))

(* Ffloat NaN/Inf must serialize to JSON null, not the raw NaN/Infinity tokens.
   RFC 8259 forbids NaN/Infinity, so emitting them makes the whole document
   unparseable by strict consumers (jq, Python json.loads). NaN/Inf is
   reachable from corrupt .als automation values (change_projector wraps
   Automation.FloatEvent as Ffloat with no validation). See review_0614.org
   "Ffloat NaN/Inf serializes to invalid JSON". *)
let float_field new_val =
  Field
    {
      name = "Value";
      change = Modified;
      domain_type = DTOther;
      oldval = None;
      newval = Some (Ffloat new_val);
    }

(* Extract the new_value JSON node from a rendered field object. *)
let new_value_of = function
  | `Assoc fields -> (List.assoc_opt "new_value" fields : Yojson.Safe.t option)
  | _ -> None

let test_nan_float_is_null () =
  let items = render_one (cfg_of Full) (float_field Float.nan) in
  let field_obj = one items in
  (* Must parse cleanly — the bug made the document unparseable. *)
  match new_value_of field_obj with
  | Some `Null -> () (* expected *)
  | other ->
    Alcotest.failf "expected `Null for NaN, got %s"
      (Yojson.Safe.to_string (Option.value other ~default:`Null))

let test_infinity_float_is_null () =
  let items = render_one (cfg_of Full) (float_field infinity) in
  let field_obj = one items in
  match new_value_of field_obj with
  | Some `Null -> () (* expected *)
  | other ->
    Alcotest.failf "expected `Null for Infinity, got %s"
      (Yojson.Safe.to_string (Option.value other ~default:`Null))

(* Finite Ffloat must still serialize as a JSON number (regression guard so
   the sanitization branch doesn't accidentally coerce all floats to null). *)
let test_finite_float_is_number () =
  let items = render_one (cfg_of Full) (float_field 1.5) in
  let field_obj = one items in
  match new_value_of field_obj with
  | Some (`Float f) -> Alcotest.(check bool) "finite float preserved" true (Float.equal f 1.5)
  | other ->
    Alcotest.failf "expected `Float 1.5, got %s"
      (Yojson.Safe.to_string (Option.value other ~default:`Null))

let tests =
  [
    "element Summary", `Quick, test_elem_summary;
    "element Full", `Quick, test_elem_full;
    "element Compact", `Quick, test_elem_compact;
    "section Summary", `Quick, test_section_summary;
    "liveset Summary", `Quick, test_liveset_summary;
    "collection Summary", `Quick, test_collection_summary;
    "collection Compact", `Quick, test_collection_compact;
    "collection Compact empty", `Quick, test_collection_compact_empty;
    "ignore", `Quick, test_ignore;
    "track identity at Summary", `Quick, test_track_identity_at_summary;
    "track identity at Compact no duplicate", `Quick, test_track_identity_at_compact_no_duplicate;
    "track identity at Full", `Quick, test_track_identity_at_full;
    "non-track item gets no identity hoisting", `Quick, test_non_track_item_no_identity_hoisting;
    "liveset tempo context at Summary", `Quick, test_liveset_tempo_context_at_summary;
    "liveset tempo context at Compact", `Quick, test_liveset_tempo_context_at_compact;
    "liveset tempo context at Full", `Quick, test_liveset_tempo_context_at_full;
    "non-LiveSet item gets no tempo hoisting", `Quick, test_non_liveset_item_no_tempo_hoisting;
    "NaN float serializes to null", `Quick, test_nan_float_is_null;
    "Infinity float serializes to null", `Quick, test_infinity_float_is_null;
    "finite float stays a number", `Quick, test_finite_float_is_number;
  ]

let () = Alcotest.run "Json renderer detail levels" [ "json_renderer", tests ]
