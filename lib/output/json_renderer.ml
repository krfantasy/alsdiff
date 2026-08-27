open Output_types
open Presentation_model
open Config

let field_value_to_yojson (v : field_value) : Yojson.Safe.t =
  match v with
  | Fint i -> `Int i
  (* Ffloat NaN/Infinity would make Yojson.Safe.to_string raise
     "NaN value not allowed in standard JSON", crashing the whole diff
     command on a single bad automation value from a corrupt .als. RFC 8259
     forbids NaN/Inf; emit `Null instead so the document stays parseable by
     strict consumers (jq, Python json.loads). Matches the convention in
     seconds_to_mmssms (track.ml). See review_0614.org
     "Ffloat NaN/Inf serializes to invalid JSON". *)
  | Ffloat f ->
    if Float.is_nan f || Float.is_infinite f then `Null
    else `Float f
  | Fbool b -> `Bool b
  | Fstring s -> `String s

let change_type_to_string = function
  | Unchanged -> "Unchanged"
  | Added -> "Added"
  | Removed -> "Removed"
  | Modified -> "Modified"


let rec item_to_yojson (cfg : detail_config) (item : item) : Yojson.Safe.t option =
  let level = get_effective_detail cfg item.change item.domain_type in
  let base = [
    ("type", `String "item");
    ("name", `String item.name);
    ("change", `String (change_type_to_string item.change));
    ("domain_type", `String (domain_type_to_string item.domain_type));
  ] in
  (* Omit the "children" key when no children render, keeping output minimal. *)
  let node children =
    if children = [] then `Assoc base
    else `Assoc (base @ [("children", `List children)])
  in
  (* TrackId/GroupId are structural identity, not diff content: consumers (the
     web app) nest tracks under their group by these fields, so they ride along
     at every detail level — including counts-only Summary and Field-dropping
     Compact — exactly once each, even where the normal path would render them.
     The LiveSet-level Tempo/Time Signature context fields (the project's
     current tempo/time signature, emitted by create_liveset_item) ride the
     same channel: the web app's realtime ruler needs them even when the
     whole MainTrack item is level-dropped under Summary/Compact presets. *)
  let is_context_field (f : field) =
    match item.domain_type with
    | DTTrack -> f.name = "TrackId" || f.name = "GroupId"
    | DTLiveset -> f.name = "Tempo" || f.name = "Time Signature"
    | _ -> false
  in
  let identity =
    List.filter_map (function
        | Field f when is_context_field f -> Some (field_to_yojson f)
        | _ -> None) item.children
  in
  let sub_views = List.filter (function
      | Field f -> not (is_context_field f)
      | _ -> true) item.children in
  match level with
  | Ignore -> None
  | Summary ->
    (* Summary mode: change counts, no children. LiveSet is the root container,
       so it shows its sub-views even in Summary (mirrors text_renderer.ml:162-174);
       its context fields ride along like the track identity fields. *)
    if item.domain_type = DTLiveset then
      Some (node (identity @ List.filter_map (view_to_yojson cfg) sub_views))
    else begin
      let breakdown =
        if is_element_like_item cfg item then count_fields_breakdown item
        else count_sub_views_breakdown cfg item
      in
      let with_counts = base @ [("counts", change_breakdown_to_yojson breakdown)] in
      if identity = [] then Some (`Assoc with_counts)
      else Some (`Assoc (with_counts @ [("children", `List identity)]))
    end
  | Compact ->
    (* Compact mode: Item/Collection sub-views only, no Fields (mirrors
       text_renderer.ml:200-206). Element-like items have only Fields, so this
       yields no children for them — track identity fields are the exception. *)
    let children =
      identity
      @ List.filter_map
        (fun (v : view) ->
           match v with
           | Item _ | Collection _ -> view_to_yojson cfg v
           | Field _ -> None)
        sub_views
    in
    Some (node children)
  | Inline | Full ->
    (* Inline/Full mode: all sub-views, each rendered at its own level. This
       reproduces the original behavior, so Full/verbose output stays identical. *)
    Some (node (identity @ List.filter_map (view_to_yojson cfg) sub_views))

and collection_to_yojson (cfg : detail_config) (col : collection) : Yojson.Safe.t option =
  let level = get_effective_detail cfg col.change col.domain_type in
  let base = [
    ("type", `String "collection");
    ("name", `String col.name);
    ("change", `String (change_type_to_string col.change));
    ("domain_type", `String (domain_type_to_string col.domain_type));
  ] in
  match level with
  | Ignore -> None
  | Summary | Compact ->
    (* Summary/Compact: header + counts, no element list
       (Compact == Summary for collections, per [detail_level] doc). *)
    let breakdown = count_elements_breakdown cfg col in
    Some (`Assoc (base @ [("counts", change_breakdown_to_yojson breakdown)]))
  | Inline | Full ->
    (* Inline/Full: list elements, truncated by [max_collection_items]. *)
    let filtered, truncation_info = filter_collection_elements_with_info cfg col in
    if filtered = [] then None
    else
      let items = List.filter_map (item_to_yojson cfg) filtered in
      let with_items = base @ [("items", `List items)] in
      let with_truncation =
        match truncation_info with
        | None -> with_items
        | Some info ->
          let breakdown = info.truncated_breakdown in
          let truncated_obj = [
            ("added", `Int breakdown.added);
            ("removed", `Int breakdown.removed);
            ("modified", `Int breakdown.modified);
          ] in
          with_items @ [
            ("total", `Int info.total);
            ("displayed", `Int info.displayed);
            ("truncated", `Assoc truncated_obj);
          ]
      in
      Some (`Assoc with_truncation)

and view_to_yojson (cfg : detail_config) (view : view) : Yojson.Safe.t option =
  match view with
  | Field f ->
    let level = get_effective_detail cfg f.change f.domain_type in
    if not (should_render_level level) then None
    else Some (field_to_yojson f)
  | Item item -> item_to_yojson cfg item
  | Collection col -> collection_to_yojson cfg col

(** [field_to_yojson] serializes a field view without any detail-level gate.
    Only for views that must render unconditionally (track identity fields). *)
and field_to_yojson (f : field) : Yojson.Safe.t =
  let base = [
    ("type", `String "field");
    ("name", `String f.name);
    ("change", `String (change_type_to_string f.change));
    ("domain_type", `String (domain_type_to_string f.domain_type));
  ] in
  let with_old = match f.oldval with
    | None -> base
    | Some v -> base @ [("old_value", field_value_to_yojson v)]
  in
  match f.newval with
  | None -> `Assoc with_old
  | Some v -> `Assoc (with_old @ [("new_value", field_value_to_yojson v)])

let render (cfg : detail_config) (views : view list) : string =
  let entries = List.filter_map (view_to_yojson cfg) views in
  let json = `Assoc [("diff", `List entries)] in
  Yojson.Safe.pretty_to_string json
