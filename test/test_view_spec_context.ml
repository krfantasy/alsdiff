open Alsdiff_base.Diff
open Alsdiff_output.View_model

(* PPX-generated fill_context (TODO item 4): mini types mirror the real
   shapes — Leaf (like TimeSignature), Mid (like Mixer: marked children,
   recursion target), Top (like MidiClip: marked child + unmarked child +
   builder collection). Names/labels flow from the same generated specs the
   fill matches on — no string coupling. *)

module Leaf = struct
  (* id like MidiNote: the collection diff (diff_list_id) needs IDENTIFIABLE *)
  type t = {
    id : int; [@id.id] [@patch.identity] [@view.skip]
    amt : float; [@view.label "Amt"]
  } [@@deriving eq, id, patch, view_spec] [@@patch.generate_diff]
end

module Mid = struct
  type t = {
    one : Leaf.t; [@view.child "DTOther"] [@view.label "One"] [@view.context]
    two : Leaf.t; [@view.child "DTOther"] [@view.label "Two"] [@view.context]
  } [@@deriving eq, patch, view_spec] [@@patch.generate_diff]
end

module Top = struct
  type t = {
    marked : Mid.t;    [@view.child "DTOther"] [@view.label "Marked"] [@view.context]
    unmarked : Leaf.t; [@view.child "DTOther"] [@view.label "Unmarked"]
    leaves : Leaf.t list; [@view.collection "DTOther"] [@view.builder "build_leaves"]
  } [@@deriving eq, patch, view_spec] [@@patch.generate_diff]
end

module LeafVS = Leaf.ViewSpec(DeviceViewSpecB)
module MidVS = Mid.ViewSpec(DeviceViewSpecB)
module TopVS = Top.ViewSpec(DeviceViewSpecB)

let fmt = default_dual_time_formatter

let leaf id amt = { Leaf.id; amt }

(* find_view_by_name-style helpers over the local view type *)
let find_item name views =
  List.find_opt (function Item { name = n; _ } -> n = name | _ -> false) views

let find_field name views =
  List.find_opt (function Field { name = n; _ } -> n = name | _ -> false) views

let must_find name views =
  match find_item name views with
  | Some (Item i) -> i
  | _ -> Alcotest.fail ("missing " ^ name)

(* diff_complex_value returns a structured_update; these fixtures always
   differ, so destructure the `Modified case (same first-class-module
   pattern as Clip.AudioClip.diff's (module Loop) calls). *)
let modified_patch (type v) (module M : DIFFABLE_EQ with type t = v) old_v new_v =
  match diff_complex_value (module M) old_v new_v with
  | `Modified p -> p
  | `Unchanged -> Alcotest.fail "expected a Modified patch"

(* Top with marked Mid unchanged (only the collection changed): the Marked
   placeholder is whole-rebuilt from the old value; Unmarked stays empty. *)
let test_top_placeholder_rebuilt_unmarked_untouched () =
  let old_v =
    { Top.marked = { Mid.one = leaf 1 1.0; two = leaf 2 2.0 };
      unmarked = leaf 3 3.0; leaves = [] }
  in
  let new_v = { old_v with Top.leaves = [ leaf 4 4.0 ] } in
  let patch = modified_patch (module Top) old_v new_v in
  let build_leaves c =
    (* name by id, as the real projector does for clips — this also reads
       Leaf.Patch.id / Leaf.id, keeping warning 69 quiet *)
    let id = match c with
      | `Added v | `Removed v -> v.Leaf.id
      | `Modified p -> p.Leaf.Patch.id
      | `Unchanged -> -1
    in
    LeafVS.build_item ~format_time:fmt ~domain_type:DTOther
      ~name:(Printf.sprintf "Leaf#%d" id) c in
  let item =
    TopVS.build_item ~format_time:fmt ~build_leaves ~domain_type:DTOther
      ~name:"Top" (`Modified patch)
  in
  let filled = TopVS.fill_context ~format_time:fmt old_v item in
  let marked = must_find "Marked" filled.children in
  (match find_item "One" marked.children, find_item "Two" marked.children with
   | Some (Item one), Some (Item two) ->
     (match find_field "Amt" one.children, find_field "Amt" two.children with
      | Some (Field { change = Unchanged; newval = Some (Ffloat 1.0); _ }),
        Some (Field { change = Unchanged; newval = Some (Ffloat 2.0); _ }) -> ()
      | _ -> Alcotest.fail "One/Two rebuilt from old values, restamped Unchanged")
   | _ -> Alcotest.fail "Marked rebuilt with One/Two children");
  Alcotest.(check bool) "unmarked placeholder stays empty" true
    ((must_find "Unmarked" filled.children).children = [])

(* Mid partially changed: One Modified keeps its patch emission, Two's empty
   placeholder is filled from the old value. *)
let test_mid_modified_recursion_within_child () =
  let old_v = { Mid.one = leaf 1 1.0; two = leaf 2 2.0 } in
  let new_v = { Mid.one = leaf 1 9.0; two = leaf 2 2.0 } in
  let patch = modified_patch (module Mid) old_v new_v in
  let item =
    MidVS.build_item ~format_time:fmt ~domain_type:DTOther ~name:"Mid"
      (`Modified patch)
  in
  let filled = MidVS.fill_context ~format_time:fmt old_v item in
  let one = must_find "One" filled.children in
  Alcotest.(check bool) "Modified One untouched by fill" true
    (match find_field "Amt" one.children with
     | Some (Field { change = Modified; _ }) -> true
     | _ -> false);
  let two = must_find "Two" filled.children in
  (match find_field "Amt" two.children with
   | Some (Field { change = Unchanged; newval = Some (Ffloat 2.0); _ }) -> ()
   | _ -> Alcotest.fail "Two filled from old value")

(* Unmarked types generate the S-conformant no-op. *)
let test_unmarked_type_noop () =
  let old_v = leaf 1 1.0 in
  (* read the id once: it exists for the collection diff, not the view *)
  Alcotest.(check int) "leaf id" 1 old_v.Leaf.id;
  let item = { name = "Leaf"; change = Modified; domain_type = DTOther; children = [] } in
  Alcotest.(check bool) "unmarked type: fill_context is identity" true
    (LeafVS.fill_context ~format_time:fmt old_v item = item)

let () =
  Alcotest.run "ViewSpecContext" [
    "fill_context", [
      Alcotest.test_case "Top placeholder rebuilt, unmarked untouched" `Quick
        test_top_placeholder_rebuilt_unmarked_untouched;
      Alcotest.test_case "Mid modified: One kept, Two filled" `Quick
        test_mid_modified_recursion_within_child;
      Alcotest.test_case "unmarked type: no-op fill" `Quick test_unmarked_type_noop;
    ];
  ]
