open Alsdiff_base.Diff
open Alsdiff_output.View_model

(* Mini types mirror the real sum shapes: SVa/SVb are record types with
   ViewSpecs (naming attrs so build_section_name is generated); SVSum is a
   self-routed sum (Device shape). *)

module SVa = struct
  type t = {
    id : int;               [@id.id] [@patch.identity] [@view.const] [@view.label "Id"]
    name : string;          [@view.name] [@view.skip]
    current_name : string;  [@patch.identity] [@view.name_patch]
    amt : float;            [@view.label "Amt"]
  } [@@deriving eq, id, patch, view_spec] [@@patch.generate_diff] [@@view.type_label "SVa"]
end

module SVb = struct
  type t = {
    id : int;               [@id.id] [@patch.identity] [@view.const] [@view.label "Id"]
    name : string;          [@view.name] [@view.skip]
    current_name : string;  [@patch.identity] [@view.name_patch]
    amt : float;            [@view.label "Amt"]
  } [@@deriving eq, id, patch, view_spec] [@@patch.generate_diff] [@@view.type_label "SVb"]
end

module SVSum = struct
  (* Patch before the sum: the generated ViewSpec matches on Patch constructors,
     so it must be in scope at the generation point. *)
  module Patch = struct
    type t = APatch of SVa.Patch.t | BPatch of SVb.Patch.t
  end
  type t = A of SVa.t | B of SVb.t
  [@@deriving eq, view_spec]
  [@@view.type_label "SVSum"]
  [@@view.variant_dispatch "DTOther" (
      "A", "SVa", "APatch", "";
      "B", "SVb", "BPatch", "";
    )]
end

module SVSumVS = SVSum.ViewSpec (DeviceViewSpecB)

let fmt = default_dual_time_formatter
let va id amt = { SVa.id; name = "a" ^ string_of_int id; current_name = "a"; amt }
let vb id amt = { SVb.id; name = "b" ^ string_of_int id; current_name = "b"; amt }

let test_self_routed_added () =
  let item = SVSumVS.build_item ~format_time:fmt (`Added (SVSum.A (va 1 5.0))) in
  Alcotest.(check string) "name from variant build_section_name" "SVa (#1): a1" item.name;
  Alcotest.(check bool) "change is Added" true (item.change = Added);
  Alcotest.(check bool) "domain from table" true (item.domain_type = DTOther);
  (* Second entry routes through its own variant module *)
  let item_b = SVSumVS.build_item ~format_time:fmt (`Added (SVSum.B (vb 2 7.0))) in
  Alcotest.(check string) "second ctor routes to SVb VS" "SVb (#2): b2" item_b.name

let test_self_routed_modified_routes_by_patch_ctor () =
  let patch = match diff_complex_value (module SVa) (va 1 5.0) (va 1 6.0) with
    | `Modified p -> p
    | `Unchanged -> Alcotest.fail "expected Modified"
  in
  let item = SVSumVS.build_item ~format_time:fmt (`Modified (SVSum.Patch.APatch patch)) in
  Alcotest.(check string) "modified routes to SVa VS" "SVa (#1): a" item.name;
  Alcotest.(check bool) "change is Modified" true (item.change = Modified);
  (* Second entry's patch ctor routes through SVb's ViewSpec *)
  let patch_b = match diff_complex_value (module SVb) (vb 9 1.0) (vb 9 2.0) with
    | `Modified p -> p
    | `Unchanged -> Alcotest.fail "expected Modified"
  in
  let item_b = SVSumVS.build_item ~format_time:fmt (`Modified (SVSum.Patch.BPatch patch_b)) in
  Alcotest.(check string) "modified routes to SVb VS" "SVb (#9): b" item_b.name

let test_self_routed_unchanged_placeholder () =
  let item = SVSumVS.build_item ~format_time:fmt `Unchanged in
  Alcotest.(check string) "placeholder name from type_label" "SVSum" item.name;
  Alcotest.(check bool) "change is Unchanged" true (item.change = Unchanged);
  Alcotest.(check bool) "no children" true (item.children = [])

let () =
  Alcotest.run "view_spec_variant" [
    ("self_routed", [
        Alcotest.test_case "added" `Quick test_self_routed_added;
        Alcotest.test_case "modified" `Quick test_self_routed_modified_routes_by_patch_ctor;
        Alcotest.test_case "unchanged" `Quick test_self_routed_unchanged_placeholder;
      ]);
  ]
