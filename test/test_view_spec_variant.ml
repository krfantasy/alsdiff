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

(* Builder-routed sum (Track shape): routing table carries builder labels;
   a patch-less entry (Return shape) and a skip entry (Main shape). *)
module BVSum = struct
  module Patch = struct
    type t = APatch of SVa.Patch.t | BPatch of SVb.Patch.t | ZPatch of SVb.Patch.t
  end
  type t = A of SVa.t | B of SVb.t | Nop of SVb.t | Zed of SVa.t
  [@@deriving eq, view_spec]
  [@@view.variant_dispatch "DTOther" (
      "A", "SVa", "APatch", "build_a";
      "B", "SVb", "BPatch", "build_b";
      "Nop", "SVb", "", "build_nop";
      "Zed", "", "ZPatch", "";
    )]
end

module BVSumVS = BVSum.ViewSpec (DeviceViewSpecB)

let test_builder_routed_dispatch_and_retag () =
  (* builders record the (label, change kind, payload id) they received *)
  let calls = ref [] in
  let tag c = match c with
    | `Added _ -> "added" | `Removed _ -> "removed"
    | `Modified _ -> "modified" | `Unchanged -> "unchanged"
  in
  let build_a c =
    (match c with
     | `Added v | `Removed v -> calls := ("a", tag c, v.SVa.id) :: !calls
     | `Modified _ -> calls := ("a", "modified", -1) :: !calls
     | `Unchanged -> calls := ("a", "unchanged", -1) :: !calls);
    { name = "a"; change = Unchanged; domain_type = DTOther; children = [] }
  in
  let build_b c = ignore c; { name = "b"; change = Unchanged; domain_type = DTOther; children = [] } in
  let build_nop c = ignore c; { name = "nop"; change = Unchanged; domain_type = DTOther; children = [] } in
  let built = BVSumVS.build_item ~build_a ~build_b ~build_nop (`Added (BVSum.A (va 7 1.0))) in
  (match built with
   | Some item -> Alcotest.(check string) "routed to build_a" "a" item.name
   | None -> Alcotest.fail "expected Some");
  Alcotest.(check string) "retag recorded" "added"
    (match !calls with
     | [ ("a", kind, 7) ] -> kind
     | _ -> "BAD");
  (* second routed entry: Added and Modified both route through build_b
     (also builds B/BPatch so warning 37 sees them constructed) *)
  let patch_b = match diff_complex_value (module SVb) (vb 3 1.0) (vb 3 2.0) with
    | `Modified p -> BVSum.Patch.BPatch p
    | `Unchanged -> Alcotest.fail "expected Modified"
  in
  (match BVSumVS.build_item ~build_a ~build_b ~build_nop (`Added (BVSum.B (vb 3 1.0))) with
   | Some item -> Alcotest.(check string) "second ctor routes to build_b" "b" item.name
   | None -> Alcotest.fail "expected Some");
  (match BVSumVS.build_item ~build_a ~build_b ~build_nop (`Modified patch_b) with
   | Some item -> Alcotest.(check string) "modified routes to build_b" "b" item.name
   | None -> Alcotest.fail "expected Some");
  (* routed Modified through build_a records the modified retag *)
  let patch_a = match diff_complex_value (module SVa) (va 7 1.0) (va 7 2.0) with
    | `Modified p -> BVSum.Patch.APatch p
    | `Unchanged -> Alcotest.fail "expected Modified"
  in
  (match BVSumVS.build_item ~build_a ~build_b ~build_nop (`Modified patch_a) with
   | Some item -> Alcotest.(check string) "modified routes to build_a" "a" item.name
   | None -> Alcotest.fail "expected Some");
  Alcotest.(check string) "modified retag recorded" "modified"
    (match !calls with
     | ("a", "modified", -1) :: _ -> "modified"
     | _ -> "BAD")

let test_builder_routed_skip_and_unchanged_none () =
  let any = { name = "x"; change = Unchanged; domain_type = DTOther; children = [] } in
  let b = BVSumVS.build_item
      ~build_a:(Fun.const any) ~build_b:(Fun.const any) ~build_nop:(Fun.const any) in
  (* a real ZPatch payload, like the self-routed test builds *)
  let zpatch = match diff_complex_value (module SVb) (vb 1 1.0) (vb 1 2.0) with
    | `Modified p -> BVSum.Patch.ZPatch p
    | `Unchanged -> Alcotest.fail "expected Modified"
  in
  Alcotest.(check bool) "skip ctor -> None" true
    (b (`Added (BVSum.Zed (va 1 1.0))) = None);
  Alcotest.(check bool) "skip patch -> None" true (b (`Modified zpatch) = None);
  Alcotest.(check bool) "Unchanged -> None" true (b `Unchanged = None);
  Alcotest.(check bool) "patch-less ctor still routes Added" true
    (b (`Added (BVSum.Nop (vb 2 2.0))) <> None)

let () =
  Alcotest.run "view_spec_variant" [
    ("self_routed", [
        Alcotest.test_case "added" `Quick test_self_routed_added;
        Alcotest.test_case "modified" `Quick test_self_routed_modified_routes_by_patch_ctor;
        Alcotest.test_case "unchanged" `Quick test_self_routed_unchanged_placeholder;
      ]);
    ("builder_routed", [
        Alcotest.test_case "dispatch and retag" `Quick test_builder_routed_dispatch_and_retag;
        Alcotest.test_case "skip and unchanged none" `Quick test_builder_routed_skip_and_unchanged_none;
      ]);
  ]
