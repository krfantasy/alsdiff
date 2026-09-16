open Alsdiff_base.Diff
open Alsdiff_output.View_model

(* Compile-time regression (BUG-14): [@view.inline_child] on a child whose
   ViewSpec is PPX-generated must typecheck. The splice used to pass the
   child's unapplied `field_specs` function where `B.map_specs` requires the
   spliced list, which only compiled by accident for hand-written ViewSpecs
   (GenericParam) whose field_specs is a plain value. *)

module Inner = struct
  type t = {
    amount : float;  [@view.label "Amount"]
  } [@@deriving eq, patch, view_spec] [@@patch.generate_diff]
end

module Outer = struct
  type t = {
    name : string;         [@view.label "Name"]
    base : Inner.t;        [@view.inline_child]
  } [@@deriving eq, patch, view_spec] [@@patch.generate_diff]
end

(* Applying the functor elaborates the spliced field list. *)
module OuterVS = Outer.ViewSpec(DeviceViewSpecB)

let test_inline_child_field_specs () =
  let specs = OuterVS.field_specs ~format_time:default_dual_time_formatter in
  (* Outer's own "Name" field + Inner's "Amount" field spliced in. *)
  Alcotest.(check int) "inline child contributes its field specs" 2 (List.length specs)

let () =
  Alcotest.run "ViewSpecInlineChild" [
    "inline_child", [
      Alcotest.test_case "PPX-generated child splices field specs"
        `Quick test_inline_child_field_specs;
    ];
  ]
