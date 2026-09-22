type t = A of int | B of int
[@@deriving eq, view_spec]
[@@view.variant_dispatch "DTOther" (
    "A", "M", "APatch", "build_a";
    "B", "", "BPatch", "build_b";
  )]
