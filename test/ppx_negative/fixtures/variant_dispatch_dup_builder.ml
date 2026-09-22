type t = A of int | B of int | C of int
[@@deriving eq, view_spec]
[@@view.variant_dispatch "DTOther" (
    "A", "M", "APatch", "build_a";
    "B", "M", "BPatch", "build_a";
    "C", "", "CPatch", "";
  )]
