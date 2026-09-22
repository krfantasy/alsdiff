type t = A of int | B of int
[@@deriving eq, view_spec]
[@@view.type_label "T"]
[@@view.variant_dispatch "DTOther" (
    "A", "M", "APatch", "";
    "B", "", "BPatch", "";
  )]
