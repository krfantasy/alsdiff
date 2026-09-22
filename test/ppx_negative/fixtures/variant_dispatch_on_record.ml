type t = { a : int; [@view.label "A"] }
[@@deriving eq, view_spec]
[@@view.variant_dispatch "DTOther" ("A", "", "", "")]
