(* 182840c narrowing: attributes irrelevant to patch semantics (deprecated,
   warning, ocaml-dot attributes) must NOT be rejected on constructors. *)
type t =
  | Old [@deprecated "use New"]
  | New [@ocaml.warning "-32"]
[@@deriving patch]
