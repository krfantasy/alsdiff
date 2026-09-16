(* BUG-15 / 5364b2a: deriving view_spec on a variant must be rejected loudly
   instead of silently emitting an empty ViewSpec. *)
type t = A | B [@@deriving view_spec]
