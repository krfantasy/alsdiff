(* [@view.context] generates a section-placeholder fill, so it is only
   supported on [@view.child] / [@view.optional_child] fields; an inline
   atomic field has no placeholder to fill. *)
type t = { x : int [@view.context] } [@@deriving view_spec]
