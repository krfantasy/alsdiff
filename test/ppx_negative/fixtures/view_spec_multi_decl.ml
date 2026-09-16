(* BUG-15 / 5364b2a: an and-group would collide on the generated ViewSpec
   module name; reject instead of silently deriving the first type only. *)
type a = { x : int } and b = { y : int } [@@deriving view_spec]
