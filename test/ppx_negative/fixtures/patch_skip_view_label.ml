(* BUG-16 / 44b9f78: patch.skip removes the field from Patch.t, so a view
   accessor (view.label et al.) cannot be generated — reject up front. *)
type t = {
  id : int; [@id.id] [@patch.identity] [@view.skip]
  hidden : int; [@patch.skip] [@view.label "Hidden"]
}
[@@deriving id, patch, view_spec]
