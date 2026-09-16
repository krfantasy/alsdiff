(* Positive control: the shapes used by lib/live must keep expanding cleanly. *)
type t = {
  id : int; [@id.id] [@patch.identity] [@view.skip]
  destination : int; [@view.label "Destination"]
  amount : float;
}
[@@deriving id, patch, view_spec] [@@patch.generate_diff]
