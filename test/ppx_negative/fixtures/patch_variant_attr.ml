(* 9d712d6: the variant patch generator ignores constructor field attributes;
   reject instead of generating a contradictory patch. *)
type t =
  | A [@id.id]
  | B
[@@deriving patch]
