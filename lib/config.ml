(* TODO: under construction *)
type config = {
  details_level : int;                  (** 1-3, set it to 0 will not generate the diff summaries *)
  time_or_position : bool;
} [@@deriving jsonschema, yojson]
