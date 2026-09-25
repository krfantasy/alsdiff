(** The role a field plays in the diff view — its riding policy at detail
    levels, not its provenance: [Context] and [Identity] fields ride at
    every detail level (including counts-only Summary and Field-dropping
    Compact), exactly once each; [Content] fields obey the normal level
    gates. Today the projector stamps as riders exactly the identity join
    keys (TrackId/GroupId, including the re-stamped value-side fields on
    Added/Removed tracks) and the LiveSet tempo/time-signature fields.
    Other unchanged material re-attached from the old document (mixer
    strips, note pitch, clip sections) is deliberately [Content]: it is
    presentation for the changed node, not a rider, and must still drop at
    counts-only levels. See docs/adr/0001-unchanged-context-in-diff-output.md. *)
type field_kind = Content | Context | Identity

type field = {
  name : string;
  change : Output_types.change_type;
  domain_type : Output_types.domain_type;
  kind : field_kind;
  oldval : Output_types.field_value option;
  newval : Output_types.field_value option;
}

and item = {
  name : string;
  change : Output_types.change_type;
  domain_type : Output_types.domain_type;
  children : view list;
}

and collection = {
  name : string;
  change : Output_types.change_type;
  domain_type : Output_types.domain_type;
  items : view list;
  truncatable : bool;
  (** [false] exempts this collection from the [max_collection_items] cap
      (Config.filter_collection_elements_with_info); detail-level filtering
      still applies. For structural collections whose every element must
      render; ordinary content collections (Notes/Events/Clips/Devices/...)
      keep [true]. No production collection opts out yet — all sites pass
      the [true] default. The intended first consumer is emitting
      tracks/returns as real Collections instead of flat LiveSet children
      (which exist precisely to bypass the cap); migration recipe: wrap the
      build_liveset_tracks_items/build_liveset_returns_items output in
      [Collection { truncatable = false; ... }] only after Summary/Compact
      collections learn to render their items and the web's extractTracks
      (web/src/lib/diff-parser.ts, reads direct item children only) learns
      the wrapper — otherwise Summary users see an empty track list. *)
}

and view =
  | Field of field
  | Item of item
  | Collection of collection
