(** The role a field plays in the diff view. [Content] is the change itself;
    [Context] is unchanged presentation state materialized from the old
    document; [Identity] is a structural join key (TrackId/GroupId).
    Context and Identity ride at every detail level; Content never does.
    See docs/adr/0001-unchanged-context-in-diff-output.md. *)
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
}

and view =
  | Field of field
  | Item of item
  | Collection of collection
