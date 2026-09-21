[@@@warning "-unused-var"]
open Alsdiff_base.Diff

module type S = sig
  type domain_type
  type change_type =
    | Unchanged
    | Added
    | Removed
    | Modified
  type field_value =
    | Fint of int
    | Ffloat of float
    | Fbool of bool
    | Fstring of string

  and view
  and item
  and collection

  and dual_time_formatter = {
    format_old : float -> field_value;
    format_new : float -> field_value;
  }

  type ('v, 'p) unified_field_spec
  type ('v, 'p) section_spec

  val int_value : int -> field_value
  val float_value : float -> field_value
  val bool_value : bool -> field_value
  val string_value : string -> field_value
  val default_domain_type : domain_type
  val domain_type_of_name : string -> domain_type
  val format_unix_timestamp : int -> string

  val make_spec :
    ('a -> field_value) -> string -> ('v -> 'a) -> ('p -> 'a atomic_update) ->
    ('v, 'p) unified_field_spec
  val make_spec_const :
    ('a -> field_value) -> string -> ('v -> 'a) ->
    ('v, 'p) unified_field_spec
  val make_int : string -> ('v -> int) -> ('p -> int atomic_update) -> ('v, 'p) unified_field_spec
  val make_float : string -> ('v -> float) -> ('p -> float atomic_update) -> ('v, 'p) unified_field_spec
  val make_string : string -> ('v -> string) -> ('p -> string atomic_update) -> ('v, 'p) unified_field_spec
  val make_bool : string -> ('v -> bool) -> ('p -> bool atomic_update) -> ('v, 'p) unified_field_spec
  val make_time_field :
    dual_time_formatter -> string -> ('v -> float) -> ('p -> float atomic_update) ->
    ('v, 'p) unified_field_spec

  val build_value_field_views :
    ('v, 'p) unified_field_spec list -> change_type -> 'v -> domain_type:domain_type -> view list
  val build_patch_field_views :
    ('v, 'p) unified_field_spec list -> 'p -> domain_type:domain_type -> view list

  (** [item_children item] returns the children views of an item. Exposed so
      generated [@view.child] specs can extract children from a child type's
      full [build_item] (which renders nested children, not just inline). *)
  val item_children : item -> view list

  (** [build_value_children] / [build_patch_children] render the FULL child
      section (all sub-views via [section_specs]), not just inline atomic
      fields. Used by generated [@view.child] specs so a child whose own
      fields are nested [@view.child] (e.g. Mixer -> GenericParam) renders its
      whole subtree. Leaf types delegate to [build_value_field_views]/
      [build_patch_field_views] (their inline fields ARE their full content). *)
  val build_value_children :
    format_time:dual_time_formatter -> ?domain_type:domain_type -> change_type -> 'v -> view list
  val build_patch_children :
    format_time:dual_time_formatter -> ?domain_type:domain_type -> 'p -> view list


  val map_specs :
    ('v2 -> 'v1) ->
    ('p2 -> 'p1 structured_update) ->
    ('v1, 'p1) unified_field_spec list ->
    ('v2, 'p2) unified_field_spec list

  val build_item_from_specs :
    name:string -> domain_type:domain_type -> specs:('v, 'p) section_spec list ->
    ('v, 'p) structured_change -> item

  (** [fill_context] fills a Modified item's unchanged context from the old
      (pre-change) value: child sections marked [@view.context] are rebuilt
      from the old value when the patch emitted them as empty Unchanged
      placeholders, and a Modified marked child recurses through the child
      type's own fill. The B-side default is a no-op; only context-marked
      types generate a real fill (TODO item 4). *)
  val fill_context : format_time:dual_time_formatter -> 'a -> item -> item

  (** [fill_section_context specs old item] applies each spec's context fill
      to [item]'s children — the body of a generated [fill_context]. *)
  val fill_section_context : ('v, 'p) section_spec list -> 'v -> item -> item

  module Spec : sig
    val inline_fields :
      specs:('v, 'p) unified_field_spec list -> domain_type:domain_type -> ('v, 'p) section_spec
    val child :
      name:string ->
      of_value:('parent -> 'nested) ->
      of_patch:('patch -> 'np structured_update) ->
      build_value_children:(change_type -> 'nested -> view list) ->
      build_patch_children:('np -> view list) ->
      domain_type:domain_type ->
      ('parent, 'patch) section_spec
    val child_optional :
      name:string ->
      of_value:('parent -> 'nested option) ->
      of_patch:('patch -> ('nested, 'np) structured_change) ->
      build_value_children:(change_type -> 'nested -> view list) ->
      build_patch_children:('np -> view list) ->
      domain_type:domain_type ->
      ('parent, 'patch) section_spec
    (* [child_with_context] is [child] for a context-marked field
       ([@view.context]): the mandatory [~context] callback (the child
       type's [fill_context], format_time pre-applied) fills empty
       Unchanged placeholders from the old parent value and recurses into
       Modified children. Mandatory rather than optional so existing
       [child] applications stay total — an optional argument followed only
       by labelled arguments cannot be erased. (Plain comment: docstrings
       cannot attach inside a nested module-type sig — warning 50.) *)
    val child_with_context :
      context:('nested -> item -> item) ->
      name:string ->
      of_value:('parent -> 'nested) ->
      of_patch:('patch -> 'np structured_update) ->
      build_value_children:(change_type -> 'nested -> view list) ->
      build_patch_children:('np -> view list) ->
      domain_type:domain_type ->
      ('parent, 'patch) section_spec
    (* [child_optional_with_context] is [child_optional] for a
       context-marked field; see [child_with_context]. *)
    val child_optional_with_context :
      context:('nested -> item -> item) ->
      name:string ->
      of_value:('parent -> 'nested option) ->
      of_patch:('patch -> ('nested, 'np) structured_change) ->
      build_value_children:(change_type -> 'nested -> view list) ->
      build_patch_children:('np -> view list) ->
      domain_type:domain_type ->
      ('parent, 'patch) section_spec
    val collection :
      name:string ->
      of_value:('parent -> 'elem list) ->
      of_patch:('patch -> ('elem, 'ep) structured_change list) ->
      build_item:(('elem, 'ep) structured_change -> item) ->
      domain_type:domain_type ->
      ('parent, 'patch) section_spec
  end
end
