open Alsdiff_live
open Alsdiff_base.Diff
open Output_types
open Display_context
open Presentation_model

(** [opt_view wrap x] lifts an option into a 0- or 1-element [view list], wrapping the
    value with [wrap] (e.g. [Item] or [Collection]). *)
let opt_view (wrap : 'a -> view) (x : 'a option) : view list =
  Option.to_list (Option.map wrap x)

(** [track_id_of t] returns the identity id of a track value (0 for Main). *)
let track_id_of = function
  | Track.Midi t -> t.Track.MidiTrack.id
  | Track.Audio t | Track.Group t | Track.Return t -> t.Track.AudioTrack.id
  | Track.Main _ -> 0

(** [patch_track_id_of p] returns the identity id of a track patch (0 for MainPatch). *)
let patch_track_id_of = function
  | Track.Patch.MidiPatch p -> p.Track.MidiTrack.Patch.id
  | Track.Patch.AudioPatch p | Track.Patch.GroupPatch p -> p.Track.AudioTrack.Patch.id
  | Track.Patch.MainPatch _ -> 0


(** ViewBuilder module - uses the unified 3-type system (Field, Item, Collection) *)
module ViewBuilder = struct

  (** [change_type_of c] extracts the change type from a structured change. *)
  let change_type_of (c : ('a, 'p) structured_change) : change_type =
    match c with
    | `Added _ -> Added
    | `Removed _ -> Removed
    | `Modified _ -> Modified
    | `Unchanged -> Unchanged

  let map_atomic_update (f : 'a -> 'b) (u : 'a atomic_update) : 'b atomic_update =
    match u with
    | `Modified { oldval; newval } -> `Modified { oldval = f oldval; newval = f newval }
    | `Unchanged -> `Unchanged

  let map_atomic_update_dual (f_old : 'a -> 'b) (f_new : 'a -> 'b) (u : 'a atomic_update) : 'b atomic_update =
    match u with
    | `Modified { oldval; newval } -> `Modified { oldval = f_old oldval; newval = f_new newval }
    | `Unchanged -> `Unchanged


  (** [build_item_from_children c ~name ~of_value ~of_patch ~build_value_children ~build_patch_children]
      builds a [item] with arbitrary children for a nested structured type.

      This is the new equivalent of [build_nested_section_view], but returns a [item] instead
      of [section_view], and can contain any [new_view] children (not just [view]).

      @param c the parent structured change
      @param name the item name
      @param of_value extracts the nested value from the parent value
      @param of_patch extracts the nested update from the parent patch
      @param build_value_children builds view list from nested value and change type
      @param build_patch_children builds view list from nested patch
      @param domain_type the domain type for this item
      @return Some item if there are children, None otherwise
  *)
  let build_item_from_children
      (c : ('parent, 'pp) structured_change)
      ~(name : string)
      ~(of_value : 'parent -> 'nested)
      ~(of_patch : 'pp -> 'np structured_update)
      ~(build_value_children : change_type -> 'nested -> view list)
      ~(build_patch_children : 'np -> view list)
      ~(domain_type : domain_type)
    : item option =
    match c with
    | `Added parent ->
      let nested_val = of_value parent in
      let children = build_value_children Added nested_val in
      if children = [] then None
      else Some { name; change = Added; domain_type; children }
    | `Removed parent ->
      let nested_val = of_value parent in
      let children = build_value_children Removed nested_val in
      if children = [] then None
      else Some { name; change = Removed; domain_type; children }
    | `Modified patch ->
      (match of_patch patch with
       | `Unchanged ->
         (* Nested child is unchanged but the parent changed. Emit a placeholder
            so renderers that want to show unchanged context (JSON/web, verbose)
            can do so. The TEXT renderer suppresses bare Unchanged headers with
            no renderable children (see text_renderer.ml pp_view), so this does
            not regress the e9d4b96 text cleanup. *)
         Some { name; change = Unchanged; domain_type; children = [] }
       | `Modified np ->
         let children = build_patch_children np in
         if children = [] then None
         else Some { name; change = Modified; domain_type; children })
    | `Unchanged ->
      (* Parent is unchanged - we don't have access to the value to extract nested content.
         This is a fundamental limitation - unchanged items don't carry their values. *)
      None


  (** [build_item_from_children_with_change c ~name ~of_value ~of_patch ~build_value_children ~build_patch_children]
      builds a [item] for a nested structured type that may be added/removed independently.

      This is the new equivalent of [build_nested_section_view_with_change], but returns a [item].

      @param c the parent structured change
      @param name the item name
      @param of_value extracts the nested value from the parent value
      @param of_patch extracts the nested change from the parent patch
      @param build_value_children builds view list from nested value and change type
      @param build_patch_children builds view list from nested patch
      @param domain_type the domain type for this item
      @return Some item if there are children, None otherwise
  *)
  let build_item_from_children_with_change
      (type parent pp nested_actual np)
      (c : (parent, pp) structured_change)
      ~(name : string)
      ~(of_value : parent -> nested_actual option)
      ~(of_patch : pp -> (nested_actual, np) structured_change)
      ~(build_value_children : change_type -> nested_actual -> view list)
      ~(build_patch_children : np -> view list)
      ~(domain_type : domain_type)
    : item option =
    match c with
    | `Added parent ->
      (match of_value parent with
       | None -> None
       | Some nested_val ->
         let children = build_value_children Added nested_val in
         if children = [] then None
         else Some { name; change = Added; domain_type; children })
    | `Removed parent ->
      (match of_value parent with
       | None -> None
       | Some nested_val ->
         let children = build_value_children Removed nested_val in
         if children = [] then None
         else Some { name; change = Removed; domain_type; children })
    | `Modified patch ->
      (match of_patch patch with
       | `Unchanged ->
         (* See build_item_from_children: emit a placeholder so JSON/verbose
            consumers see the node; text_renderer suppresses the bare header. *)
         Some { name; change = Unchanged; domain_type; children = [] }
       | `Added nested_val ->
         let children = build_value_children Added nested_val in
         if children = [] then None
         else Some { name; change = Modified; domain_type; children }
       | `Removed nested_val ->
         let children = build_value_children Removed nested_val in
         if children = [] then None
         else Some { name; change = Modified; domain_type; children }
       | `Modified np ->
         let children = build_patch_children np in
         if children = [] then None
         else Some { name; change = Modified; domain_type; children })
    | `Unchanged -> None


  (** [build_collection c ~name ~of_value ~of_patch ~build_item]
      builds a [new_collection] for a list field containing structured items.

      This is the new equivalent of [build_collection_view], but:
      - Returns [new_collection] instead of [collection_view]
      - The [build_item] function should return a [item] (which gets wrapped in [Item])
      - Items in the collection can have full structure (not simplified [element_view])

      @param c the parent structured change
      @param name the collection name
      @param of_value extracts the item list from the parent value
      @param of_patch extracts the change list from the parent patch
      @param build_item builds a item from an item change
      @param domain_type the domain type for this collection
      @return Some new_collection if there are items, None otherwise
  *)
  let build_collection
      (c : ('parent, 'pp) structured_change)
      ~(name : string)
      ~(of_value : 'parent -> 'item list)
      ~(of_patch : 'pp -> ('item, 'ip) structured_change list)
      ~(build_item : ('item, 'ip) structured_change -> item)
      ~(domain_type : domain_type)
    : collection option =
    let change_type = change_type_of c in
    let items = match c with
      | `Added parent ->
        parent |> of_value |> List.map (fun item -> Item (build_item (`Added item)))
      | `Removed parent ->
        parent |> of_value |> List.map (fun item -> Item (build_item (`Removed item)))
      | `Modified patch ->
        patch |> of_patch |> List.map (fun item_change -> Item (build_item item_change))
      | `Unchanged -> []
    in
    (* Filter out Unchanged items and placeholder items (for unchanged items where we don't have values) *)
    let items = List.filter (fun (i : view) ->
        match i with
        | Item item -> item.change <> Unchanged && item.name <> ""
        | Collection col -> col.change <> Unchanged
        | Field _ -> true
      ) items in
    if items = [] then None
    else Some { name; change = change_type; domain_type; items }

end


(* ==================== Reference Context (the old document) ==================== *)

(** [Ctx] is the generic context-resolution layer: the old (pre-change)
    document, indexed by id at each domain scope, queried by item builders.
    Built once per projection; [empty] is the no-reference context — every
    lookup returns [None].

    Id scoping mirrors the schema: track ids are global across a liveset's
    tracks and returns (siblings of one XML <Tracks> element); id 0 is the
    Main-track sentinel, so [track ~id:0] is always [None] while
    [automation ~track_id:0] resolves within the Main track's automations.
    Clip, automation, note and event ids are local to their container, so
    their lookups take the full scope path.

    Sub-entity tables are memoized lazily per parent: a clip with thousands
    of notes indexes its notes once on the first changed note, not once per
    note (same rationale as the per-clip tables this module replaces). A
    memoized [None] means "parent absent in the old document" and is cached
    too, so repeated lookups of an added child's id never rescan. *)
module Ctx : sig
  type t

  val empty : t
  (** No old document: every lookup returns [None]. *)

  val of_liveset : Liveset.t -> t
  (** Index [ls.tracks @ ls.returns] and [ls.main]. On an id shared by a
      regular track and a return (impossible in Live-authored data — track ids
      are globally unique), the first occurrence wins. *)

  val of_track_list : main:Track.MainTrack.t option -> Track.t list -> t
  (** Index an explicit track list — narrow callers and tests. First
      occurrence of an id wins (see [of_liveset]). *)

  val track : t -> id:int -> Track.t option
  val main_track : t -> Track.MainTrack.t option

  val midi_clip : t -> track_id:int -> id:int -> Clip.MidiClip.t option
  val audio_clip : t -> track_id:int -> id:int -> Clip.AudioClip.t option
  val automation : t -> track_id:int -> id:int -> Automation.t option
  (** [automation ~track_id:0] resolves within the Main track's automations. *)

  val midi_note : t -> track_id:int -> clip_id:int -> id:int -> Clip.MidiNote.t option
  val envelope_event :
    t -> track_id:int -> automation_id:int -> id:int -> Automation.EnvelopeEvent.t option
end = struct
  type t = {
    tracks : (int, Track.t) Hashtbl.t;
    main : Track.MainTrack.t option;
    (* Lazy per-parent memo tables; a [None] value memoizes "parent absent". *)
    midi_clips : (int, (int, Clip.MidiClip.t) Hashtbl.t option) Hashtbl.t;
    audio_clips : (int, (int, Clip.AudioClip.t) Hashtbl.t option) Hashtbl.t;
    automations : (int, (int, Automation.t) Hashtbl.t option) Hashtbl.t;
    notes : (int * int, (int, Clip.MidiNote.t) Hashtbl.t option) Hashtbl.t;
    events : (int * int, (int, Automation.EnvelopeEvent.t) Hashtbl.t option) Hashtbl.t;
  }

  (* [add_first tbl id t] indexes [t] under [id] unless one is present: the
     first occurrence of an id wins, so a return sharing a regular track's id
     cannot shadow it ([Hashtbl.add] alone would be last-wins). Live-authored
     ids are globally unique; this is a defensive, documented tie-break. *)
  let add_first tbl id (t : Track.t) =
    if not (Hashtbl.mem tbl id) then Hashtbl.add tbl id t

  let of_track_list ~(main : Track.MainTrack.t option) (tracks : Track.t list) : t =
    let tbl = Hashtbl.create 16 in
    List.iter (fun t -> add_first tbl (track_id_of t) t) tracks;
    { tracks = tbl; main;
      midi_clips = Hashtbl.create 8; audio_clips = Hashtbl.create 8;
      automations = Hashtbl.create 8; notes = Hashtbl.create 8;
      events = Hashtbl.create 8 }

  let of_liveset (ls : Liveset.t) : t =
    let main = match ls.Liveset.main with Track.Main m -> Some m | _ -> None in
    of_track_list ~main (ls.Liveset.tracks @ ls.Liveset.returns)

  let empty = of_track_list ~main:None []

  let track (c : t) ~(id : int) : Track.t option =
    if id = 0 then None else Hashtbl.find_opt c.tracks id

  let main_track (c : t) : Track.MainTrack.t option = c.main

  (* [memo_parent tbl ~key ~build] returns the id-keyed table for [key],
     building and memoizing it — including the [None] case — on first use.
     Returns ['v option]: a memoized [None] means "parent absent". *)
  let memo_parent (tbl : ('k, 'v option) Hashtbl.t) ~key (build : unit -> 'v option)
    : 'v option =
    match Hashtbl.find_opt tbl key with
    | Some v -> v
    | None ->
      let v = build () in
      Hashtbl.replace tbl key v;
      v

  let midi_clip (c : t) ~(track_id : int) ~(id : int) : Clip.MidiClip.t option =
    match
      memo_parent c.midi_clips ~key:track_id (fun () ->
          match track c ~id:track_id with
          | Some (Track.Midi t) ->
            let tbl = Hashtbl.create 16 in
            List.iter (fun (cl : Clip.MidiClip.t) ->
                Hashtbl.replace tbl cl.Clip.MidiClip.id cl) t.Track.MidiTrack.clips;
            Some tbl
          | _ -> None)
    with
    | None -> None
    | Some clips -> Hashtbl.find_opt clips id

  let audio_clip (c : t) ~(track_id : int) ~(id : int) : Clip.AudioClip.t option =
    match
      memo_parent c.audio_clips ~key:track_id (fun () ->
          match track c ~id:track_id with
          | Some (Track.Audio t | Track.Group t | Track.Return t) ->
            let tbl = Hashtbl.create 16 in
            List.iter (fun (cl : Clip.AudioClip.t) ->
                Hashtbl.replace tbl cl.Clip.AudioClip.id cl) t.Track.AudioTrack.clips;
            Some tbl
          | _ -> None)
    with
    | None -> None
    | Some clips -> Hashtbl.find_opt clips id

  let track_automations (t : Track.t) : Automation.t list =
    match t with
    | Track.Midi x -> x.Track.MidiTrack.automations
    | Track.Audio x | Track.Group x | Track.Return x -> x.Track.AudioTrack.automations
    | Track.Main x -> x.Track.MainTrack.automations

  let automation (c : t) ~(track_id : int) ~(id : int) : Automation.t option =
    match
      memo_parent c.automations ~key:track_id (fun () ->
          let autos =
            if track_id = 0 then
              Option.map (fun (m : Track.MainTrack.t) -> m.Track.MainTrack.automations) c.main
            else Option.map track_automations (track c ~id:track_id)
          in
          match autos with
          | None -> None
          | Some autos ->
            let tbl = Hashtbl.create 8 in
            List.iter (fun (a : Automation.t) ->
                Hashtbl.replace tbl a.Automation.id a) autos;
            Some tbl)
    with
    | None -> None
    | Some tbl -> Hashtbl.find_opt tbl id

  let midi_note (c : t) ~(track_id : int) ~(clip_id : int) ~(id : int)
    : Clip.MidiNote.t option =
    match
      memo_parent c.notes ~key:(track_id, clip_id) (fun () ->
          match midi_clip c ~track_id ~id:clip_id with
          | None -> None
          | Some cl ->
            let tbl = Hashtbl.create 64 in
            List.iter (fun (n : Clip.MidiNote.t) ->
                Hashtbl.replace tbl n.Clip.MidiNote.id n) cl.Clip.MidiClip.notes;
            Some tbl)
    with
    | None -> None
    | Some notes -> Hashtbl.find_opt notes id

  let envelope_event (c : t) ~(track_id : int) ~(automation_id : int) ~(id : int)
    : Automation.EnvelopeEvent.t option =
    match
      memo_parent c.events ~key:(track_id, automation_id) (fun () ->
          match automation c ~track_id ~id:automation_id with
          | None -> None
          | Some a ->
            let tbl = Hashtbl.create 32 in
            List.iter (fun (e : Automation.EnvelopeEvent.t) ->
                Hashtbl.replace tbl e.Automation.EnvelopeEvent.id e) a.Automation.events;
            Some tbl)
    with
    | None -> None
    | Some events -> Hashtbl.find_opt events id
end


(* ==================== Unified Field Spec System ==================== *)

(** A unified field specification that can generate field views for both
    Added/Removed (from value) and Modified (from patch) cases.
    This eliminates the need for paired create_X_fields / create_X_patch_fields functions.
*)
type ('value, 'patch) unified_field_spec = {
  name : string;
  get_value : 'value -> field_value;                          (** Extract field value from parent *)
  get_old_value : 'value -> field_value option;               (** None = use get_value *)
  get_patch : 'patch -> field_value atomic_update;            (** Extract field update from patch *)
}


(** [build_value_field_views specs change_type value] builds field views from a value.
    Used for Added/Removed cases.
    @param specs the list of unified field specs
    @param change_type the type of change (Added or Removed)
    @param value the parent value
    @param domain_type the domain type for these fields
*)
let build_value_field_views
    (specs : ('v, 'p) unified_field_spec list)
    (change_type : change_type)
    (value : 'v)
    ~(domain_type : domain_type)
  : view list =
  specs |> List.map (fun spec ->
      let old_val = match spec.get_old_value value with
        | Some fv -> fv
        | None -> spec.get_value value
      in
      (Field {
          name = spec.name;
          change = change_type;
          domain_type;
          kind = Content;
          oldval = (if change_type = Removed then Some old_val else None);
          newval = (if change_type = Added then Some (spec.get_value value) else None);
        } : view))


(** [build_patch_field_views specs patch] builds field views from a patch.
    Used for Modified cases. Only returns fields that have actually changed.
    @param specs the list of unified field specs
    @param patch the parent patch
    @param domain_type the domain type for these fields
*)
let build_patch_field_views
    (specs : ('v, 'p) unified_field_spec list)
    (patch : 'p)
    ~(domain_type : domain_type)
  : view list =
  specs
  |> List.filter_map (fun spec ->
      let update = spec.get_patch patch in
      match update with
      | `Unchanged -> None
      | `Modified { oldval; newval } ->
        Some ((Field {
            name = spec.name;
            change = Modified;
            domain_type;
            kind = Content;
            oldval = Some oldval;
            newval = Some newval;
          } : view))
    )

let map_specs
    (f_v : 'v2 -> 'v1)
    (f_p : 'p2 -> 'p1 structured_update)
    (specs : ('v1, 'p1) unified_field_spec list)
  : ('v2, 'p2) unified_field_spec list =
  List.map (fun spec ->
      { (spec) with
        get_value = (fun v -> spec.get_value (f_v v));
        get_old_value = (fun v -> spec.get_old_value (f_v v));
        get_patch = (fun p ->
            match f_p p with
            | `Modified bp -> spec.get_patch bp
            | `Unchanged -> `Unchanged)
      }) specs


(** [view_to_unchanged v] recursively re-stamps a view subtree as [Unchanged].
    Fields get [oldval = newval] so a value-rendered subtree (built via
    [\`Added value]) looks like unchanged reference data when spliced into an
    Unchanged placeholder. Used by the reference-population of Unchanged Mixer
    children (see create_*_track_item): we rebuild a Mixer's children from the
    reference track's value, then restamp them Unchanged so the JSON/text
    renderers treat them as context, not as changes. *)
let rec view_to_unchanged (v : view) : view =
  match v with
  | Field f -> Field { f with change = Unchanged; oldval = f.newval }
  | Item i -> Item { i with change = Unchanged; children = List.map view_to_unchanged i.children }
  | Collection c ->
    Collection { c with change = Unchanged; items = List.map view_to_unchanged c.items }


(** A specification for building a child section of an Item *)
type ('parent, 'patch) section_spec = {
  name : string;
  build : ('parent, 'patch) structured_change -> view option;
  fill : 'parent -> view -> view;
  (** Context fill from the old (pre-change) parent value: rebuilds this
      section's empty Unchanged placeholder — or recurses into a Modified
      child item via the [~context] callback — restamped Unchanged through
      [view_to_unchanged]. Identity unless the field is context-marked
      ([@view.context], generated by the view_spec PPX — TODO item 4). See
      [fill_section_context]. *)
}


(** Spec module - combinators for building section_spec values declaratively *)
module Spec = struct

  (** [inline_fields ~specs ~domain_type] builds inline field views from unified field specs.
      Returns Field views wrapped in an Item with empty name (""), automatically filtering
      out Unchanged fields. The empty name signals to [build_item_from_specs] that this
      Item's children should be inlined directly into the parent.
  *)
  let inline_fields
      (type v p)
      ~(specs : (v, p) unified_field_spec list)
      ~(domain_type : domain_type)
    : (v, p) section_spec =
    {
      name = "";
      build = (fun c ->
          let fields = match c with
            | `Added value ->
              build_value_field_views specs Added value ~domain_type
            | `Removed value ->
              build_value_field_views specs Removed value ~domain_type
            | `Modified patch ->
              build_patch_field_views specs patch ~domain_type
            | `Unchanged -> []
          in
          (* Filter out Unchanged fields *)
          let filtered = List.filter (function
              | Field f -> f.change <> Unchanged
              | _ -> true
            ) fields in
          if filtered = [] then None
          else Some (Item { name = ""; change = ViewBuilder.change_type_of c; domain_type; children = filtered })
        );
      (* Inline fields are flat children of the parent item; their context
         fill is handled at item level (see [fill_inline_context]), so the
         per-spec fill is identity. *)
      fill = (fun _ v -> v);
    }

  (** [child ~name ~of_value ~of_patch ~build_value_children ~build_patch_children ~domain_type]
      builds a section_spec for a nested item (e.g., Mixer, Loop).
  *)
  let child
      (type parent patch nested np)
      ~(name : string)
      ~(of_value : parent -> nested)
      ~(of_patch : patch -> np structured_update)
      ~(build_value_children : change_type -> nested -> view list)
      ~(build_patch_children : np -> view list)
      ~(domain_type : domain_type)
    : (parent, patch) section_spec =
    {
      name;
      build = (fun c ->
          ViewBuilder.build_item_from_children c
            ~name
            ~of_value
            ~of_patch
            ~build_value_children
            ~build_patch_children
            ~domain_type
          |> Option.map (fun i -> Item i)
        );
      fill = (fun _ v -> v);
    }

  (** [child_with_context] is [child] for a context-marked field
      ([@view.context], TODO item 4). Context fill from the old (pre-change)
      parent value: an empty Unchanged placeholder is rebuilt through the same
      [build] closure (value side, `` `Added ``) and restamped; a Modified
      child recurses into the child type's own fill via [~context]. Other
      views (Added/Removed children, non-empty items, other names) pass
      through untouched. [~context] is mandatory rather than optional so
      existing [child] applications stay total — an optional argument followed
      only by labelled arguments cannot be erased. *)
  let child_with_context
      (type parent patch nested np)
      ~(context : nested -> item -> item)
      ~(name : string)
      ~(of_value : parent -> nested)
      ~(of_patch : patch -> np structured_update)
      ~(build_value_children : change_type -> nested -> view list)
      ~(build_patch_children : np -> view list)
      ~(domain_type : domain_type)
    : (parent, patch) section_spec =
    let build c =
      ViewBuilder.build_item_from_children c
        ~name ~of_value ~of_patch ~build_value_children ~build_patch_children ~domain_type
      |> Option.map (fun i -> Item i)
    in
    let fill (old : parent) (v : view) : view =
      match v with
      | Item { name = n; change = Unchanged; children = []; _ } when String.equal n name ->
        (match build (`Added old) with
         | Some rebuilt -> view_to_unchanged rebuilt
         | None -> v)
      | Item ({ name = n; change = Modified; _ } as mi) when String.equal n name ->
        Item (context (of_value old) mi)
      | _ -> v
    in
    { name; build; fill }

  (** [child_optional ~name ~of_value ~of_patch ~build_value_children ~build_patch_children ~domain_type]
      builds a section_spec for a nested item that can be added/removed independently.
  *)
  let child_optional
      (type parent patch nested_actual np)
      ~(name : string)
      ~(of_value : parent -> nested_actual option)
      ~(of_patch : patch -> (nested_actual, np) structured_change)
      ~(build_value_children : change_type -> nested_actual -> view list)
      ~(build_patch_children : np -> view list)
      ~(domain_type : domain_type)
    : (parent, patch) section_spec =
    {
      name;
      build = (fun c ->
          ViewBuilder.build_item_from_children_with_change c
            ~name
            ~of_value
            ~of_patch
            ~build_value_children
            ~build_patch_children
            ~domain_type
          |> Option.map (fun i -> Item i)
        );
      fill = (fun _ v -> v);
    }

  (** [child_optional_with_context] is [child_optional] for a context-marked
      field ([@view.context], TODO item 4); as [child_with_context]'s fill,
      but a reference without the optional section leaves the placeholder as
      emitted (the rebuild yields [None]). *)
  let child_optional_with_context
      (type parent patch nested_actual np)
      ~(context : nested_actual -> item -> item)
      ~(name : string)
      ~(of_value : parent -> nested_actual option)
      ~(of_patch : patch -> (nested_actual, np) structured_change)
      ~(build_value_children : change_type -> nested_actual -> view list)
      ~(build_patch_children : np -> view list)
      ~(domain_type : domain_type)
    : (parent, patch) section_spec =
    let build c =
      ViewBuilder.build_item_from_children_with_change c
        ~name ~of_value ~of_patch ~build_value_children ~build_patch_children ~domain_type
      |> Option.map (fun i -> Item i)
    in
    let fill (old : parent) (v : view) : view =
      match v with
      | Item { name = n; change = Unchanged; children = []; _ } when String.equal n name ->
        (match build (`Added old) with
         | Some rebuilt -> view_to_unchanged rebuilt
         | None -> v)
      | Item ({ name = n; change = Modified; _ } as mi) when String.equal n name ->
        (match of_value old with
         | Some old_nested -> Item (context old_nested mi)
         | None -> v)
      | _ -> v
    in
    { name; build; fill }

  (** [collection ~name ~of_value ~of_patch ~build_item ~domain_type]
      builds a section_spec for a collection of items.
  *)
  let collection
      (type parent patch elem ep)
      ~(name : string)
      ~(of_value : parent -> elem list)
      ~(of_patch : patch -> (elem, ep) structured_change list)
      ~(build_item : (elem, ep) structured_change -> item)
      ~(domain_type : domain_type)
    : (parent, patch) section_spec =
    {
      name;
      build = (fun c ->
          ViewBuilder.build_collection c
            ~name
            ~of_value
            ~of_patch
            ~build_item
            ~domain_type
          |> Option.map (fun col -> Collection col)
        );
      (* Collections are never context-filled: their elements resolve the old
         document themselves through [Ctx] in the bespoke create_* builders. *)
      fill = (fun _ v -> v);
    }

end


(** [fill_section_context specs old item] applies each spec's context fill to
    [item]'s children — the body the view_spec PPX generates for a
    context-marked type's [fill_context] (TODO item 4). Specs without a real
    fill (unmarked fields, inline fields, collections) leave their child
    untouched, so the walk is total but inert for unmarked sections. *)
let fill_section_context
    (type v p)
    (specs : (v, p) section_spec list)
    (old : v)
    (item : item)
  : item =
  { item with
    children = List.map (fun child ->
        List.fold_left (fun v' spec -> spec.fill old v') child specs)
        item.children }

(** [fill_inline_context ~domain_type specs old item] re-attaches a Modified
    item's missing inline fields from the old value as Unchanged context:
    fields the patch path emitted stay untouched; missing ones are rebuilt from
    [old] (value side, `` `Added ``), restamped, and PREPENDED — so a Modified
    field keeps rendering last, exactly as the hand-written note/event ctx
    blocks did. [old = None] (no old document) returns [item] unchanged. *)
let fill_inline_context
    (type v p)
    ~(domain_type : domain_type)
    (specs : (v, p) unified_field_spec list)
    (old : v option)
    (item : item)
  : item =
  match old with
  | None -> item
  | Some ref ->
    let present name = List.exists (function
        | Field f -> f.name = name
        | _ -> false) item.children in
    let context = build_value_field_views specs Added ref ~domain_type
      |> List.filter_map (fun v' ->
          match v' with
          | Field ({ name; _ } as f) when not (present name) ->
            Some (view_to_unchanged (Field f))
          | _ -> None)
    in
    { item with children = context @ item.children }


(** [set_domain_type dt v] returns [v] with its top-level [domain_type] set to [dt].
    Shallow by design: the only producer of the inline (name = "") splice marker is
    [Spec.inline_fields], whose children are flat [Field] views, so there are no nested
    children whose own domain would be clobbered. *)
let set_domain_type (dt : domain_type) (v : view) : view =
  match v with
  | Field f -> Field { f with domain_type = dt }
  | Item i -> Item { i with domain_type = dt }
  | Collection c -> Collection { c with domain_type = dt }


(** [build_item_from_specs ~name ~domain_type ~specs c] builds an item from a list of section specs.
    This is the main entry point for declaratively building complex items.
    Each spec in the list is applied to the change, and the resulting views are concatenated.

    For inline_fields specs (name = ""), the children are extracted and restamped with the
    parent's [domain_type] before being spliced in. The PPX emits [B.default_domain_type]
    ([DTOther]) as a placeholder for inline_fields — a type's own domain is not known at PPX
    time — so the real domain is the parent's, which is only known here. For other specs, the
    resulting view is added as-is.
*)
let build_item_from_specs
    (type parent patch)
    ~(name : string)
    ~(domain_type : domain_type)
    ~(specs : (parent, patch) section_spec list)
    (c : (parent, patch) structured_change)
  : item =
  let change_type = ViewBuilder.change_type_of c in
  let children = specs |> List.filter_map (fun spec ->
      match spec.build c with
      | None -> None
      | Some view ->
        if spec.name = "" then
          (* inline_fields: splice children in, restamped with the parent's domain *)
          match view with
          | Item { children; _ } -> Some (List.map (set_domain_type domain_type) children)
          | _ -> Some [set_domain_type domain_type view]
        else
          Some [view]
    ) |> List.flatten in
  { name; change = change_type; domain_type; children }


(** Helper functions for creating field descriptors with common wrappers *)
let make_spec
    (wrapper : 'a -> field_value)
    (name : string)
    (get_v : 'v -> 'a)
    (get_p : 'p -> 'a atomic_update)
  : ('v, 'p) unified_field_spec =
  {
    name;
    get_value = (fun v -> wrapper (get_v v));
    get_old_value = (fun _ -> None);
    get_patch = (fun p -> ViewBuilder.map_atomic_update wrapper (get_p p));
  }

(** [make_spec_const wrapper name get_v] creates a unified field spec for a value that never changes in a patch (e.g. name). *)
let make_spec_const
    (wrapper : 'a -> field_value)
    (name : string)
    (get_v : 'v -> 'a)
  : ('v, 'p) unified_field_spec =
  {
    name;
    get_value = (fun v -> wrapper (get_v v));
    get_old_value = (fun _ -> None);
    get_patch = (fun _ -> `Unchanged);
  }

let make_int n v p = make_spec int_value n v p
let make_float n v p = make_spec float_value n v p
let make_string n v p = make_spec string_value n v p
let make_bool n v p = make_spec bool_value n v p

let make_time_field (fmt : dual_time_formatter) name get_v get_p = {
  name;
  get_value = (fun v -> fmt.format_new (get_v v));
  get_old_value = (fun v -> Some (fmt.format_old (get_v v)));
  get_patch = (fun p -> ViewBuilder.map_atomic_update_dual fmt.format_old fmt.format_new (get_p p));
}


(* Default note name style for MIDI notes *)
let default_note_name_style = Sharp

(** [create_note_item] builds a [item] for a single note change (new type system).
    @param ctx the old-document context; the old note (resolved by id within
      the (track, clip) scope) backs a Modified note's unchanged leaf fields
    @param track_id the enclosing track's identity id (ctx scope)
    @param clip_id the enclosing clip's identity id (ctx scope)
    @param note_name_style the style to use for note names (Sharp or Flat)
    @param c the note structured change
*)
let create_note_item
    ~(ctx : Ctx.t)
    ~(track_id : int)
    ~(clip_id : int)
    ?(note_name_style : note_display_style = default_note_name_style)
    ?(format_time : dual_time_formatter = default_dual_time_formatter)
    (c : (Clip.MidiNote.t, Clip.MidiNote.Patch.t) structured_change)
  : item =
  let open Clip.MidiNote in
  (* The old note backs a Modified note's unchanged leaf fields as context
     (see the fill below); its absence (no old document, added clip) keeps
     the patch-only emission. *)
  let reference_note = match c with
    | `Modified np -> Ctx.midi_note ctx ~track_id ~clip_id ~id:np.Patch.id
    | _ -> None
  in
  let specs = [
    make_time_field format_time "Time" (fun (x : t) -> x.time) (fun (x : Patch.t) -> x.time);
    make_float "Duration" (fun (x : t) -> x.duration) (fun (x : Patch.t) -> x.duration);
    make_float "Velocity" (fun (x : t) -> x.velocity) (fun (x : Patch.t) -> x.velocity);
    make_int "Note" (fun (x : t) -> x.note) (fun (x : Patch.t) -> x.note);
    make_float "Off Velocity" (fun (x : t) -> x.off_velocity) (fun (x : Patch.t) -> x.off_velocity);
  ]
  in
  let pitch = match c with
    | `Added n | `Removed n -> Some n.note
    | `Modified np ->
      (match np.Patch.note with
       | `Modified { newval; _ } -> Some newval
       | `Unchanged -> Option.map (fun (r : t) -> r.note) reference_note)
    | `Unchanged -> None
  in
  let note_name = match pitch, c with
    | Some p, _ ->
      Printf.sprintf "Note %s (%d)" (get_note_name_from_int ~style:note_name_style p) p
    | None, `Modified np -> Printf.sprintf "Note (#%d)" np.Patch.id
    | None, _ -> "Note"
  in
  let section_spec = Spec.inline_fields ~specs ~domain_type:DTNote in
  let item = build_item_from_specs ~name:note_name ~domain_type:DTNote ~specs:[section_spec] c in
  fill_inline_context ~domain_type:DTNote specs reference_note item


(** [event_value_to_field_value] converts an Automation.event_value to a field_value *)
let event_value_to_field_value v =
  match v with
  | Automation.FloatEvent f -> Ffloat f
  | Automation.IntEvent i -> Fint i
  | Automation.EnumEvent e -> Fint e


(* ==================== MidiClip Specs (using PPX + manual Loop) ==================== *)

(** [build_clip_section_name ~clip_type ~get_id ~get_name ~get_patch_id ~get_patch_name c]
    builds a section name for any clip type.
    @param clip_type The clip type label (e.g., "MidiClip", "AudioClip")
    @param get_id Extracts the ID from a clip value
    @param get_name Extracts the name from a clip value
    @param get_patch_id Extracts the ID from a clip patch
    @param get_patch_name Extracts the name atomic update from a clip patch
*)
let build_clip_section_name
    (type v p)
    ~(clip_type : string)
    ~(get_id : v -> int)
    ~(get_name : v -> string)
    ~(get_patch_id : p -> int)
    ~(get_patch_name : p -> string atomic_update)
    (c : (v, p) structured_change)
  : string =
  match c with
  | `Added clip | `Removed clip -> Printf.sprintf "%s (#%d): %s" clip_type (get_id clip) (get_name clip)
  | `Modified patch ->
    (match get_patch_name patch with
     | `Modified { newval; _ } -> Printf.sprintf "%s (#%d): %s" clip_type (get_patch_id patch) newval
     | `Unchanged -> Printf.sprintf "%s (#%d)" clip_type (get_patch_id patch))
  | `Unchanged -> clip_type

(** [build_midi_clip_section_name] builds the section name for a MidiClip. *)
let build_midi_clip_section_name =
  build_clip_section_name
    ~clip_type:"MidiClip"
    ~get_id:(fun c -> c.Clip.MidiClip.id)
    ~get_name:(fun c -> c.Clip.MidiClip.name)
    ~get_patch_id:(fun p -> p.Clip.MidiClip.Patch.id)
    ~get_patch_name:(fun p -> p.Clip.MidiClip.Patch.name)


(* ==================== AudioClip name helper ==================== *)

(** [build_audio_clip_section_name] builds the section name for an AudioClip. *)
let build_audio_clip_section_name =
  build_clip_section_name
    ~clip_type:"AudioClip"
    ~get_id:(fun c -> c.Clip.AudioClip.id)
    ~get_name:(fun c -> c.Clip.AudioClip.name)
    ~get_patch_id:(fun p -> p.Clip.AudioClip.Patch.id)
    ~get_patch_name:(fun p -> p.Clip.AudioClip.Patch.name)


(* ==================== Device ViewSpec Instantiations ==================== *)

module DeviceViewSpecB : Alsdiff_view_spec_types.View_spec_types.S
  with type domain_type = Output_types.domain_type
   and type change_type = Output_types.change_type
   and type field_value = Output_types.field_value
   and type view = Presentation_model.view
   and type item = Presentation_model.item
   and type collection = Presentation_model.collection
   and type dual_time_formatter = Display_context.dual_time_formatter
   and type ('v, 'p) section_spec = ('v, 'p) section_spec
   and type ('v, 'p) unified_field_spec = ('v, 'p) unified_field_spec
= struct
  type domain_type = Output_types.domain_type
  type change_type = Output_types.change_type =
    | Unchanged
    | Added
    | Removed
    | Modified
  type field_value = Output_types.field_value =
    | Fint of int
    | Ffloat of float
    | Fbool of bool
    | Fstring of string
  and view = Presentation_model.view =
    | Field of field
    | Item of item
    | Collection of collection
  and item = Presentation_model.item = {
    name : string;
    change : change_type;
    domain_type : domain_type;
    children : view list;
  }
  and collection = Presentation_model.collection = {
    name : string;
    change : change_type;
    domain_type : domain_type;
    items : view list;
  }
  and dual_time_formatter = Display_context.dual_time_formatter = {
    format_old : float -> field_value;
    format_new : float -> field_value;
  }

  type nonrec ('v, 'p) unified_field_spec = ('v, 'p) unified_field_spec
  type nonrec ('v, 'p) section_spec = ('v, 'p) section_spec

  let int_value = int_value
  let float_value = float_value
  let bool_value = bool_value
  let string_value = string_value
  let default_domain_type = DTOther
  let domain_type_of_name = Output_types.domain_type_of_name
  let format_unix_timestamp = Display_context.format_unix_timestamp

  let make_spec = make_spec
  let make_spec_const = make_spec_const
  let make_int = make_int
  let make_float = make_float
  let make_string = make_string
  let make_bool = make_bool
  let make_time_field = make_time_field

  let build_value_field_views = build_value_field_views
  let build_patch_field_views = build_patch_field_views
  let map_specs = map_specs
  let build_item_from_specs = build_item_from_specs
  let item_children (i : item) : view list = i.children
  (* Context fill: B is the primitives module (never a [@view.context] target
     itself), so its fill_context is the S-signature no-op; real fills live in
     each context-marked type's generated ViewSpec and compose through
     [fill_section_context]. *)
  let fill_context ~format_time:_ (_ : 'a) (item : item) : item = item
  let fill_section_context = fill_section_context
  (* B is the primitives module (never a [@view.child] target itself); these
     satisfy the S signature but are never invoked. Real child-section
     rendering happens in each type's generated ViewSpec. *)
  let build_value_children ~format_time:_ ?(domain_type = DTOther) (_ct : change_type) (_v : 'a) : view list =
    let _ = domain_type in []
  let build_patch_children ~format_time:_ ?(domain_type = DTOther) (_p : 'a) : view list =
    let _ = domain_type in []

  module Spec = Spec
end

module RegularDeviceVS = Device.RegularDevice.ViewSpec(DeviceViewSpecB)
module PluginDeviceVS = Device.PluginDevice.ViewSpec(DeviceViewSpecB)
module Max4LiveDeviceVS = Device.Max4LiveDevice.ViewSpec(DeviceViewSpecB)
module GroupDeviceVS = Device.GroupDevice.ViewSpec(DeviceViewSpecB)
module MidiTrackVS = Track.MidiTrack.ViewSpec(DeviceViewSpecB)
module AudioTrackVS = Track.AudioTrack.ViewSpec(DeviceViewSpecB)
module MainTrackVS = Track.MainTrack.ViewSpec(DeviceViewSpecB)
module MidiClipVS = Clip.MidiClip.ViewSpec(DeviceViewSpecB)
module AudioClipVS = Clip.AudioClip.ViewSpec(DeviceViewSpecB)
module CurveControlsVS = Automation.CurveControls.ViewSpec(DeviceViewSpecB)
module VersionVS = Liveset.Version.ViewSpec(DeviceViewSpecB)
module DeviceVS = Device.ViewSpec(DeviceViewSpecB)


(** [create_events_item] builds a [item] for an envelope event change (new type system).
    @param ctx the old-document context; the old event (resolved by id within
      the (track, automation) scope) re-attaches unchanged leaf fields for a
      `` `Modified `` change (mirroring [create_note_item]). A curve-only edit
      otherwise emits just the Curve child with no Time/Value to place the
      event by.
    @param track_id the enclosing track's identity id (ctx scope; 0 = Main)
    @param automation_id the enclosing automation's identity id (ctx scope)
*)
let create_events_item
    ~(ctx : Ctx.t)
    ~(track_id : int)
    ~(automation_id : int)
    ?(format_time : dual_time_formatter = default_dual_time_formatter)
    (c : (Automation.EnvelopeEvent.t, Automation.EnvelopeEvent.Patch.t) structured_change)
  : item =
  let open Automation in
  let reference_event = match c with
    | `Modified ep ->
      Ctx.envelope_event ctx ~track_id ~automation_id
        ~id:ep.Automation.EnvelopeEvent.Patch.id
    | _ -> None
  in
  let base_specs = [
    make_time_field format_time "Time" (fun (x : EnvelopeEvent.t) -> x.time) (fun (x : EnvelopeEvent.Patch.t) -> x.time);
    make_spec event_value_to_field_value "Value"
      (fun (x : EnvelopeEvent.t) -> x.value) (fun (x : EnvelopeEvent.Patch.t) -> x.value);
  ]
  in
  let curve_section_spec = Spec.child_optional
      ~name:"Curve"
      ~of_value:(fun (e : EnvelopeEvent.t) -> e.curve)
      ~of_patch:(fun (p : EnvelopeEvent.Patch.t) -> p.curve)
      ~build_value_children:(CurveControlsVS.build_value_fields ~format_time ~domain_type:DTEvent)
      ~build_patch_children:(CurveControlsVS.build_patch_fields ~format_time ~domain_type:DTEvent)
      ~domain_type:DTEvent
  in
  let base_section_spec = Spec.inline_fields ~specs:base_specs ~domain_type:DTEvent in
  let item = build_item_from_specs ~name:"EnvelopeEvent" ~domain_type:DTEvent ~specs:[base_section_spec; curve_section_spec] c in
  fill_inline_context ~domain_type:DTEvent base_specs reference_event item


(* ==================== Clip Item Builders (after VS instantiations) ==================== *)

(** [create_midi_clip_item] creates a [item] from a MidiClip structured change.
    The PPX generates inline fields (name, start/end time), the Loop child,
    the TimeSignature child, and the Notes collection, threading format_time
    parent->child so Loop's time fields render correctly. Modified notes
    resolve their old values through [ctx], keyed by this clip's identity id
    ([Ctx] memoizes the note table once per clip); the same ctx-resolved old
    clip fills the empty Unchanged Loop/TimeSignature placeholders via the
    generated [fill_context] ([@view.context] on loop/signature). *)
let create_midi_clip_item
    ~(ctx : Ctx.t)
    ~(track_id : int)
    ?(note_name_style : note_display_style = default_note_name_style)
    ?(format_time : dual_time_formatter = default_dual_time_formatter)
    (c : (Clip.MidiClip.t, Clip.MidiClip.Patch.t) structured_change)
  : item =
  let name = build_midi_clip_section_name c in
  let clip_id = match c with
    | `Added cl | `Removed cl -> cl.Clip.MidiClip.id
    | `Modified p -> p.Clip.MidiClip.Patch.id
    | `Unchanged -> -1
  in
  let specs = MidiClipVS.section_specs ~format_time
      ~build_notes:(create_note_item ~ctx ~track_id ~clip_id
                      ~note_name_style ~format_time) in
  let item = build_item_from_specs ~name ~domain_type:DTClip ~specs c in
  match c with
  | `Modified cp ->
    (match Ctx.midi_clip ctx ~track_id ~id:cp.Clip.MidiClip.Patch.id with
     | Some ref -> MidiClipVS.fill_context ~format_time ref item
     | None -> item)
  | _ -> item

(** [create_audio_clip_item] creates a [item] from an AudioClip structured change.
    The PPX generates inline fields (name, start/end time), the Loop child,
    the TimeSignature child, the SampleRef child, and the Fade child, threading
    format_time parent->child so Loop's time fields render correctly. The
    empty Unchanged placeholders are filled from the ctx-resolved old clip via
    the generated [fill_context] ([@view.context] on loop/signature/sample_ref/
    fade). *)
let create_audio_clip_item
    ~(ctx : Ctx.t)
    ~(track_id : int)
    ?(format_time : dual_time_formatter = default_dual_time_formatter)
    (c : (Clip.AudioClip.t, Clip.AudioClip.Patch.t) structured_change)
  : item =
  let name = build_audio_clip_section_name c in
  let specs = AudioClipVS.section_specs ~format_time in
  let item = build_item_from_specs ~name ~domain_type:DTClip ~specs c in
  match c with
  | `Modified cp ->
    (match Ctx.audio_clip ctx ~track_id ~id:cp.Clip.AudioClip.Patch.id with
     | None -> item
     | Some ref -> AudioClipVS.fill_context ~format_time ref item)
  | _ -> item


(* ==================== Track Element Views ==================== *)


(** [create_automation_item] builds a [item] for an automation change (new type system).
    @param ctx the old-document context; the old automation (resolved by id
      within the track scope) supplies per-event references for `` `Modified ``
      events so unchanged event fields can be re-attached as context (see
      [create_events_item])
    @param track_id the enclosing track's identity id (ctx scope; 0 = Main)
    @param get_pointee_name function to resolve pointee IDs to names
    @param c the automation structured change
*)
let create_automation_item
    ~(ctx : Ctx.t)
    ~(track_id : int)
    ~(get_pointee_name : int -> string)
    ?(format_time : dual_time_formatter = default_dual_time_formatter)
    (c : (Automation.t, Automation.Patch.t) structured_change)
  : item =
  let open Automation in
  let change_type = ViewBuilder.change_type_of c in
  let automation_id = match c with
    | `Added a | `Removed a -> a.Automation.id
    | `Modified patch -> patch.Automation.Patch.id
    | `Unchanged -> -1
  in
  let automation_name = match c with
    | `Added a | `Removed a ->
      Printf.sprintf "Automation (id=%d, target=%s)" a.id (get_pointee_name a.target)
    | `Modified patch -> Printf.sprintf "Automation (id=%d, target=%s)" patch.id (get_pointee_name patch.target)
    | `Unchanged -> "Automation"
  in

  (* Wrap a list of event items in an [Events] Collection, so that
     [max_collection_items] truncation applies uniformly to Modified, Added
     and Removed automations. Empty event lists yield no children. *)
  let wrap_events (event_items : view list) : view list =
    match event_items with
    | [] -> []
    | _ -> [ Collection { name = "Events"; change = change_type; domain_type = DTEvent; items = event_items } ]
  in
  let render_value_events
      (tag : EnvelopeEvent.t -> (EnvelopeEvent.t, EnvelopeEvent.Patch.t) structured_change)
      (events : EnvelopeEvent.t list) : view list =
    events |> List.map (fun e ->
        let event_item =
          create_events_item ~ctx ~track_id ~automation_id ~format_time (tag e) in
        Item { event_item with name = Printf.sprintf "Event[%d]" e.Automation.EnvelopeEvent.id })
  in
  (* Modified events resolve their old values through [ctx] themselves, keyed
     by this automation's identity id; [Ctx] memoizes the event table once per
     (track, automation) — same quadratic-scan rationale as the notes path. *)
  let event_children : view list =
    match c with
    | `Modified patch ->
      let events = patch.events |> List.filter_map (fun event_change ->
          match event_change with
          | `Unchanged -> None
          | _ ->
            let event_id = match event_change with
              | `Added e -> e.Automation.EnvelopeEvent.id
              | `Removed e -> e.Automation.EnvelopeEvent.id
              | `Modified p -> p.Automation.EnvelopeEvent.Patch.id
              | `Unchanged -> -1
            in
            let event_item =
              create_events_item ~ctx ~track_id ~automation_id ~format_time event_change in
            Some (Item { event_item with name = Printf.sprintf "Event[%d]" event_id }))
      in
      wrap_events events
    | `Added a -> wrap_events (render_value_events (fun e -> `Added e) a.events)
    | `Removed r -> wrap_events (render_value_events (fun e -> `Removed e) r.events)
    | `Unchanged -> []
  in

  { name = automation_name; change = change_type; domain_type = DTAutomation; children = event_children }



(* ==================== Full Track Views ==================== *)


(** [prepend_track_identity_fields ~track_id ~group_id item] re-attaches the
    TrackId/GroupId identity fields to a Modified track item. They are identity
    metadata, not diff content: consumers (the web app) nest tracks under their
    group by these fields, but the patch path drops them for Modified tracks
    (the const spec yields no patch value; an unchanged group_id atom carries
    none either). TrackId comes from the patch's identity field; GroupId from
    the reference (old) track when the patch says unchanged. Fields the patch
    path already emitted (GroupId changed -> Modified field) are left alone. *)
let prepend_track_identity_fields
    ~(track_id : int option)
    ~(group_id : int option)
    (item : item)
  : item =
  let present name = List.exists (function
      | Field f -> f.name = name
      | _ -> false) item.children in
  let mk name v =
    Field { name; change = Unchanged; domain_type = DTTrack; kind = Identity;
            oldval = None; newval = Some (Fint v) }
  in
  let extras = List.filter_map (fun (name, v) ->
      match v with
      | Some v when not (present name) -> Some (mk name v)
      | _ -> None)
      [ ("TrackId", track_id); ("GroupId", group_id) ]
  in
  { item with children = extras @ item.children }


(** [create_midi_track_item] creates a [item] from a MidiTrack structured change (new type system).
    @param get_pointee_name function to resolve pointee IDs to names
    @param note_name_style the style to use for note names (Sharp or Flat)
    @param c the MIDI track structured change
*)
let create_midi_track_item
    ~(ctx : Ctx.t)
    ~(get_pointee_name : int -> string)
    ?(note_name_style : note_display_style = default_note_name_style)
    ?(format_time : dual_time_formatter = default_dual_time_formatter)
    (c : (Track.MidiTrack.t, Track.MidiTrack.Patch.t) structured_change)
  : item =
  let track_id = match c with
    | `Modified p -> p.Track.MidiTrack.Patch.id
    | `Added t | `Removed t -> t.Track.MidiTrack.id
    | `Unchanged -> 0
  in
  let ref_track = Ctx.track ctx ~id:track_id in
  let item = MidiTrackVS.build_item
      ~format_time
      ~build_clips:(create_midi_clip_item ~ctx ~track_id ~note_name_style ~format_time)
      ~build_automations:(create_automation_item ~ctx ~track_id ~get_pointee_name ~format_time)
      ~build_devices:(DeviceVS.build_item ~format_time)
      ~name:(MidiTrackVS.build_section_name c)
      ~domain_type:DTTrack c in
  let item = match ref_track with
    | Some (Track.Midi rt) -> MidiTrackVS.fill_context ~format_time rt item
    | _ -> item
  in
  match c with
  | `Modified pt ->
    let group_id = match (pt.Track.MidiTrack.Patch.group_id, ref_track) with
      | `Unchanged, Some (Track.Midi rt) -> Some rt.Track.MidiTrack.group_id
      | _ -> None
    in
    prepend_track_identity_fields ~track_id:(Some pt.Track.MidiTrack.Patch.id) ~group_id item
  | _ -> item

(** [create_audio_like_track_item] creates a [item] for AudioTrack-like structured changes.
    Shared implementation for AudioTrack and GroupTrack (which share the same internal structure).
    @param get_pointee_name function to resolve pointee IDs to names
    @param track_type_name The display type name (e.g., "AudioTrack" or "Group")
    @param c the track structured change
*)
let create_audio_like_track_item
    ~(ctx : Ctx.t)
    ~(get_pointee_name : int -> string)
    ?(format_time : dual_time_formatter = default_dual_time_formatter)
    ~track_type_name
    (c : (Track.AudioTrack.t, Track.AudioTrack.Patch.t) structured_change)
  : item =
  let track_id = match c with
    | `Modified p -> p.Track.AudioTrack.Patch.id
    | `Added t | `Removed t -> t.Track.AudioTrack.id
    | `Unchanged -> 0
  in
  let ref_track = Ctx.track ctx ~id:track_id in
  let item = AudioTrackVS.build_item
      ~format_time
      ~build_clips:(create_audio_clip_item ~ctx ~track_id ~format_time)
      ~build_automations:(create_automation_item ~ctx ~track_id ~get_pointee_name ~format_time)
      ~build_devices:(DeviceVS.build_item ~format_time)
      ~name:(AudioTrackVS.build_section_name ~type_label:track_type_name c)
      ~domain_type:DTTrack c in
  let item = match ref_track with
    | Some (Track.Audio rt | Track.Group rt | Track.Return rt) ->
      AudioTrackVS.fill_context ~format_time rt item
    | _ -> item
  in
  match c with
  | `Modified pt ->
    let group_id = match (pt.Track.AudioTrack.Patch.group_id, ref_track) with
      | `Unchanged, Some (Track.Audio rt | Track.Group rt | Track.Return rt) ->
        Some rt.Track.AudioTrack.group_id
      | _ -> None
    in
    prepend_track_identity_fields ~track_id:(Some pt.Track.AudioTrack.Patch.id) ~group_id item
  | _ -> item


let create_audio_track_item
    ~(ctx : Ctx.t)
    ~(get_pointee_name : int -> string)
    ?(note_name_style : note_display_style = default_note_name_style)
    ?(format_time : dual_time_formatter = default_dual_time_formatter)
    (c : (Track.AudioTrack.t, Track.AudioTrack.Patch.t) structured_change)
  : item =
  ignore (note_name_style : note_display_style);
  create_audio_like_track_item ~ctx ~get_pointee_name ~format_time
    ~track_type_name:"AudioTrack" c


(* Return tracks share the AudioTrack representation, but the item name is the
   only carrier of the track kind for consumers (web/CLI), so they must not be
   labeled "AudioTrack" (group tracks already get their own "Group" label). *)
let create_return_track_item
    ~(ctx : Ctx.t)
    ~(get_pointee_name : int -> string)
    ?(note_name_style : note_display_style = default_note_name_style)
    ?(format_time : dual_time_formatter = default_dual_time_formatter)
    (c : (Track.AudioTrack.t, Track.AudioTrack.Patch.t) structured_change)
  : item =
  ignore (note_name_style : note_display_style);
  create_audio_like_track_item ~ctx ~get_pointee_name ~format_time
    ~track_type_name:"ReturnTrack" c


let create_group_track_item
    ~(ctx : Ctx.t)
    ~(get_pointee_name : int -> string)
    ?(note_name_style : note_display_style = default_note_name_style)
    ?(format_time : dual_time_formatter = default_dual_time_formatter)
    (c : (Track.AudioTrack.t, Track.AudioTrack.Patch.t) structured_change)
  : item =
  ignore (note_name_style : note_display_style);
  create_audio_like_track_item ~ctx ~get_pointee_name ~format_time
    ~track_type_name:"Group" c


(** [create_main_track_item] creates a [item] from a MainTrack structured change (new type system).
    @param get_pointee_name function to resolve pointee IDs to names
    @param c the main track structured change

    For an `` `Unchanged `` master with a reference value, [build_item] renders a
    bare item with NO children — there is no Mixer placeholder for
    [fill_context] to fill. Instead the value side is built from the reference
    ([`Added m]) and restamped [Unchanged] via [view_to_unchanged] (fields get
    [oldval = newval]), so consumers can read the project's tempo/time
    signature even when only regular tracks changed. Only the Mixer child is
    kept: restamping the whole subtree would also materialize the reference
    master's Automations/Devices as pseudo-context — symmetric with
    [fill_context], which fills only context-marked sections. *)
let create_main_track_item
    ~(ctx : Ctx.t)
    ~(get_pointee_name : int -> string)
    ?(note_name_style : note_display_style = default_note_name_style)
    ?(format_time : dual_time_formatter = default_dual_time_formatter)
    (c : (Track.MainTrack.t, Track.MainTrack.Patch.t) structured_change)
  : item =
  ignore (note_name_style : note_display_style);
  let ref_main = Ctx.main_track ctx in
  let build_main
      (tag : (Track.MainTrack.t, Track.MainTrack.Patch.t) structured_change)
    : item =
    MainTrackVS.build_item
      ~format_time
      ~build_automations:(create_automation_item ~ctx ~track_id:0 ~get_pointee_name ~format_time)
      ~build_devices:(DeviceVS.build_item ~format_time)
      ~name:(MainTrackVS.build_section_name tag)
      ~domain_type:DTTrack tag
  in
  match c, ref_main with
  | `Unchanged, Some m ->
    (match view_to_unchanged (Item (build_main (`Added m))) with
     | Item i ->
       { i with
         children =
           List.filter (function
               | Item { name = "Mixer"; _ } -> true
               | _ -> false) i.children }
     | _ -> assert false)
  | _ ->
    let item = build_main c in
    (match ref_main with
     | None -> item
     | Some rt -> MainTrackVS.fill_context ~format_time rt item)


(* ==================== Liveset View ==================== *)

let locator_field_specs ?(format_time = default_dual_time_formatter) () : (Liveset.Locator.t, Liveset.Locator.Patch.t) unified_field_spec list = [
  make_spec_const int_value "Id" (fun (x : Liveset.Locator.t) -> x.id);
  make_string "Name" (fun (x : Liveset.Locator.t) -> x.name) (fun (p : Liveset.Locator.Patch.t) -> p.name);
  make_time_field format_time "Time" (fun (x : Liveset.Locator.t) -> x.time) (fun (p : Liveset.Locator.Patch.t) -> p.time);
]

let locator_section_specs ?(format_time = default_dual_time_formatter) () : (Liveset.Locator.t, Liveset.Locator.Patch.t) section_spec list = [
  Spec.inline_fields ~specs:(locator_field_specs ~format_time ()) ~domain_type:DTLocator;
]

let create_locator_item
    ?(format_time : dual_time_formatter = default_dual_time_formatter)
    (c : (Liveset.Locator.t, Liveset.Locator.Patch.t) structured_change)
  : item =
  let locator_name = match c with
    | `Added l | `Removed l -> Printf.sprintf "Locator (id=%d)" l.Liveset.Locator.id
    | `Modified p -> Printf.sprintf "Locator (id=%d)" p.Liveset.Locator.Patch.id
    | `Unchanged -> "Locator"
  in
  build_item_from_specs ~name:locator_name ~domain_type:DTLocator ~specs:(locator_section_specs ~format_time ()) c



(* ==================== Liveset Helper Functions ==================== *)

(** [make_format_time] creates a time formatting closure based on the chosen format.
    Uses tempo and time signature events from the MainTrack for conversion.
    QuarterNotes returns Ffloat (no change), BeatTime/RealTime return Fstring.
    Precomputes sorted segments once to avoid redundant sorting per call. *)
let make_format_time (time_format : time_format)
    ~(tempo_events : (float * float * Automation.CurveControls.t option) list)
    ~(ts_events : (float * Clip.TimeSignature.t) list)
    () : float -> field_value =
  match time_format with
  | QuarterNotes -> float_value
  | BeatTime ->
    let segments = Track.MainTrack.prepare_ts_segments ts_events in
    fun x -> Fstring (format_position (Track.MainTrack.time_to_position_precomputed segments x))
  | RealTime ->
    let segments = Track.MainTrack.prepare_tempo_segments tempo_events in
    fun x -> Fstring (format_realtime (Track.MainTrack.time_to_realtime_precomputed x segments))

let make_dual_format_time (time_format : time_format)
    ~(tempo_events_old : (float * float * Automation.CurveControls.t option) list)
    ~(ts_events_old : (float * Clip.TimeSignature.t) list)
    ~(tempo_events_new : (float * float * Automation.CurveControls.t option) list)
    ~(ts_events_new : (float * Clip.TimeSignature.t) list)
    () : dual_time_formatter =
  {
    format_old = make_format_time time_format ~tempo_events:tempo_events_old ~ts_events:ts_events_old ();
    format_new = make_format_time time_format ~tempo_events:tempo_events_new ~ts_events:ts_events_new ();
  }

(** [make_pointee_resolver c] creates a pointee name resolver function from a liveset change.
    This is used to resolve automation target IDs to human-readable names.
*)
let make_pointee_resolver
    (c : (Liveset.t, Liveset.Patch.t) structured_change)
  : int -> string =
  match c with
  | `Added ls | `Removed ls -> (fun id -> Liveset.get_pointee_name_from_table ls.Liveset.pointees id)
  | `Modified patch ->
    (fun id ->
       match Liveset.get_pointee_name_from_table_opt patch.Liveset.Patch.new_pointees id with
       | Some name -> name
       | None ->
         match Liveset.get_pointee_name_from_table_opt patch.Liveset.Patch.old_pointees id with
         | Some name -> name
         | None -> Printf.sprintf "<Pointee %d>" id)
  | `Unchanged -> fun id -> Printf.sprintf "<Pointee %d>" id


(** [dispatch_track_change ~ctx ~get_pointee_name ~note_name_style tc] dispatches a
    track change to the appropriate track item builder based on track type. The
    old document is resolved from [ctx] by the change's identity id — including
    the Return-vs-Audio relabeling, since a return track has no patch variant.
    Returns None for Unchanged or Main tracks (Main tracks are handled separately).
*)
let dispatch_track_change
    ~(ctx : Ctx.t)
    ~(get_pointee_name : int -> string)
    ?(note_name_style : note_display_style = default_note_name_style)
    ?(format_time : dual_time_formatter = default_dual_time_formatter)
    (tc : (Track.t, Track.Patch.t) structured_change)
  : view option =
  let ref_track = match tc with
    | `Modified p -> Ctx.track ctx ~id:(patch_track_id_of p)
    | _ -> None
  in
  match tc with
  (* Midi tracks *)
  | `Added (Track.Midi t) ->
    Some (Item (create_midi_track_item ~ctx ~get_pointee_name ~note_name_style ~format_time (`Added t)))
  | `Removed (Track.Midi t) ->
    Some (Item (create_midi_track_item ~ctx ~get_pointee_name ~note_name_style ~format_time (`Removed t)))
  | `Modified (Track.Patch.MidiPatch pt) ->
    Some (Item (create_midi_track_item ~ctx ~get_pointee_name ~note_name_style ~format_time (`Modified pt)))
  (* Audio tracks *)
  | `Added (Track.Audio t) ->
    Some (Item (create_audio_track_item ~ctx ~get_pointee_name ~note_name_style ~format_time (`Added t)))
  | `Removed (Track.Audio t) ->
    Some (Item (create_audio_track_item ~ctx ~get_pointee_name ~note_name_style ~format_time (`Removed t)))
  | `Modified (Track.Patch.AudioPatch pt) ->
    (* A modified return track surfaces as an AudioPatch (Return has no patch
       variant); the reference track is what tells us to label it ReturnTrack. *)
    (match ref_track with
     | Some (Track.Return _) ->
       Some (Item (create_return_track_item ~ctx ~get_pointee_name ~note_name_style ~format_time (`Modified pt)))
     | _ ->
       Some (Item (create_audio_track_item ~ctx ~get_pointee_name ~note_name_style ~format_time (`Modified pt))))
  (* Group tracks *)
  | `Added (Track.Group t) ->
    Some (Item (create_group_track_item ~ctx ~get_pointee_name ~note_name_style ~format_time (`Added t)))
  | `Removed (Track.Group t) ->
    Some (Item (create_group_track_item ~ctx ~get_pointee_name ~note_name_style ~format_time (`Removed t)))
  | `Modified (Track.Patch.GroupPatch pt) ->
    Some (Item (create_group_track_item ~ctx ~get_pointee_name ~note_name_style ~format_time (`Modified pt)))
  (* Return tracks - dedicated builder so the item name carries the kind *)
  | `Added (Track.Return t) ->
    Some (Item (create_return_track_item ~ctx ~get_pointee_name ~note_name_style ~format_time (`Added t)))
  | `Removed (Track.Return t) ->
    Some (Item (create_return_track_item ~ctx ~get_pointee_name ~note_name_style ~format_time (`Removed t)))
  (* Main tracks - handled separately in project *)
  | `Added (Track.Main _) | `Removed (Track.Main _) | `Modified (Track.Patch.MainPatch _) -> None
  | `Unchanged -> None


(** [build_liveset_section_items] is the shared skeleton for tracks and returns:
    derive the change list from [of_value]/[of_patch] (filtering with
    [value_filter]/[change_filter]) and dispatch each change via
    [dispatch_track_change]; the old document is resolved through [ctx]. *)
let build_liveset_section_items
    ~(ctx : Ctx.t)
    ~(get_pointee_name : int -> string)
    ?(note_name_style : note_display_style = default_note_name_style)
    ?(format_time : dual_time_formatter = default_dual_time_formatter)
    ~(of_value : Liveset.t -> Track.t list)
    ~(of_patch : Liveset.Patch.t -> (Track.t, Track.Patch.t) structured_change list)
    ~(value_filter : Track.t -> bool)
    ~(change_filter : (Track.t, Track.Patch.t) structured_change -> bool)
    (c : (Liveset.t, Liveset.Patch.t) structured_change)
  : view list =
  let changes = match c with
    | `Added ls -> ls |> of_value |> List.filter value_filter |> List.map (fun t -> `Added t)
    | `Removed ls -> ls |> of_value |> List.filter value_filter |> List.map (fun t -> `Removed t)
    | `Modified patch -> patch |> of_patch |> List.filter change_filter
    | `Unchanged -> []
  in
  List.filter_map (fun tc ->
      dispatch_track_change ~ctx ~get_pointee_name ~note_name_style ~format_time tc
    ) changes


(** [build_liveset_tracks_items ~ctx ~get_pointee_name ~note_name_style c] builds
    view items for all regular tracks (Midi, Audio, Group) in a liveset change.
    Main and Return tracks are handled separately; unchanged context is resolved
    through [ctx]. *)
let build_liveset_tracks_items
    ~(ctx : Ctx.t)
    ~(get_pointee_name : int -> string)
    ?(note_name_style : note_display_style = default_note_name_style)
    ?(format_time : dual_time_formatter = default_dual_time_formatter)
    (c : (Liveset.t, Liveset.Patch.t) structured_change)
  : view list =
  let is_regular_track = function
    | Track.Main _ | Track.Return _ -> false
    | _ -> true
  in
  let is_regular_track_change = function
    | `Added (Track.Main _) | `Removed (Track.Main _) | `Modified (Track.Patch.MainPatch _) -> false
    | `Added (Track.Return _) | `Removed (Track.Return _) -> false
    | _ -> true
  in
  build_liveset_section_items ~ctx ~get_pointee_name ~note_name_style ~format_time
    ~of_value:(fun ls -> ls.Liveset.tracks)
    ~of_patch:(fun p -> p.tracks)
    ~value_filter:is_regular_track
    ~change_filter:is_regular_track_change
    c


(** [build_liveset_returns_items ~ctx ~get_pointee_name ~note_name_style c] builds
    view items for all return tracks in a liveset change; unchanged context is
    resolved through [ctx]. *)
let build_liveset_returns_items
    ~(ctx : Ctx.t)
    ~(get_pointee_name : int -> string)
    ?(note_name_style : note_display_style = default_note_name_style)
    ?(format_time : dual_time_formatter = default_dual_time_formatter)
    (c : (Liveset.t, Liveset.Patch.t) structured_change)
  : view list =
  build_liveset_section_items ~ctx ~get_pointee_name ~note_name_style ~format_time
    ~of_value:(fun ls -> ls.Liveset.returns)
    ~of_patch:(fun p -> p.returns)
    ~value_filter:(Fun.const true)
    ~change_filter:(Fun.const true)
    c


(** Liveset field specifications for atomic fields (Name, Creator) *)
let liveset_field_specs : (Liveset.t, Liveset.Patch.t) unified_field_spec list = [
  make_string "Name"    (fun ls -> ls.Liveset.name)    (fun p -> p.Liveset.Patch.name);
  make_string "Creator" (fun ls -> ls.Liveset.creator) (fun p -> p.Liveset.Patch.creator);
]


(** [param_value_to_field_value] converts a device parameter value to a
    serializable field value (mirrors GenericParam.ViewSpec.pv_to_fv). *)
let param_value_to_field_value (v : Device.param_value) : field_value =
  match v with
  | Device.Float f -> Ffloat f
  | Device.Int i -> Fint i
  | Device.Bool b -> Fbool b
  | Device.Enum (e, _) -> Fint e

(** [liveset_tempo_context c ~reference_main] extracts the current-side tempo
    (BPM) and time-signature (Ableton encoded code) of the project from a
    liveset change, for consumers that need them at every detail level (the
    web app's realtime ruler). The value is read from the patch when the
    master changed, else from the reference main track; an `` `Unchanged ``
    liveset (self-diff) carries no context — the web shows the no-diff
    result instead. *)
let liveset_tempo_context
    (c : (Liveset.t, Liveset.Patch.t) structured_change)
    ~(reference_main : Track.MainTrack.t option)
  : (field_value option * field_value option) =
  let main_values (m : Track.MainTrack.t) =
    (Some (param_value_to_field_value
             m.Track.MainTrack.mixer.tempo.Device.GenericParam.value),
     Some (param_value_to_field_value
             m.Track.MainTrack.mixer.time_signature.Device.GenericParam.value))
  in
  let ref_values () =
    match reference_main with
    | Some m -> main_values m
    | None -> (None, None)
  in
  let param_current ~(ref : field_value option)
      (u : Device.GenericParam.Patch.t structured_update) : field_value option =
    match u with
    | `Modified p ->
      (match p.Device.GenericParam.Patch.value with
       | `Modified { newval; _ } -> Some (param_value_to_field_value newval)
       | `Unchanged -> ref)
    | `Unchanged -> ref
  in
  match c with
  | `Added ls | `Removed ls ->
    (match ls.Liveset.main with
     | Track.Main m -> main_values m
     | _ -> (None, None))
  | `Modified p ->
    (match p.Liveset.Patch.main with
     | `Modified pt ->
       (match pt.Track.MainTrack.Patch.mixer with
        | `Modified mx ->
          let ref_tempo, ref_ts = ref_values () in
          (param_current ~ref:ref_tempo mx.Track.MainMixer.Patch.tempo,
           param_current ~ref:ref_ts mx.Track.MainMixer.Patch.time_signature)
        | `Unchanged -> ref_values ())
     | `Unchanged -> ref_values ())
  | `Unchanged -> (None, None)

(** [project ~old change] projects a liveset change into the view tree.
    [old], the pre-change document, is a peer input — not a patch annotation:
    unchanged context (mixer strips, note pitch, tempo/time-signature,
    GroupId) is resolved from it by id through [Ctx], so "show unchanged
    context" is one uniform policy instead of per-type plumbing. All callers
    already hold both documents.
    @param note_name_style the style to use for note names (Sharp or Flat)
    @param old the pre-change document; [None] is the no-reference projection
    @param c the liveset structured change *)
let project
    ?(note_name_style : note_display_style = default_note_name_style)
    ?(format_time : dual_time_formatter = default_dual_time_formatter)
    ?(old : Liveset.t option)
    (c : (Liveset.t, Liveset.Patch.t) structured_change)
  : item =
  let ctx = match old with
    | Some ls -> Ctx.of_liveset ls
    | None -> Ctx.empty
  in
  let change_type = ViewBuilder.change_type_of c in
  let get_pointee_name = make_pointee_resolver c in

  (* Build section name from liveset name *)
  let section_name = match c with
    | `Added ls | `Removed ls -> "LiveSet: " ^ ls.name
    | `Modified patch ->
      (match patch.name with
       | `Modified { newval; _ } -> "LiveSet: " ^ newval
       | `Unchanged -> "LiveSet")
    | `Unchanged -> "LiveSet"
  in

  (* Build atomic fields using liveset_field_specs *)
  let atomic_children =
    (match c with
     | `Added v -> build_value_field_views liveset_field_specs Added v ~domain_type:DTLiveset
     | `Removed v -> build_value_field_views liveset_field_specs Removed v ~domain_type:DTLiveset
     | `Modified p -> build_patch_field_views liveset_field_specs p ~domain_type:DTLiveset
     | `Unchanged -> [])
    |> List.filter (function Field fv -> fv.change <> Unchanged | _ -> true)
  in

  (* Build Version section *)
  let version_item = ViewBuilder.build_item_from_children c
      ~name:"Version"
      ~of_value:(fun (ls : Liveset.t) -> ls.version)
      ~of_patch:(fun (p : Liveset.Patch.t) -> p.version)
      ~build_value_children:(VersionVS.build_value_fields ~format_time ~domain_type:DTVersion)
      ~build_patch_children:(VersionVS.build_patch_fields ~format_time ~domain_type:DTVersion)
      ~domain_type:DTVersion
  in

  (* Reference main track (old side), resolved through [ctx]. *)
  let ref_main = Ctx.main_track ctx in

  (* Build Main Track section - singleton, always present.
     Emit the inner item directly so it appears flat under the LiveSet
     instead of wrapped in a redundant "Main Track" envelope. The item is
     carried whenever a reference liveset is available — including when the
     main patch is `Unchanged (built from the reference and populated with
     Unchanged mixer context) so consumers can read the project's tempo/time
     signature when only regular tracks changed. *)
  let main_track_item = match c with
    | `Modified p ->
      (match p.Liveset.Patch.main with
       | `Modified pt ->
         Some (create_main_track_item ~ctx ~get_pointee_name ~note_name_style ~format_time (`Modified pt))
       | `Unchanged ->
         (match ref_main with
          | Some _ -> Some (create_main_track_item ~ctx ~get_pointee_name ~note_name_style ~format_time `Unchanged)
          | None -> None))
    | `Added ls | `Removed ls ->
      (* The master must render for whole-liveset Added/Removed too (its
         tempo/time signature/devices are part of the set), symmetric with the
         other value-side sections. *)
      (match ls.Liveset.main with
       | Track.Main m ->
         let tag = match c with `Added _ -> `Added m | _ -> `Removed m in
         Some (create_main_track_item ~ctx ~get_pointee_name ~note_name_style ~format_time tag)
       | _ -> None)
    | `Unchanged -> None
  in

  (* Build Locators collection *)
  let locators_collection = ViewBuilder.build_collection c
      ~name:"Locators"
      ~of_value:(fun (ls : Liveset.t) -> ls.Liveset.locators)
      ~of_patch:(fun (p : Liveset.Patch.t) -> p.locators)
      ~build_item:(create_locator_item ~format_time)
      ~domain_type:DTLocator
  in

  (* Combine all children: tracks/returns are flat direct children (not wrapped
     in collections) so they bypass [max_collection_items] — tracks are
     structural and truncating them is meaningless for practical usage. The cap
     still applies to Events/Notes/Clips/Devices/Locators via their Collection
     wrappers. Order: atomic fields → Version → Main Track → regular tracks →
     returns → locators. *)
  (* Tempo/Time Signature context: the current project tempo and time
     signature ride on the LiveSet item at every detail level (mirroring the
     TrackId/GroupId identity fields), so level-dropping renderers can never
     hide them — the web app's realtime ruler needs them even when the whole
     MainTrack item is dropped under Summary/Compact presets. *)
  let tempo_context, ts_context = liveset_tempo_context c ~reference_main:ref_main in
  let context_fields =
    List.filter_map (fun (name, v) ->
        match v with
        | Some fv ->
          Some (Field { name; change = Unchanged; domain_type = DTLiveset;
                        kind = Context;
                        oldval = None; newval = Some fv })
        | None -> None)
      [ ("Tempo", tempo_context); ("Time Signature", ts_context) ]
  in

  let children =
    atomic_children
    @ context_fields
    @ opt_view (fun i -> Item i) version_item
    @ opt_view (fun i -> Item i) main_track_item
    @ build_liveset_tracks_items ~ctx ~get_pointee_name ~note_name_style ~format_time c
    @ build_liveset_returns_items ~ctx ~get_pointee_name ~note_name_style ~format_time c
    @ opt_view (fun c -> Collection c) locators_collection
  in

  { name = section_name; change = change_type; domain_type = DTLiveset; children }
