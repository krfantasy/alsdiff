open Ppxlib
module List = ListLabels

(* Flatten a Longident.t to a string list *)
let rec lid_to_list : Longident.t -> string list = function
  | Lident s -> [s]
  | Ldot (lid, s) -> lid_to_list lid @ [s]
  | Lapply _ -> []

(* ==================== Attribute helpers ==================== *)

let has_attribute (attrs : attributes) attr_name =
  List.exists attrs ~f:(fun attr ->
      String.equal attr.attr_name.txt attr_name)

(* Extract a string from an attribute payload like [@view.label "X"] *)
let extract_string_payload (payload : payload) : string option =
  match payload with
  | PStr [{ pstr_desc = Pstr_eval ({ pexp_desc = Pexp_constant (Pconst_string (s, _, _)); _ }, _); _ }] ->
    Some s
  | _ -> None

(* Extract an identifier from an attribute payload like [@view.scalar time] *)
let extract_ident_payload (payload : payload) : string option =
  match payload with
  | PStr [{ pstr_desc = Pstr_eval ({ pexp_desc = Pexp_ident { txt = Lident name; _ }; _ }, _); _ }] ->
    Some name
  | _ -> None

(* ==================== View attribute parsing ==================== *)

let get_scalar_kind (attrs : attributes) : string option =
  List.find_map attrs ~f:(fun attr ->
      if String.equal attr.attr_name.txt "view.scalar"
      then extract_ident_payload attr.attr_payload
      else None)

let is_const_attr (attrs : attributes) : bool =
  has_attribute attrs "view.const"

let get_custom_fn (attrs : attributes) : string option =
  List.find_map attrs ~f:(fun attr ->
      if String.equal attr.attr_name.txt "view.custom"
      then extract_ident_payload attr.attr_payload
      else None)

let has_skip_attr (attrs : attributes) : bool =
  has_attribute attrs "view.skip"

let get_label (attrs : attributes) : string option =
  List.find_map attrs ~f:(fun attr ->
      if String.equal attr.attr_name.txt "view.label"
      then extract_string_payload attr.attr_payload
      else None)

let get_child_domain (attrs : attributes) : string option =
  List.find_map attrs ~f:(fun attr ->
      if String.equal attr.attr_name.txt "view.child"
      then extract_string_payload attr.attr_payload
      else None)

let get_optional_child_domain (attrs : attributes) : string option =
  List.find_map attrs ~f:(fun attr ->
      if String.equal attr.attr_name.txt "view.optional_child"
      then extract_string_payload attr.attr_payload
      else None)

let get_collection_domain (attrs : attributes) : string option =
  List.find_map attrs ~f:(fun attr ->
      if String.equal attr.attr_name.txt "view.collection"
      then extract_string_payload attr.attr_payload
      else None)

let has_inline_child_attr (attrs : attributes) : bool =
  has_attribute attrs "view.inline_child"

let has_name_attr (attrs : attributes) : bool =
  has_attribute attrs "view.name"

let has_name_patch_attr (attrs : attributes) : bool =
  has_attribute attrs "view.name_patch"

let has_display_attr (attrs : attributes) : bool =
  has_attribute attrs "view.display"

let get_type_label (attrs : attributes) : string option =
  List.find_map attrs ~f:(fun attr ->
      if String.equal attr.attr_name.txt "view.type_label"
      then extract_string_payload attr.attr_payload
      else None)

let get_builder_name (attrs : attributes) : string option =
  List.find_map attrs ~f:(fun attr ->
      if String.equal attr.attr_name.txt "view.builder"
      then extract_string_payload attr.attr_payload
      else None)

let has_context_attr (attrs : attributes) : bool =
  has_attribute attrs "view.context"

(* ==================== Variant dispatch attribute ==================== *)

(* Routing-table entry for [@@view.variant_dispatch] — see the payload
   grammar comment at [parse_variant_dispatch]. The PPX does NOT check that
   constructor, module or patch names exist: the generated match is
   exhaustiveness- and name-checked by the compiler downstream (a missing
   table entry = non-exhaustive match error; a wrong module = unbound
   module error). *)
type variant_entry = {
  vctor : string;    (* constructor name, e.g. "Regular" *)
  vmodule : string;  (* variant module, e.g. "RegularDevice"; "" = skip entry *)
  vpatch : string;   (* patch constructor, e.g. "RegularPatch"; "" = no Modified arm *)
  vbuilder : string; (* builder label, e.g. "build_midi"; "" = self-routed mode *)
}

type variant_dispatch = {
  vd_domain : string;        (* domain-type name, e.g. "DTDevice" *)
  vd_entries : variant_entry list;
}

let has_variant_dispatch_attr (attrs : attributes) : bool =
  has_attribute attrs "view.variant_dispatch"

let string_literal_of_pexp (e : expression) : string option =
  match e.pexp_desc with
  | Pexp_constant (Pconst_string (s, _, _)) -> Some s
  | _ -> None

let expr_of_item (it : structure_item) : expression option =
  match it.pstr_desc with
  | Pstr_eval (e, _) -> Some e
  | _ -> None

(* Flatten `e1; e2; ...` into [e1; e2; ...] *)
let rec flatten_sequence (e : expression) : expression list =
  match e.pexp_desc with
  | Pexp_sequence (a, b) -> flatten_sequence a @ flatten_sequence b
  | _ -> [e]

let entry_of_expr (e : expression) : (variant_entry, string) result =
  match e.pexp_desc with
  | Pexp_tuple [ a; b; c; d ] ->
    (match (string_literal_of_pexp a, string_literal_of_pexp b,
            string_literal_of_pexp c, string_literal_of_pexp d) with
     | Some vctor, Some vmodule, Some vpatch, Some vbuilder ->
       Ok { vctor; vmodule; vpatch; vbuilder }
     | _ -> Error "view.variant_dispatch entries must be 4 string literals")
  | _ -> Error "view.variant_dispatch entries must be 4 string literals"

(* Payload grammar, as written on the sum types:

     [@@view.variant_dispatch "DTDevice" (
         "Regular", "RegularDevice", "RegularPatch", "";
         "Plugin", "PluginDevice", "PluginPatch", "";
       )]

   which the parser delivers as PStr [Pstr_eval (Pexp_apply ("DTDevice",
   <entry tuples joined by Pexp_sequence>))]: the domain-type name string
   applied to a parenthesized `;`-separated sequence of 4-string tuples. *)
(* Stdlib [Result] provides no [let*] binding operators, so the folds below
   thread [Ok]/[Error] explicitly, in the brief's rejection order. *)
let parse_variant_dispatch (attrs : attributes) : (variant_dispatch, string) result =
  let payload =
    List.find_opt attrs ~f:(fun (a : attribute) ->
        String.equal a.attr_name.txt "view.variant_dispatch")
    |> Option.map (fun a -> a.attr_payload)
  in
  match payload with
  | None -> Error "internal: view.variant_dispatch absent"
  | Some (PStr items) ->
    let exprs = List.filter_map items ~f:expr_of_item in
    (match exprs with
     | [] -> Error "view.variant_dispatch needs at least one entry"
     | first :: rest ->
       (match first.pexp_desc, rest with
        | Pexp_constant (Pconst_string (_, _, _)), [] ->
          (* domain string with no entry tuples applied to it *)
          Error "view.variant_dispatch needs at least one entry"
        | Pexp_apply
            ( { pexp_desc = Pexp_constant (Pconst_string (domain, _, _)); _ },
              [ (Nolabel, body) ] ),
          [] ->
          List.fold_left (flatten_sequence body) ~init:(Ok [])
            ~f:(fun acc e ->
                match acc with
                | Error msg -> Error msg
                | Ok entries ->
                  (match entry_of_expr e with
                   | Error msg -> Error msg
                   | Ok entry -> Ok (entry :: entries)))
          |> Result.map (fun entries ->
              { vd_domain = domain; vd_entries = List.rev entries })
        | _ -> Error "view.variant_dispatch must start with a domain-type name string"))
  | Some _ -> Error "view.variant_dispatch payload must be a domain name applied to entry tuples"

(* Mode + table consistency, in rejection order (order matters — a
   skip-in-all-self table must report the skip error, not the mixed error):
   - all-self: every entry has a module and an empty builder (Device)
   - builder-routed: every entry either has a module AND a builder label, or
     is a skip entry (module = "", builder = "") (Track)
   Then, regardless of mode: self-routed needs [@view.type_label]; a skip
   entry may not carry a builder label; patch constructors and (routed)
   builder labels are each claimed by at most one entry.
   Returns [true] = builder-routed. *)
let validate_variant_dispatch
    ~(type_label : string option)
    (vd : variant_dispatch)
  : (bool, string) result
  =
  let all_self =
    List.for_all vd.vd_entries ~f:(fun e -> e.vmodule <> "" && e.vbuilder = "") in
  let no_builders = List.for_all vd.vd_entries ~f:(fun e -> e.vbuilder = "") in
  let has_skip = List.exists vd.vd_entries ~f:(fun e -> e.vmodule = "") in
  if vd.vd_entries = [] then Error "view.variant_dispatch needs at least one entry"
  else
    match
      (if all_self then Ok false
       else if no_builders && has_skip then
         Error "self-routed variant_dispatch cannot skip constructors"
       else if
         List.for_all vd.vd_entries ~f:(fun e -> e.vmodule = "" || e.vbuilder <> "")
       then Ok true
       else Error "view.variant_dispatch mixes self-routed and builder-routed entries")
    with
    | Error msg -> Error msg
    | Ok builder_routed ->
      (* self-routed needs a placeholder name for the `Unchanged arm *)
      if (not builder_routed) && type_label = None then
        Error "self-routed variant_dispatch requires [@view.type_label] for the Unchanged placeholder"
      else if List.exists vd.vd_entries ~f:(fun e -> e.vmodule = "" && e.vbuilder <> "") then
        Error "skip entry cannot carry a builder label in view.variant_dispatch"
      else
        (* patch constructors must be claimed by at most one entry *)
        let patches =
          List.filter_map vd.vd_entries ~f:(fun e ->
              if e.vpatch = "" then None else Some e.vpatch)
        in
        if List.length patches
           <> List.length (List.sort_uniq ~cmp:String.compare patches) then
          Error "duplicate patch constructor in view.variant_dispatch"
        else
          (* builder labels too: two routed entries sharing one (necessarily
             the same vmodule — different modules would be a type error) make
             the generated nested labelled funs shadow-bind: the last binding
             silently wins for BOTH arms, and the compiler catches nothing. *)
          let builders =
            List.filter_map vd.vd_entries ~f:(fun e ->
                if e.vmodule <> "" && e.vbuilder <> "" then Some e.vbuilder else None)
          in
          if List.length builders
             <> List.length (List.sort_uniq ~cmp:String.compare builders) then
            Error "duplicate builder label in view.variant_dispatch"
          else Ok builder_routed

(* ==================== Label generation ==================== *)

(* Convert snake_case to title case: start_time -> "Start Time", on -> "On" *)
let field_name_to_label (name : string) : string =
  let buf = Buffer.create (String.length name) in
  String.iteri (fun i c ->
      if c = '_' then Buffer.add_char buf ' '
      else if i = 0 || (i > 0 && name.[i - 1] = '_') then Buffer.add_char buf (Char.uppercase_ascii c)
      else Buffer.add_char buf c
    ) name;
  Buffer.contents buf

let get_field_label (field_name : string) (attrs : attributes) : string =
  match get_label attrs with
  | Some s -> s
  | None -> field_name_to_label field_name

(* ==================== Type classification ==================== *)

let is_atomic_type_name : core_type -> string option = function
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "int"; _ }, []); _ } -> Some "int"
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "float"; _ }, []); _ } -> Some "float"
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "string"; _ }, []); _ } -> Some "string"
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "bool"; _ }, []); _ } -> Some "bool"
  | _ -> None

let make_maker_name = function
  | "int" -> "B.make_int" | "float" -> "B.make_float"
  | "string" -> "B.make_string" | "bool" -> "B.make_bool"
  | _ -> assert false

let make_wrapper_name = function
  | "int" -> "B.int_value" | "float" -> "B.float_value"
  | "string" -> "B.string_value" | "bool" -> "B.bool_value"
  | _ -> assert false

(* Get the module path from a type like Loop.t -> Longident for "Loop" *)
let module_path_of_type (ptyp : core_type) : Longident.t option =
  match ptyp.ptyp_desc with
  | Ptyp_constr ({ txt = Ldot (prefix, "t"); _ }, []) -> Some prefix
  | _ -> None

let module_path_of_option_type (ptyp : core_type) : Longident.t option =
  match ptyp.ptyp_desc with
  | Ptyp_constr ({ txt = Lident "option"; _ }, [inner]) -> module_path_of_type inner
  | _ -> None

let module_path_of_list_type (ptyp : core_type) : Longident.t option =
  match ptyp.ptyp_desc with
  | Ptyp_constr ({ txt = Lident "list"; _ }, [inner]) -> module_path_of_type inner
  | _ -> None

(* ==================== Code generation helpers ==================== *)

let mk_lid_expr loc lid =
  Ast_builder.Default.pexp_ident ~loc { loc; txt = lid }

let mk_str loc s = Ast_builder.Default.estring ~loc s

let generate_value_accessor ~loc field_name =
  let open Ast_builder.Default in
  pexp_fun ~loc Nolabel None
    (ppat_var ~loc { txt = "v"; loc })
    (pexp_field ~loc
       (pexp_ident ~loc { txt = Lident "v"; loc })
       { loc; txt = Lident field_name })

let generate_patch_accessor ~loc field_name =
  let open Ast_builder.Default in
  pexp_fun ~loc Nolabel None
    (ppat_var ~loc { txt = "p"; loc })
    (pexp_field ~loc
       (pexp_ident ~loc { txt = Lident "p"; loc })
       { loc; txt = Ldot (Lident "Patch", field_name) })

(* Build a list expression from a list of expressions *)
let mk_list_expr loc exprs =
  let open Ast_builder.Default in
  List.fold_right exprs ~init:(pexp_construct ~loc { txt = Lident "[]"; loc } None)
    ~f:(fun e acc -> pexp_construct ~loc { txt = Lident "::"; loc } (Some (pexp_tuple ~loc [e; acc])))

(* ==================== Field spec generators ==================== *)

let generate_atomic_field_spec ~loc label field_name maker =
  let open Ast_builder.Default in
  pexp_apply ~loc (mk_lid_expr loc (Longident.parse maker))
    [ Nolabel, mk_str loc label
    ; Nolabel, generate_value_accessor ~loc field_name
    ; Nolabel, generate_patch_accessor ~loc field_name ]

let generate_time_field_spec ~loc label field_name =
  let open Ast_builder.Default in
  pexp_apply ~loc (mk_lid_expr loc (Ldot (Lident "B", "make_time_field")))
    [ Nolabel, pexp_ident ~loc { txt = Lident "format_time"; loc }
    ; Nolabel, mk_str loc label
    ; Nolabel, generate_value_accessor ~loc field_name
    ; Nolabel, generate_patch_accessor ~loc field_name ]

let generate_unix_timestamp_field_spec ~loc label field_name =
  let open Ast_builder.Default in
  let wrapper =
    pexp_fun ~loc Nolabel None
      (ppat_var ~loc { txt = "x"; loc })
      (pexp_apply ~loc (mk_lid_expr loc (Ldot (Lident "B", "string_value")))
         [ Nolabel, pexp_apply ~loc (mk_lid_expr loc (Ldot (Lident "B", "format_unix_timestamp")))
             [ Nolabel, pexp_ident ~loc { txt = Lident "x"; loc } ] ])
  in
  pexp_apply ~loc (mk_lid_expr loc (Ldot (Lident "B", "make_spec")))
    [ Nolabel, wrapper
    ; Nolabel, mk_str loc label
    ; Nolabel, generate_value_accessor ~loc field_name
    ; Nolabel, generate_patch_accessor ~loc field_name ]

let generate_const_field_spec ~loc label field_name atomic_type =
  let open Ast_builder.Default in
  pexp_apply ~loc (mk_lid_expr loc (Ldot (Lident "B", "make_spec_const")))
    [ Nolabel, mk_lid_expr loc (Longident.parse (make_wrapper_name atomic_type))
    ; Nolabel, mk_str loc label
    ; Nolabel, generate_value_accessor ~loc field_name ]

let generate_custom_field_spec ~loc label field_name custom_fn =
  let open Ast_builder.Default in
  pexp_apply ~loc (mk_lid_expr loc (Ldot (Lident "B", "make_spec")))
    [ Nolabel, mk_lid_expr loc (Longident.parse custom_fn)
    ; Nolabel, mk_str loc label
    ; Nolabel, generate_value_accessor ~loc field_name
    ; Nolabel, generate_patch_accessor ~loc field_name ]

(* ==================== Section spec generators ==================== *)

let lid_of_strings = function
  | [] -> assert false
  | [x] -> Lident x
  | x :: xs -> List.fold_left xs ~init:(Lident x) ~f:(fun acc s -> Ldot (acc, s))

let mk_vs_field_access loc view_spec_lid field_name arg_var =
  let open Ast_builder.Default in
  let vs_mod =
    pmod_apply ~loc
      (pmod_ident ~loc { loc; txt = view_spec_lid })
      (pmod_ident ~loc { loc; txt = Lident "B" })
  in
  (* [format_time] is a free variable captured from the enclosing
     [section_specs ~format_time] scope, so the Spec.child /
     Spec.collection build-callback signatures stay unlabeled and manual
     callers are unaffected. Threading format_time parent->child lets
     time-bearing children (Loop, CurveControls, ...) be declared via
     [@view.child] instead of hand-spliced in change_projector.ml. *)
  pexp_fun ~loc Nolabel None
    (ppat_var ~loc { txt = arg_var; loc })
    (pexp_letmodule ~loc { txt = Some "Vs"; loc } vs_mod
       (pexp_apply ~loc
          (pexp_ident ~loc { loc; txt = Ldot (Lident "Vs", field_name) })
          [ Labelled "format_time", pexp_ident ~loc { txt = Lident "format_time"; loc }
          ; Nolabel, pexp_ident ~loc { txt = Lident arg_var; loc } ]))

(** [mk_vs_children_access] generates the [build_value_children] /
    [build_patch_children] callbacks for a [Spec.child] by delegating to the
    child type's [ViewSpec.build_value_children] / [build_patch_children],
    which render the FULL child section (all sub-views via [section_specs]),
    not just inline atomic fields. This is the correct behavior for a nested
    child whose own fields are themselves [@view.child] (e.g. Mixer, whose
    volume/pan/mute/solo are GenericParam children — it has NO inline atomic
    fields, so the old [build_value_fields]/[build_patch_fields] yielded []). *)
let mk_vs_children_access loc view_spec_lid =
  let open Ast_builder.Default in
  let vs_mod =
    pmod_apply ~loc
      (pmod_ident ~loc { loc; txt = view_spec_lid })
      (pmod_ident ~loc { loc; txt = Lident "B" })
  in
  let call_vs field arg_exprs =
    pexp_letmodule ~loc { txt = Some "Vs"; loc } vs_mod
      (pexp_apply ~loc
         (pexp_ident ~loc { loc; txt = Ldot (Lident "Vs", field) })
         ((Labelled "format_time", pexp_ident ~loc { txt = Lident "format_time"; loc })
          :: arg_exprs))
  in
  (* build_value_children : change_type -> nested -> view list *)
  let build_value_fn =
    pexp_fun ~loc Nolabel None
      (ppat_var ~loc { txt = "ct"; loc })
      (pexp_fun ~loc Nolabel None
         (ppat_var ~loc { txt = "v"; loc })
         (call_vs "build_value_children"
            [ Nolabel, pexp_ident ~loc { txt = Lident "ct"; loc }
            ; Nolabel, pexp_ident ~loc { txt = Lident "v"; loc } ]))
  in
  (* build_patch_children : np -> view list *)
  let build_patch_fn =
    pexp_fun ~loc Nolabel None
      (ppat_var ~loc { txt = "np"; loc })
      (call_vs "build_patch_children"
         [ Nolabel, pexp_ident ~loc { txt = Lident "np"; loc } ])
  in
  (build_value_fn, build_patch_fn)

(** [mk_vs_fill_context] builds the [~context] callback for a context-marked
    child spec: the child ViewSpec's [fill_context] with the enclosing
    ~format_time pre-applied. Every view_spec type generates fill_context
    (no-op when unmarked), so the reference always typechecks. *)
let mk_vs_fill_context loc view_spec_lid =
  let open Ast_builder.Default in
  let vs_mod =
    pmod_apply ~loc
      (pmod_ident ~loc { loc; txt = view_spec_lid })
      (pmod_ident ~loc { txt = Lident "B"; loc })
  in
  pexp_letmodule ~loc { txt = Some "Vs"; loc } vs_mod
    (pexp_apply ~loc
       (pexp_ident ~loc { loc; txt = Ldot (Lident "Vs", "fill_context") })
       [ Labelled "format_time", pexp_ident ~loc { txt = Lident "format_time"; loc } ])

(* Context-marked fields emit through child_with_context /
   child_optional_with_context (mandatory [~context] — an optional ?context
   between labelled args is unerasable, which would make every plain
   Spec.child application partial); unmarked fields keep the plain
   constructors, byte-identical to today's generated code. *)
let generate_child_spec ~loc field_name label domain_type_name child_mod_lid ~(context : bool) =
  let open Ast_builder.Default in
  let mod_path = lid_to_list child_mod_lid in
  let view_spec_lid = lid_of_strings (mod_path @ ["ViewSpec"]) in
  let (build_value_fn, build_patch_fn) = mk_vs_children_access loc view_spec_lid in
  let constr = if context then "child_with_context" else "child" in
  let context_arg =
    if context then [ Labelled "context", mk_vs_fill_context loc view_spec_lid ] else [] in
  pexp_apply ~loc (mk_lid_expr loc (Ldot (Ldot (Lident "B", "Spec"), constr)))
    (context_arg
     @ [ Labelled "name", mk_str loc label
       ; Labelled "of_value", generate_value_accessor ~loc field_name
       ; Labelled "of_patch", generate_patch_accessor ~loc field_name
       ; Labelled "build_value_children", build_value_fn
       ; Labelled "build_patch_children", build_patch_fn
       ; Labelled "domain_type", pexp_apply ~loc (mk_lid_expr loc (Ldot (Lident "B", "domain_type_of_name")))
           [ Nolabel, mk_str loc domain_type_name ] ])

let generate_optional_child_spec ~loc field_name label domain_type_name child_mod_lid
    ~(context : bool) =
  let open Ast_builder.Default in
  let mod_path = lid_to_list child_mod_lid in
  let view_spec_lid = lid_of_strings (mod_path @ ["ViewSpec"]) in
  let (build_value_fn, build_patch_fn) = mk_vs_children_access loc view_spec_lid in
  let constr = if context then "child_optional_with_context" else "child_optional" in
  let context_arg =
    if context then [ Labelled "context", mk_vs_fill_context loc view_spec_lid ] else [] in
  pexp_apply ~loc (mk_lid_expr loc (Ldot (Ldot (Lident "B", "Spec"), constr)))
    (context_arg
     @ [ Labelled "name", mk_str loc label
       ; Labelled "of_value", generate_value_accessor ~loc field_name
       ; Labelled "of_patch", generate_patch_accessor ~loc field_name
       ; Labelled "build_value_children", build_value_fn
       ; Labelled "build_patch_children", build_patch_fn
       ; Labelled "domain_type", pexp_apply ~loc (mk_lid_expr loc (Ldot (Lident "B", "domain_type_of_name")))
           [ Nolabel, mk_str loc domain_type_name ] ])

let generate_collection_spec ~loc field_name label domain_type_name item_mod_lid =
  let open Ast_builder.Default in
  let mod_path = lid_to_list item_mod_lid in
  let view_spec_lid = lid_of_strings (mod_path @ ["ViewSpec"]) in
  let build_item_fn = mk_vs_field_access loc view_spec_lid "build_item" "ic" in
  pexp_apply ~loc (mk_lid_expr loc (Ldot (Ldot (Lident "B", "Spec"), "collection")))
    [ Labelled "name", mk_str loc label
    ; Labelled "of_value", generate_value_accessor ~loc field_name
    ; Labelled "of_patch", generate_patch_accessor ~loc field_name
    ; Labelled "build_item", build_item_fn
    ; Labelled "domain_type", pexp_apply ~loc (mk_lid_expr loc (Ldot (Lident "B", "domain_type_of_name")))
        [ Nolabel, mk_str loc domain_type_name ] ]

let generate_inline_child_binding ~loc field_name child_mod_lid =
  let open Ast_builder.Default in
  let mod_path = lid_to_list child_mod_lid in
  let view_spec_lid = lid_of_strings (mod_path @ ["ViewSpec"]) in
  let binding_name = "__inline_" ^ field_name in
  let vs_mod =
    pmod_apply ~loc
      (pmod_ident ~loc { loc; txt = view_spec_lid })
      (pmod_ident ~loc { loc; txt = Lident "B" })
  in
  let v_pat = ppat_var ~loc { txt = "v"; loc } in
  let p_pat = ppat_var ~loc { txt = "p"; loc } in
  let f_v =
    pexp_fun ~loc Nolabel None v_pat
      (pexp_field ~loc
         (pexp_ident ~loc { txt = Lident "v"; loc })
         { loc; txt = Lident field_name })
  in
  let f_p =
    pexp_fun ~loc Nolabel None p_pat
      (pexp_field ~loc
         (pexp_ident ~loc { txt = Lident "p"; loc })
         { loc; txt = Ldot (Lident "Patch", field_name) })
  in
  let map_call =
    (* The child's [field_specs] is a function of ~format_time (PPX-generated
       ViewSpecs always thread it), and [B.map_specs] requires the spliced
       list itself — so bind ~format_time here and let the use site apply it. *)
    pexp_apply ~loc (mk_lid_expr loc (Ldot (Lident "B", "map_specs")))
      [ Nolabel, f_v
      ; Nolabel, f_p
      ; Nolabel, pexp_apply ~loc
          (pexp_ident ~loc { loc; txt = Ldot (Lident "Vs", "field_specs") })
          [ Labelled "format_time", pexp_ident ~loc { txt = Lident "format_time"; loc } ] ]
  in
  let body =
    let inner = pexp_letmodule ~loc { txt = Some "Vs"; loc } vs_mod map_call in
    pexp_fun ~loc (Labelled "format_time") None
      (ppat_var ~loc { txt = "format_time"; loc })
      inner
  in
  pstr_value ~loc Nonrecursive [{
      pvb_pat = ppat_var ~loc { txt = binding_name; loc };
      pvb_expr = body;
      pvb_attributes = [];
      pvb_loc = loc;
      pvb_constraint = None }]

(* ==================== Field classification ==================== *)

type field_class =
  | Inline_atomic of string
  | Inline_time
  | Inline_unix_timestamp
  | Inline_const of string
  | Inline_custom of string
  | Inline_child of Longident.t
  | Nested_child of string * Longident.t * bool (* bool = [@view.context] *)
  | Nested_optional_child of string * Longident.t * bool
  | Nested_collection of string * Longident.t
  | Skipped

(* [patch.skip] drops the field from Patch.t, so only view kinds without a
   patch accessor can compose with it: view.skip and view.const. Any other
   view attribute would generate `fun p -> p.Patch.<field>` for a field that
   no longer exists — a confusing compile error, so reject it up front. *)
let find_incompatible_patch_skip_view (ld : label_declaration) : string option =
  let attrs = ld.pld_attributes in
  if not (has_attribute attrs "patch.skip") then None
  else
    List.find_map attrs ~f:(fun (attr : attribute) ->
        let name = attr.attr_name.txt in
        if String.equal name "patch.skip" || String.equal name "view.skip"
           || String.equal name "view.const"
        then None
        else if String.starts_with ~prefix:"view." name then Some name
        else None)

(* [@view.context] generates a section-placeholder fill, so it is only
   meaningful on [@view.child] / [@view.optional_child] fields: an inline
   atomic or collection field has no placeholder to fill, and marking one
   would silently do nothing. *)
let find_misplaced_context (ld : label_declaration) : string option =
  if not (has_context_attr ld.pld_attributes) then None
  else if has_attribute ld.pld_attributes "view.child"
       || has_attribute ld.pld_attributes "view.optional_child"
  then None
  else Some ld.pld_name.txt

let classify_field (ld : label_declaration) : field_class =
  let attrs = ld.pld_attributes in
  if has_skip_attr attrs || has_name_patch_attr attrs || has_display_attr attrs then Skipped
  else if has_attribute attrs "patch.skip"
       && not (has_attribute attrs "view.child"
               || has_attribute attrs "view.optional_child"
               || has_attribute attrs "view.collection"
               || has_attribute attrs "view.const"
               || has_inline_child_attr attrs
               || Option.is_some (get_scalar_kind attrs)
               || Option.is_some (get_custom_fn attrs)
               || Option.is_some (get_label attrs))
  then Skipped
  else
    match get_child_domain attrs with
    | Some dt ->
      (match module_path_of_type ld.pld_type with
       | Some mp -> Nested_child (dt, mp, has_context_attr attrs)
       | None -> Skipped)
    | None ->
      (match get_optional_child_domain attrs with
       | Some dt ->
         (match module_path_of_option_type ld.pld_type with
          | Some mp -> Nested_optional_child (dt, mp, has_context_attr attrs)
          | None -> Skipped)
       | None ->
         (match get_collection_domain attrs with
          | Some dt ->
            (match module_path_of_list_type ld.pld_type with
             | Some mp -> Nested_collection (dt, mp)
             | None -> Skipped)
          | None ->
            if has_inline_child_attr attrs then
              (match module_path_of_type ld.pld_type with
               | Some mp -> Inline_child mp
               | None -> Skipped)
            else
              (match get_scalar_kind attrs with
               | Some "time" -> Inline_time
               | Some "unix_timestamp" -> Inline_unix_timestamp
               | _ ->
                 if is_const_attr attrs then
                   (match is_atomic_type_name ld.pld_type with
                    | Some at -> Inline_const at
                    | None -> Skipped)
                 else
                   (match get_custom_fn attrs with
                    | Some fn -> Inline_custom fn
                    | None ->
                      (match is_atomic_type_name ld.pld_type with
                       | Some at -> Inline_atomic at
                       | None -> Skipped)))))

(* ==================== Naming info extraction ==================== *)

type naming_info = {
  name_field : string option;
  name_patch_field : string option;
  display_field : string option;
  type_label : string option;
  id_field : string option;
}

let extract_naming_info (type_decl : type_declaration) (fields : label_declaration list) : naming_info = {
  name_field = List.find_map fields ~f:(fun ld ->
      if has_name_attr ld.pld_attributes then Some ld.pld_name.txt else None);
  name_patch_field = List.find_map fields ~f:(fun ld ->
      if has_name_patch_attr ld.pld_attributes then Some ld.pld_name.txt else None);
  display_field = List.find_map fields ~f:(fun ld ->
      if has_display_attr ld.pld_attributes then Some ld.pld_name.txt else None);
  type_label = get_type_label type_decl.ptype_attributes;
  id_field = List.find_map fields ~f:(fun ld ->
      if has_attribute ld.pld_attributes "id.id" then Some ld.pld_name.txt else None);
}

(* ==================== build_section_name generation ==================== *)

let generate_build_section_name ~loc (ni : naming_info) =
  let open Ast_builder.Default in
  let label_val = match ni.type_label with Some s -> s | None -> "" in
  let c_var = pexp_ident ~loc { txt = Lident "c"; loc } in
  let v_var = pexp_ident ~loc { txt = Lident "v"; loc } in
  let p_var = pexp_ident ~loc { txt = Lident "p"; loc } in
  let tl_var = pexp_ident ~loc { txt = Lident "type_label"; loc } in

  let sprintf_expr args =
    pexp_apply ~loc (mk_lid_expr loc (Ldot (Lident "Printf", "sprintf"))) args
  in

  (* `Added v | `Removed v -> Printf.sprintf ... *)
  let added_removed_rhs =
    let name_f = Option.get ni.name_field in
    match ni.id_field with
    | Some id_f ->
      sprintf_expr [
        Nolabel, mk_str loc "%s (#%d): %s";
        Nolabel, tl_var;
        Nolabel, pexp_field ~loc v_var { loc; txt = Lident id_f };
        Nolabel, pexp_field ~loc v_var { loc; txt = Lident name_f };
      ]
    | None ->
      sprintf_expr [
        Nolabel, mk_str loc "%s: %s";
        Nolabel, tl_var;
        Nolabel, pexp_field ~loc v_var { loc; txt = Lident name_f };
      ]
  in

  (* `Modified p -> Printf.sprintf ...
     The name field (atomic_update) carries {oldval; newval}. When renamed,
     surface both as "old -> new" so the rename is visible; otherwise fall
     back to the name_patch field (an identity in the patch, plain string). *)
  let modified_rhs =
    let open Ast_builder.Default in
    (* Prefer the atomic_update name field; fall back to name_patch field. *)
    let atomic_f =
      Option.value ~default:(Option.get ni.name_patch_field) ni.name_field
    in
    let atomic_access = pexp_field ~loc p_var { loc; txt = Lident atomic_f } in
    let fallback_f = Option.get ni.name_patch_field in
    let fallback_access = pexp_field ~loc p_var { loc; txt = Lident fallback_f } in
    let old_var = pexp_ident ~loc { txt = Lident "oldval"; loc } in
    let new_var = pexp_ident ~loc { txt = Lident "newval"; loc } in
    let renamed_rhs =
      (match ni.id_field with
       | Some id_f ->
         sprintf_expr [
           Nolabel, mk_str loc "%s (#%d): %s -> %s";
           Nolabel, tl_var;
           Nolabel, pexp_field ~loc p_var { loc; txt = Lident id_f };
           Nolabel, old_var;
           Nolabel, new_var;
         ]
       | None ->
         sprintf_expr [
           Nolabel, mk_str loc "%s: %s -> %s";
           Nolabel, tl_var;
           Nolabel, old_var;
           Nolabel, new_var;
         ])
    in
    let unchanged_rhs =
      (match ni.id_field with
       | Some id_f ->
         sprintf_expr [
           Nolabel, mk_str loc "%s (#%d): %s";
           Nolabel, tl_var;
           Nolabel, pexp_field ~loc p_var { loc; txt = Lident id_f };
           Nolabel, fallback_access;
         ]
       | None ->
         sprintf_expr [
           Nolabel, mk_str loc "%s: %s";
           Nolabel, tl_var;
           Nolabel, fallback_access;
         ])
    in
    let renamed_case = {
      Parsetree.pc_lhs =
        ppat_variant ~loc "Modified"
          (Some (ppat_record ~loc [
               { txt = Lident "oldval"; loc }, ppat_var ~loc { txt = "oldval"; loc };
               { txt = Lident "newval"; loc }, ppat_var ~loc { txt = "newval"; loc };
             ] Closed));
      pc_guard = None;
      pc_rhs = renamed_rhs;
    } in
    let unchanged_case = {
      Parsetree.pc_lhs = ppat_variant ~loc "Unchanged" None;
      pc_guard = None;
      pc_rhs = unchanged_rhs;
    } in
    pexp_match ~loc atomic_access [renamed_case; unchanged_case]
  in

  let cases = [
    { Parsetree.pc_lhs = ppat_or ~loc
          (ppat_variant ~loc "Added" (Some (ppat_var ~loc { txt = "v"; loc })))
          (ppat_variant ~loc "Removed" (Some (ppat_var ~loc { txt = "v"; loc })));
      pc_guard = None;
      pc_rhs = added_removed_rhs };
    { Parsetree.pc_lhs = ppat_variant ~loc "Modified" (Some (ppat_var ~loc { txt = "p"; loc }));
      pc_guard = None;
      pc_rhs = modified_rhs };
    { Parsetree.pc_lhs = ppat_variant ~loc "Unchanged" None;
      pc_guard = None;
      pc_rhs = tl_var };
  ] in

  let match_body = pexp_match ~loc c_var cases in
  let c_pat =
    let open Ast_builder.Default in
    ppat_constraint ~loc
      (ppat_var ~loc { txt = "c"; loc })
      (ptyp_constr ~loc
         { loc; txt = Ldot (Ldot (Lident "Alsdiff_base", "Diff"), "structured_change") }
         [ ptyp_constr ~loc { loc; txt = Lident "t" } []
         ; ptyp_constr ~loc { loc; txt = Ldot (Lident "Patch", "t") } [] ])
  in
  let with_c =
    pexp_fun ~loc Nolabel None c_pat match_body
  in
  let with_type_label =
    pexp_fun ~loc (Optional "type_label")
      (Some (mk_str loc label_val))
      (ppat_var ~loc { txt = "type_label"; loc })
      with_c
  in
  pstr_value ~loc Nonrecursive [{
      pvb_pat = ppat_var ~loc { txt = "build_section_name"; loc };
      pvb_expr = with_type_label;
      pvb_attributes = [];
      pvb_loc = loc;
      pvb_constraint = None }]

(* ==================== Builder collection spec generator ==================== *)

let generate_builder_collection_spec ~loc field_name label domain_type_name builder_name =
  let open Ast_builder.Default in
  pexp_apply ~loc (mk_lid_expr loc (Ldot (Ldot (Lident "B", "Spec"), "collection")))
    [ Labelled "name", mk_str loc label
    ; Labelled "of_value", generate_value_accessor ~loc field_name
    ; Labelled "of_patch", generate_patch_accessor ~loc field_name
    ; Labelled "build_item", pexp_ident ~loc { txt = Lident builder_name; loc }
    ; Labelled "domain_type", pexp_apply ~loc (mk_lid_expr loc (Ldot (Lident "B", "domain_type_of_name")))
        [ Nolabel, mk_str loc domain_type_name ] ]

(* ==================== Main code generation ==================== *)

let generate_specs_from_fields ~loc fields =
  let has_time_field = List.exists fields ~f:(fun ld ->
      match classify_field ld with Inline_time -> true | _ -> false) in
  let field_specs = List.filter_map fields ~f:(fun ld ->
      let fname = ld.pld_name.txt in
      let label = get_field_label fname ld.pld_attributes in
      match classify_field ld with
      | Inline_atomic at -> Some (generate_atomic_field_spec ~loc label fname (make_maker_name at))
      | Inline_time -> Some (generate_time_field_spec ~loc label fname)
      | Inline_unix_timestamp -> Some (generate_unix_timestamp_field_spec ~loc label fname)
      | Inline_const at -> Some (generate_const_field_spec ~loc label fname at)
      | Inline_custom fn -> Some (generate_custom_field_spec ~loc label fname fn)
      | _ -> None) in
  let builder_fields = List.filter_map fields ~f:(fun ld ->
      match (classify_field ld, get_builder_name ld.pld_attributes) with
      | Nested_collection _, Some bname -> Some (ld.pld_name.txt, bname)
      | _ -> None) in
  let child_section_specs = List.filter_map fields ~f:(fun ld ->
      let fname = ld.pld_name.txt in
      let label = get_field_label fname ld.pld_attributes in
      match classify_field ld with
      | Nested_child (dt, mp, ctx) -> Some (generate_child_spec ~loc fname label dt mp ~context:ctx)
      | Nested_optional_child (dt, mp, ctx) ->
        Some (generate_optional_child_spec ~loc fname label dt mp ~context:ctx)
      | Nested_collection (dt, mp) ->
        (match get_builder_name ld.pld_attributes with
         | Some bname -> Some (generate_builder_collection_spec ~loc fname label dt bname)
         | None -> Some (generate_collection_spec ~loc fname label dt mp))
      | _ -> None) in
  let inline_child_fields = List.filter_map fields ~f:(fun ld ->
      match classify_field ld with
      | Inline_child mp -> Some (ld.pld_name.txt, mp)
      | _ -> None) in
  (* format_time is always threaded: every generated binding (field_specs,
     section_specs, build_value_fields, build_patch_fields, build_item) binds
     ~format_time so a parent's child-spec generator can unconditionally call
     Vs.build_value_fields ~format_time ... regardless of whether the child
     type itself has time fields. Only [field_specs] can bind ~format_time
     without referencing it (when this type has no Inline_time field), so the
     warning-27 (unused-variable) suppression is scoped to that single binding
     in [generate_view_spec_impl] rather than blanket-disabled across the
     functor — a dropped ~format_time reference in any other binding (or in a
     time-bearing field_specs) then still trips warning 27 at compile time.
     Inline children are spliced via flat field lists: each child ViewSpec is
     instantiated once per parent and its field_specs applied to the threaded
     ~format_time, so they are excluded from the build_* threading above. *)
  (has_time_field, field_specs, child_section_specs, inline_child_fields, builder_fields)

(* ==================== Variant dispatch ViewSpec generation ==================== *)

(* Variant dispatch emitter. In BOTH modes the PPX only transcribes the
   table — the generated match's exhaustiveness (every constructor covered)
   and the referenced names are checked by the compiler downstream.

   Self-routed (Task 3): the sum's own ViewSpec functor delegates each
   change arm to the variant module's ViewSpec named in the routing table.
   The `` `Unchanged `` arm delegates to the FIRST entry's ViewSpec with
   ~name set to the type's [@@view.type_label] string: build_item_from_specs
   on `Unchanged yields {name; change=Unchanged; domain_type; children=[]},
   identical to the hand-written placeholder arm.

   Builder-routed (Task 5): one labelled builder arg per ROUTED entry
   (vmodule <> ""), in table order; `Added/`Removed/`Modified arms re-tag
   the payload to the variant module's structured_change and hand it to
   that entry's builder, returning Some. Skip entries (vmodule = "") get
   NO builder arg — validation rejects a skip entry carrying a builder
   label (the emitter would ignore it, but silently is worse than loud) —
   and their arms collapse with `Unchanged into one final -> None arm. No
   ~format_time parameter: builders arrive as pre-applied closures. *)
let generate_variant_dispatch ~type_decl ~vd ~builder_routed : structure =
  let open Ast_builder.Default in
  let loc = type_decl.ptype_loc in
  if builder_routed then
    let sc_lid = Ldot (Ldot (Lident "Alsdiff_base", "Diff"), "structured_change") in
    let case pat rhs = { Parsetree.pc_lhs = pat; pc_guard = None; pc_rhs = rhs } in
    let routed = List.filter vd.vd_entries ~f:(fun e -> e.vmodule <> "") in
    let skips = List.filter vd.vd_entries ~f:(fun e -> e.vmodule = "") in
    let v_var = pexp_ident ~loc { txt = Lident "v"; loc } in
    let p_var = pexp_ident ~loc { txt = Lident "p"; loc } in
    (* Some (build_x (`Tag payload)) *)
    let retag e tag payload =
      pexp_construct ~loc { txt = Lident "Some"; loc }
        (Some (pexp_apply ~loc (pexp_ident ~loc { txt = Lident e.vbuilder; loc })
             [ Nolabel, pexp_variant ~loc tag (Some payload) ]))
    in
    (* `Tag (<ctor> <payload-pat>) *)
    let value_pat e tag payload_pat =
      ppat_variant ~loc tag
        (Some (ppat_construct ~loc { loc; txt = Lident e.vctor } (Some payload_pat)))
    in
    (* `Modified (Patch.<vpatch> <payload-pat>) *)
    let patch_pat e payload_pat =
      ppat_variant ~loc "Modified"
        (Some (ppat_construct ~loc { loc; txt = Ldot (Lident "Patch", e.vpatch) }
                 (Some payload_pat)))
    in
    (* Routed entry: `Added/`Removed arms, plus a `Modified arm iff the entry
       claims a patch constructor (Return shape has none). *)
    let routed_cases e =
      [ case (value_pat e "Added" (ppat_var ~loc { txt = "v"; loc })) (retag e "Added" v_var)
      ; case (value_pat e "Removed" (ppat_var ~loc { txt = "v"; loc })) (retag e "Removed" v_var) ]
      @ (if e.vpatch = "" then []
         else [ case (patch_pat e (ppat_var ~loc { txt = "p"; loc })) (retag e "Modified" p_var) ])
    in
    (* Skip entry: wildcard-payload patterns feeding the shared None arm. *)
    let skip_pats e =
      [ value_pat e "Added" (ppat_any ~loc); value_pat e "Removed" (ppat_any ~loc) ]
      @ (if e.vpatch = "" then [] else [ patch_pat e (ppat_any ~loc) ])
    in
    (* One final arm: every skip pattern plus `Unchanged -> None. *)
    let final_case =
      let pats = List.concat_map skips ~f:skip_pats @ [ ppat_variant ~loc "Unchanged" None ] in
      match pats with
      | [] -> assert false (* unreachable: `Unchanged is always appended *)
      | first :: rest ->
        case (List.fold_left rest ~init:first ~f:(ppat_or ~loc))
          (pexp_construct ~loc { txt = Lident "None"; loc } None)
    in
    let cases = List.concat_map routed ~f:routed_cases @ [ final_case ] in
    let c_pat =
      ppat_constraint ~loc
        (ppat_var ~loc { txt = "c"; loc })
        (ptyp_constr ~loc { loc; txt = sc_lid }
           [ ptyp_constr ~loc { loc; txt = Lident "t" } []
           ; ptyp_constr ~loc { loc; txt = Ldot (Lident "Patch", "t") } [] ])
    in
    let match_expr =
      pexp_constraint ~loc
        (pexp_match ~loc (pexp_ident ~loc { txt = Lident "c"; loc }) cases)
        (ptyp_constr ~loc { loc; txt = Lident "option" }
           [ ptyp_constr ~loc { loc; txt = Ldot (Lident "B", "item") } [] ])
    in
    let with_c = pexp_fun ~loc Nolabel None c_pat match_expr in
    (* ~(build_x : (<vmodule>.t, <vmodule>.Patch.t) structured_change -> B.item),
       one per routed entry, wrapped in table order. *)
    let builder_arg_ty e =
      ptyp_arrow ~loc Nolabel
        (ptyp_constr ~loc { loc; txt = sc_lid }
           [ ptyp_constr ~loc { loc; txt = Ldot (Lident e.vmodule, "t") } []
           ; ptyp_constr ~loc { loc; txt = Ldot (Ldot (Lident e.vmodule, "Patch"), "t") } [] ])
        (ptyp_constr ~loc { loc; txt = Ldot (Lident "B", "item") } [])
    in
    let expr =
      List.fold_left (List.rev routed) ~init:with_c
        ~f:(fun acc e ->
            pexp_fun ~loc (Labelled e.vbuilder) None
              (ppat_constraint ~loc (ppat_var ~loc { txt = e.vbuilder; loc })
                 (builder_arg_ty e))
              acc)
    in
    let build_item_binding =
      pstr_value ~loc Nonrecursive [{
        pvb_pat = ppat_var ~loc { txt = "build_item"; loc };
        pvb_expr = expr;
        pvb_attributes = [];
        pvb_loc = loc;
        pvb_constraint = None }]
    in
    let b_sig = pmty_ident ~loc { loc; txt = Longident.parse "Alsdiff_view_spec_types.View_spec_types.S" } in
    let functor_param = Named ({ txt = Some "B"; loc }, b_sig) in
    let body_mod = pmod_structure ~loc [build_item_binding] in
    let functor_mod = pmod_functor ~loc functor_param body_mod in
    [pstr_module ~loc {
        pmb_name = { txt = Some "ViewSpec"; loc };
        pmb_expr = functor_mod;
        pmb_attributes = [];
        pmb_loc = loc }]
  else
    match (vd.vd_entries, get_type_label type_decl.ptype_attributes) with
    | first :: _, Some type_label ->
      let fmt_var = pexp_ident ~loc { txt = Lident "format_time"; loc } in
      let dt_var = pexp_ident ~loc { txt = Lident "domain_type"; loc } in
      (* module VS_<ctor> = <vmodule>.ViewSpec (B) per table entry *)
      let alias_of e =
        pmod_apply ~loc
          (pmod_ident ~loc { loc; txt = Ldot (Lident e.vmodule, "ViewSpec") })
          (pmod_ident ~loc { loc; txt = Lident "B" })
      in
      let alias_items = List.map vd.vd_entries ~f:(fun e ->
          pstr_module ~loc {
            pmb_name = { txt = Some ("VS_" ^ e.vctor); loc };
            pmb_expr = alias_of e;
            pmb_attributes = [];
            pmb_loc = loc }) in
      (* VS_<ctor>.build_item ~format_time
         ~name:(VS_<ctor>.build_section_name (<tag> x)) ~domain_type (<tag> x) *)
      let dispatch_call alias tag arg =
        pexp_apply ~loc
          (pexp_ident ~loc { loc; txt = Ldot (Lident alias, "build_item") })
          [ Labelled "format_time", fmt_var
          ; Labelled "name",
            pexp_apply ~loc
              (pexp_ident ~loc { loc; txt = Ldot (Lident alias, "build_section_name") })
              [ Nolabel, pexp_variant ~loc tag (Some arg) ]
          ; Labelled "domain_type", dt_var
          ; Nolabel, pexp_variant ~loc tag (Some arg) ]
      in
      let case pat rhs = { Parsetree.pc_lhs = pat; pc_guard = None; pc_rhs = rhs } in
      (* Per entry: `Added (<ctor> v) / `Removed (<ctor> v) / — only when the
         entry claims a patch constructor — `Modified (Patch.<vpatch> p). *)
      let entry_cases e =
        let alias = "VS_" ^ e.vctor in
        let value_pat tag =
          ppat_variant ~loc tag
            (Some (ppat_construct ~loc { loc; txt = Lident e.vctor }
                     (Some (ppat_var ~loc { txt = "v"; loc }))))
        in
        let added_removed =
          [ case (value_pat "Added")
              (dispatch_call alias "Added" (pexp_ident ~loc { txt = Lident "v"; loc }))
          ; case (value_pat "Removed")
              (dispatch_call alias "Removed" (pexp_ident ~loc { txt = Lident "v"; loc })) ]
        in
        let modified =
          if e.vpatch = "" then []
          else
            [ case
                  (ppat_variant ~loc "Modified"
                     (Some (ppat_construct ~loc { loc; txt = Ldot (Lident "Patch", e.vpatch) }
                              (Some (ppat_var ~loc { txt = "p"; loc })))))
                  (dispatch_call alias "Modified" (pexp_ident ~loc { txt = Lident "p"; loc })) ]
        in
        added_removed @ modified
      in
      let unchanged_case =
        case (ppat_variant ~loc "Unchanged" None)
          (pexp_apply ~loc
             (pexp_ident ~loc { loc; txt = Ldot (Lident ("VS_" ^ first.vctor), "build_item") })
             [ Labelled "format_time", fmt_var
             ; Labelled "name", mk_str loc type_label
             ; Labelled "domain_type", dt_var
             ; Nolabel, pexp_variant ~loc "Unchanged" None ])
      in
      let cases = List.concat_map vd.vd_entries ~f:entry_cases @ [unchanged_case] in
      let c_pat =
        ppat_constraint ~loc
          (ppat_var ~loc { txt = "c"; loc })
          (ptyp_constr ~loc
             { loc; txt = Ldot (Ldot (Lident "Alsdiff_base", "Diff"), "structured_change") }
             [ ptyp_constr ~loc { loc; txt = Lident "t" } []
             ; ptyp_constr ~loc { loc; txt = Ldot (Lident "Patch", "t") } [] ])
      in
      let match_expr =
        pexp_constraint ~loc
          (pexp_match ~loc (pexp_ident ~loc { txt = Lident "c"; loc }) cases)
          (ptyp_constr ~loc { loc; txt = Ldot (Lident "B", "item") } [])
      in
      let with_c = pexp_fun ~loc Nolabel None c_pat match_expr in
      let with_dt =
        pexp_fun ~loc (Optional "domain_type")
          (Some (pexp_apply ~loc (mk_lid_expr loc (Ldot (Lident "B", "domain_type_of_name")))
                   [ Nolabel, mk_str loc vd.vd_domain ]))
          (ppat_var ~loc { txt = "domain_type"; loc })
          with_c
      in
      let with_ft =
        pexp_fun ~loc (Labelled "format_time") None
          (ppat_var ~loc { txt = "format_time"; loc })
          with_dt
      in
      let build_item_binding =
        pstr_value ~loc Nonrecursive [{
          pvb_pat = ppat_var ~loc { txt = "build_item"; loc };
          pvb_expr = with_ft;
          pvb_attributes = [];
          pvb_loc = loc;
          pvb_constraint = None }]
      in
      let b_sig = pmty_ident ~loc { loc; txt = Longident.parse "Alsdiff_view_spec_types.View_spec_types.S" } in
      let functor_param = Named ({ txt = Some "B"; loc }, b_sig) in
      let body_mod = pmod_structure ~loc (alias_items @ [build_item_binding]) in
      let functor_mod = pmod_functor ~loc functor_param body_mod in
      [pstr_module ~loc {
          pmb_name = { txt = Some "ViewSpec"; loc };
          pmb_expr = functor_mod;
          pmb_attributes = [];
          pmb_loc = loc }]
    | _ ->
      (* Unreachable: validate_variant_dispatch rejects empty tables and, in
         self-routed mode, a missing type_label, before this runs. *)
      assert false

let generate_view_spec_for_decl ~type_decl =
  let open Ast_builder.Default in
  let loc = type_decl.ptype_loc in
  match type_decl.ptype_kind with
  | Ptype_record _ | Ptype_open when
      has_variant_dispatch_attr type_decl.ptype_attributes ->
    let ext = Ppxlib.Location.error_extensionf ~loc
        "view.variant_dispatch is for variant (sum) types, not records" in
    [pstr_extension ~loc ext []]
  | Ptype_variant _ | Ptype_abstract when
      has_variant_dispatch_attr type_decl.ptype_attributes ->
    (match parse_variant_dispatch type_decl.ptype_attributes with
     | Error msg ->
       let ext = Ppxlib.Location.error_extensionf ~loc "%s" msg in
       [pstr_extension ~loc ext []]
     | Ok vd ->
       (match validate_variant_dispatch
                ~type_label:(get_type_label type_decl.ptype_attributes) vd with
        | Error msg ->
          let ext = Ppxlib.Location.error_extensionf ~loc "%s" msg in
          [pstr_extension ~loc ext []]
        | Ok builder_routed ->
          generate_variant_dispatch ~type_decl ~vd ~builder_routed))
  | Ptype_record fields ->
    (match List.find_map ~f:find_incompatible_patch_skip_view fields with
     | Some attr_name ->
       let ext = Ppxlib.Location.error_extensionf ~loc
           "Field with [@patch.skip] cannot also carry [%s]: patch.skip removes \
            the field from Patch.t, so no patch accessor can be generated (only \
            view.skip and view.const compose with patch.skip)" attr_name in
       [pstr_extension ~loc ext []]
     | None ->
       (match List.find_map ~f:find_misplaced_context fields with
        | Some field_name ->
          let ext = Ppxlib.Location.error_extensionf ~loc
              "Field %s carries [@view.context] but not [@view.child] or \
               [@view.optional_child]: there is no section placeholder to fill" field_name in
          [pstr_extension ~loc ext []]
        | None ->
          let (has_time_field, field_specs_exprs, child_section_specs, inline_child_fields, builder_fields) =
            generate_specs_from_fields ~loc fields
          in
          let ni = extract_naming_info type_decl fields in
          let has_naming = ni.name_field <> None && ni.name_patch_field <> None && ni.type_label <> None in

          (* --- inline_child bindings --- *)
          let inline_bindings = List.map inline_child_fields ~f:(fun (fname, mp) ->
              generate_inline_child_binding ~loc fname mp) in

          (* --- field_specs binding --- *)
          let field_specs_base = mk_list_expr loc field_specs_exprs in
          let field_specs_list =
            List.fold_left inline_child_fields ~init:field_specs_base
              ~f:(fun acc (fname, _) ->
                  let inline_ref =
                    pexp_apply ~loc
                      (pexp_ident ~loc { txt = Lident ("__inline_" ^ fname); loc })
                      [ Labelled "format_time", pexp_ident ~loc { txt = Lident "format_time"; loc } ]
                  in
                  pexp_apply ~loc
                    (pexp_ident ~loc { loc; txt = Ldot (Lident "List", "append") })
                    [ Nolabel, acc; Nolabel, inline_ref ])
          in
          let field_specs_binding =
            let expr =
              pexp_fun ~loc (Labelled "format_time")
                None
                (ppat_var ~loc { txt = "format_time"; loc })
                field_specs_list
            in
            pstr_value ~loc Nonrecursive [{
                pvb_pat = ppat_var ~loc { txt = "field_specs"; loc };
                pvb_expr = expr;
                pvb_attributes = [];
                pvb_loc = loc;
                pvb_constraint = None }]
          in

          (* --- section_specs binding --- *)
          let specs_arg =
            pexp_apply ~loc (pexp_ident ~loc { txt = Lident "field_specs"; loc })
              [Labelled "format_time", pexp_ident ~loc { txt = Lident "format_time"; loc }]
          in
          let inline_section =
            pexp_apply ~loc (mk_lid_expr loc (Ldot (Ldot (Lident "B", "Spec"), "inline_fields")))
              [ Labelled "specs", specs_arg
              ; Labelled "domain_type", mk_lid_expr loc (Ldot (Lident "B", "default_domain_type")) ]
          in
          let all_sections = inline_section :: child_section_specs in
          let section_specs_list = mk_list_expr loc all_sections in
          let section_specs_binding =
            let expr =
              let base =
                pexp_fun ~loc (Labelled "format_time")
                  None
                  (ppat_var ~loc { txt = "format_time"; loc })
                  section_specs_list
              in
              List.fold_left builder_fields ~init:base
                ~f:(fun acc (_, bname) ->
                    pexp_fun ~loc (Labelled bname)
                      None
                      (ppat_var ~loc { txt = bname; loc })
                      acc)
            in
            pstr_value ~loc Nonrecursive [{
                pvb_pat = ppat_var ~loc { txt = "section_specs"; loc };
                pvb_expr = expr;
                pvb_attributes = [];
                pvb_loc = loc;
                pvb_constraint = None }]
          in

          (* --- build_value_fields binding --- *)
          let build_value_fields_binding =
            let fs_call =
              pexp_apply ~loc (pexp_ident ~loc { txt = Lident "field_specs"; loc })
                [Labelled "format_time", pexp_ident ~loc { txt = Lident "format_time"; loc }]
            in
            let body =
              pexp_apply ~loc (mk_lid_expr loc (Ldot (Lident "B", "build_value_field_views")))
                [ Nolabel, fs_call
                ; Nolabel, pexp_ident ~loc { txt = Lident "ct"; loc }
                ; Nolabel, pexp_ident ~loc { txt = Lident "v"; loc }
                ; Labelled "domain_type", pexp_ident ~loc { txt = Lident "domain_type"; loc } ]
            in
            let inner =
              pexp_fun ~loc Nolabel None
                (ppat_var ~loc { txt = "ct"; loc })
                (pexp_fun ~loc Nolabel None
                   (ppat_var ~loc { txt = "v"; loc })
                   body)
            in
            let inner2 =
              pexp_fun ~loc (Optional "domain_type")
                (Some (mk_lid_expr loc (Ldot (Lident "B", "default_domain_type"))))
                (ppat_var ~loc { txt = "domain_type"; loc })
                inner
            in
            let expr =
              pexp_fun ~loc (Labelled "format_time")
                None
                (ppat_var ~loc { txt = "format_time"; loc })
                inner2
            in
            pstr_value ~loc Nonrecursive [{
                pvb_pat = ppat_var ~loc { txt = "build_value_fields"; loc };
                pvb_expr = expr;
                pvb_attributes = [];
                pvb_loc = loc;
                pvb_constraint = None }]
          in

          (* --- build_patch_fields binding --- *)
          let build_patch_fields_binding =
            let fs_call =
              pexp_apply ~loc (pexp_ident ~loc { txt = Lident "field_specs"; loc })
                [Labelled "format_time", pexp_ident ~loc { txt = Lident "format_time"; loc }]
            in
            let body =
              pexp_apply ~loc (mk_lid_expr loc (Ldot (Lident "B", "build_patch_field_views")))
                [ Nolabel, fs_call
                ; Nolabel, pexp_ident ~loc { txt = Lident "p"; loc }
                ; Labelled "domain_type", pexp_ident ~loc { txt = Lident "domain_type"; loc } ]
            in
            let inner =
              pexp_fun ~loc Nolabel None
                (ppat_var ~loc { txt = "p"; loc })
                body
            in
            let inner2 =
              pexp_fun ~loc (Optional "domain_type")
                (Some (mk_lid_expr loc (Ldot (Lident "B", "default_domain_type"))))
                (ppat_var ~loc { txt = "domain_type"; loc })
                inner
            in
            let expr =
              pexp_fun ~loc (Labelled "format_time")
                None
                (ppat_var ~loc { txt = "format_time"; loc })
                inner2
            in
            pstr_value ~loc Nonrecursive [{
                pvb_pat = ppat_var ~loc { txt = "build_patch_fields"; loc };
                pvb_expr = expr;
                pvb_attributes = [];
                pvb_loc = loc;
                pvb_constraint = None }]
          in

          (* --- build_item binding --- *)
          let build_item_binding =
            let specs_call_args =
              let time_args =
                [Labelled "format_time", pexp_ident ~loc { txt = Lident "format_time"; loc }] in
              let builder_args = List.map builder_fields ~f:(fun (_, bname) ->
                  Labelled bname, pexp_ident ~loc { txt = Lident bname; loc }) in
              time_args @ builder_args
            in
            let specs_arg =
              if specs_call_args = [] then
                pexp_ident ~loc { txt = Lident "section_specs"; loc }
              else
                pexp_apply ~loc (pexp_ident ~loc { txt = Lident "section_specs"; loc })
                  specs_call_args
            in
            let body =
              pexp_apply ~loc (mk_lid_expr loc (Ldot (Lident "B", "build_item_from_specs")))
                [ Labelled "name", pexp_ident ~loc { txt = Lident "name"; loc }
                ; Labelled "domain_type", pexp_ident ~loc { txt = Lident "domain_type"; loc }
                ; Labelled "specs", specs_arg
                ; Nolabel, pexp_ident ~loc { txt = Lident "c"; loc } ]
            in
            let inner =
              pexp_fun ~loc Nolabel None
                (ppat_var ~loc { txt = "c"; loc })
                body
            in
            let inner2 =
              pexp_fun ~loc (Optional "domain_type")
                (Some (mk_lid_expr loc (Ldot (Lident "B", "default_domain_type"))))
                (ppat_var ~loc { txt = "domain_type"; loc })
                inner
            in
            let inner3 =
              pexp_fun ~loc (Optional "name")
                (Some (mk_str loc ""))
                (ppat_var ~loc { txt = "name"; loc })
                inner2
            in
            let expr =
              let base =
                pexp_fun ~loc (Labelled "format_time")
                  None
                  (ppat_var ~loc { txt = "format_time"; loc })
                  inner3
              in
              List.fold_left (List.rev builder_fields) ~init:base
                ~f:(fun acc (_, bname) ->
                    pexp_fun ~loc (Labelled bname)
                      None
                      (ppat_var ~loc { txt = bname; loc })
                      acc)
            in
            pstr_value ~loc Nonrecursive [{
                pvb_pat = ppat_var ~loc { txt = "build_item"; loc };
                pvb_expr = expr;
                pvb_attributes = [];
                pvb_loc = loc;
                pvb_constraint = None }]
          in

          (* --- fill_context binding (TODO item 4): the context-marked child
             specs' fills applied to a Modified item's children. Needs no
             builders — collections are never context-marked — so it applies
             to builder-bearing types (MidiClip, tracks) too. Unmarked types
             get the S-conformant no-op. --- *)
          let fill_context_binding =
            let context_spec_exprs = List.filter_map fields ~f:(fun ld ->
                let fname = ld.pld_name.txt in
                let label = get_field_label fname ld.pld_attributes in
                match classify_field ld with
                | Nested_child (dt, mp, true) ->
                  Some (generate_child_spec ~loc fname label dt mp ~context:true)
                | Nested_optional_child (dt, mp, true) ->
                  Some (generate_optional_child_spec ~loc fname label dt mp ~context:true)
                | _ -> None)
            in
            let expr =
              if context_spec_exprs = [] then
                (* No-op: fully polymorphic, conforms to S. The labelled
                   wildcard [~format_time:_] avoids warning 27 without an
                   attribute dance. *)
                pexp_fun ~loc (Labelled "format_time") None (ppat_any ~loc)
                  (pexp_fun ~loc Nolabel None (ppat_any ~loc)
                     (pexp_fun ~loc Nolabel None (ppat_var ~loc { txt = "item"; loc })
                        (pexp_ident ~loc { txt = Lident "item"; loc })))
              else
                pexp_fun ~loc (Labelled "format_time") None (ppat_var ~loc { txt = "format_time"; loc })
                  (pexp_fun ~loc Nolabel None (ppat_var ~loc { txt = "old"; loc })
                     (pexp_fun ~loc Nolabel None (ppat_var ~loc { txt = "item"; loc })
                        (pexp_apply ~loc (mk_lid_expr loc (Ldot (Lident "B", "fill_section_context")))
                           [ Nolabel, mk_list_expr loc context_spec_exprs
                           ; Nolabel, pexp_ident ~loc { txt = Lident "old"; loc }
                           ; Nolabel, pexp_ident ~loc { txt = Lident "item"; loc } ])))
            in
            pstr_value ~loc Nonrecursive [{
                pvb_pat = ppat_var ~loc { txt = "fill_context"; loc };
                pvb_expr = expr;
                pvb_attributes = [];
                pvb_loc = loc;
                pvb_constraint = None }]
          in

          (* --- build_value_children / build_patch_children bindings ---
             These render the FULL child section (all sub-views via section_specs),
             returning the item's children. They back the generated [@view.child]
             specs so a child with nested [@view.child] fields (e.g. Mixer) renders
             its whole subtree, not just inline atomic fields.

             The full-section path needs no builders, so it is only emitted when this
             type has none. Types WITH [@view.builder] collections (e.g. MidiClip)
             are never [@view.child] targets (they're collection elements), so their
             children-binding falls back to the inline-field views — sufficient to
             satisfy the functor signature. *)
          let (build_value_children_binding, build_patch_children_binding) =
            if builder_fields = [] then begin
              (* Full-section path: reuse build_item (captures section_specs), then
                 extract .children via B.item_children. *)
              let item_of_change c_expr =
                pexp_apply ~loc (pexp_ident ~loc { txt = Lident "build_item"; loc })
                  [ Labelled "format_time", pexp_ident ~loc { txt = Lident "format_time"; loc }
                  ; Labelled "domain_type", pexp_ident ~loc { txt = Lident "domain_type"; loc }
                  ; Nolabel, c_expr ]
              in
              let mk_case pat c_expr =
                { pc_lhs = pat; pc_guard = None; pc_rhs = c_expr }
              in
              let vc_body =
                pexp_apply ~loc (mk_lid_expr loc (Ldot (Lident "B", "item_children")))
                  [ Nolabel,
                    pexp_match ~loc (pexp_ident ~loc { txt = Lident "ct"; loc })
                      [ mk_case
                          (ppat_construct ~loc { txt = Ldot (Lident "B", "Added"); loc } None)
                          (item_of_change (pexp_variant ~loc "Added"
                                             (Some (pexp_ident ~loc { txt = Lident "v"; loc }))))
                      ; mk_case
                          (ppat_construct ~loc { txt = Ldot (Lident "B", "Removed"); loc } None)
                          (item_of_change (pexp_variant ~loc "Removed"
                                             (Some (pexp_ident ~loc { txt = Lident "v"; loc }))))
                      ; mk_case
                          (ppat_construct ~loc { txt = Ldot (Lident "B", "Modified"); loc } None)
                          (item_of_change (pexp_variant ~loc "Added"
                                             (Some (pexp_ident ~loc { txt = Lident "v"; loc }))))
                      ; mk_case
                          (ppat_construct ~loc { txt = Ldot (Lident "B", "Unchanged"); loc } None)
                          (item_of_change (pexp_variant ~loc "Added"
                                             (Some (pexp_ident ~loc { txt = Lident "v"; loc }))))
                      ] ]
              in
              let vc_expr =
                pexp_fun ~loc (Labelled "format_time") None
                  (ppat_var ~loc { txt = "format_time"; loc })
                  (pexp_fun ~loc (Optional "domain_type")
                     (Some (mk_lid_expr loc (Ldot (Lident "B", "default_domain_type"))))
                     (ppat_var ~loc { txt = "domain_type"; loc })
                     (pexp_fun ~loc Nolabel None
                        (ppat_var ~loc { txt = "ct"; loc })
                        (pexp_fun ~loc Nolabel None
                           (ppat_var ~loc { txt = "v"; loc })
                           vc_body)))
              in
              let pc_body =
                pexp_apply ~loc (mk_lid_expr loc (Ldot (Lident "B", "item_children")))
                  [ Nolabel,
                    item_of_change (pexp_variant ~loc "Modified"
                                      (Some (pexp_ident ~loc { txt = Lident "p"; loc }))) ]
              in
              let pc_expr =
                pexp_fun ~loc (Labelled "format_time") None
                  (ppat_var ~loc { txt = "format_time"; loc })
                  (pexp_fun ~loc (Optional "domain_type")
                     (Some (mk_lid_expr loc (Ldot (Lident "B", "default_domain_type"))))
                     (ppat_var ~loc { txt = "domain_type"; loc })
                     (pexp_fun ~loc Nolabel None
                        (ppat_var ~loc { txt = "p"; loc })
                        pc_body))
              in
              (pstr_value ~loc Nonrecursive [{
                   pvb_pat = ppat_var ~loc { txt = "build_value_children"; loc };
                   pvb_expr = vc_expr;
                   pvb_attributes = []; pvb_loc = loc; pvb_constraint = None }],
               pstr_value ~loc Nonrecursive [{
                   pvb_pat = ppat_var ~loc { txt = "build_patch_children"; loc };
                   pvb_expr = pc_expr;
                   pvb_attributes = []; pvb_loc = loc; pvb_constraint = None }])
            end else begin
              (* Builder-bearing type (never a [@view.child] target): fall back to the
                 inline-field views to satisfy the functor signature. *)
              let fs_call =
                pexp_apply ~loc (pexp_ident ~loc { txt = Lident "field_specs"; loc })
                  [Labelled "format_time", pexp_ident ~loc { txt = Lident "format_time"; loc }]
              in
              let vc_expr =
                pexp_fun ~loc (Labelled "format_time") None
                  (ppat_var ~loc { txt = "format_time"; loc })
                  (pexp_fun ~loc (Optional "domain_type")
                     (Some (mk_lid_expr loc (Ldot (Lident "B", "default_domain_type"))))
                     (ppat_var ~loc { txt = "domain_type"; loc })
                     (pexp_fun ~loc Nolabel None
                        (ppat_var ~loc { txt = "ct"; loc })
                        (pexp_fun ~loc Nolabel None
                           (ppat_var ~loc { txt = "v"; loc })
                           (pexp_apply ~loc (mk_lid_expr loc (Ldot (Lident "B", "build_value_field_views")))
                              [ Nolabel, fs_call
                              ; Nolabel, pexp_ident ~loc { txt = Lident "ct"; loc }
                              ; Nolabel, pexp_ident ~loc { txt = Lident "v"; loc }
                              ; Labelled "domain_type", pexp_ident ~loc { txt = Lident "domain_type"; loc } ]))))
              in
              let pc_expr =
                pexp_fun ~loc (Labelled "format_time") None
                  (ppat_var ~loc { txt = "format_time"; loc })
                  (pexp_fun ~loc (Optional "domain_type")
                     (Some (mk_lid_expr loc (Ldot (Lident "B", "default_domain_type"))))
                     (ppat_var ~loc { txt = "domain_type"; loc })
                     (pexp_fun ~loc Nolabel None
                        (ppat_var ~loc { txt = "p"; loc })
                        (pexp_apply ~loc (mk_lid_expr loc (Ldot (Lident "B", "build_patch_field_views")))
                           [ Nolabel, fs_call
                           ; Nolabel, pexp_ident ~loc { txt = Lident "p"; loc }
                           ; Labelled "domain_type", pexp_ident ~loc { txt = Lident "domain_type"; loc } ])))
              in
              (pstr_value ~loc Nonrecursive [{
                   pvb_pat = ppat_var ~loc { txt = "build_value_children"; loc };
                   pvb_expr = vc_expr;
                   pvb_attributes = []; pvb_loc = loc; pvb_constraint = None }],
               pstr_value ~loc Nonrecursive [{
                   pvb_pat = ppat_var ~loc { txt = "build_patch_children"; loc };
                   pvb_expr = pc_expr;
                   pvb_attributes = []; pvb_loc = loc; pvb_constraint = None }])
            end
          in

          (* --- build_section_name binding --- *)
          let build_section_name_binding =
            if has_naming then [generate_build_section_name ~loc ni] else []
          in

          (* --- functor module --- *)
          let b_sig = pmty_ident ~loc { loc; txt = Longident.parse "Alsdiff_view_spec_types.View_spec_types.S" } in
          let functor_param = Named ({ txt = Some "B"; loc }, b_sig) in
          (* [field_specs] binds ~format_time but only references it when this type
             has an Inline_time field. For time-less types the binding would trip
             warning 27 (unused-variable), so scope the suppression to that one
             binding — disable it, emit [field_specs], then re-enable — rather than
             blanket-disabling warning 27 across the whole functor body. Any other
             binding that drops its ~format_time reference then still warns. *)
          let warning_item txt =
            pstr_attribute ~loc
              { attr_name = { txt = "warning"; loc }
              ; attr_payload = PStr [pstr_eval ~loc (mk_str loc txt) []]
              ; attr_loc = loc }
          in
          let field_specs_items =
            (if has_time_field then [] else [warning_item "-27"])
            @ [field_specs_binding]
            @ (if has_time_field then [] else [warning_item "+27"])
          in
          let body_mod = pmod_structure ~loc (
              inline_bindings @ build_section_name_binding @ field_specs_items @ [
                section_specs_binding;
                build_value_fields_binding;
                build_patch_fields_binding;
                build_item_binding;
                fill_context_binding;
                build_value_children_binding;
                build_patch_children_binding;
              ]) in
          let functor_mod = pmod_functor ~loc functor_param body_mod in
          [pstr_module ~loc {
              pmb_name = { txt = Some "ViewSpec"; loc };
              pmb_expr = functor_mod;
              pmb_attributes = [];
              pmb_loc = loc }]))
  | _ ->
    (* Mirror ppx_patch: fail loudly instead of silently emitting an empty
       ViewSpec (a populated-but-empty spec would compile and contribute zero
       views). *)
    let ext = Ppxlib.Location.error_extensionf ~loc
        "Cannot derive view_spec for non-record types" in
    [Ast_builder.Default.pstr_extension ~loc ext []]

let generate_view_spec_impl ~ctxt:_ (_rec_flag, type_decls) =
  let open Ast_builder.Default in
  match type_decls with
  | [] -> []
  | [type_decl] -> generate_view_spec_for_decl ~type_decl
  | type_decl :: _ ->
    (* And-groups would collide on the generated [ViewSpec] module name (one
       per declaration), so they are rejected instead of silently generating
       for the first type only (the old behavior dropped 2nd+ decls with no
       diagnostic and produced a populated-but-empty spec). *)
    let loc = type_decl.ptype_loc in
    let ext = Ppxlib.Location.error_extensionf ~loc
        "Cannot derive view_spec for multiple mutually defined types; derive each type separately" in
    [pstr_extension ~loc ext []]

let impl_generator = Deriving.Generator.V2.make_noarg generate_view_spec_impl

let deriver =
  Deriving.add "view_spec" ~str_type_decl:impl_generator
