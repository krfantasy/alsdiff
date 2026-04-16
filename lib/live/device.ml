open Alsdiff_base
open Alsdiff_base.Diff


exception Not_implemented of string

type enum_desc = {
  min : int;
  max : int;
  enums : string array;
} [@@deriving eq]

type param_value =
  | Float of float
  | Int of int
  | Bool of bool
  | Enum of int * enum_desc
[@@deriving eq]


(* ================== Helper functions ================== *)

(** Extract a sub-path by dropping elements from both beginning and end.

    @param drop_begin Number of elements to drop from the beginning (default: 0)
    @param drop_end Number of elements to drop from the end (default: 0)
    @param path The input path string
    @return The sub-path with specified elements removed
*)
let sub_path ?(drop_begin = 0) ?(drop_end = 0) (path : string) : string =
  let path_parts = String.split_on_char '/' path in
  let non_empty_parts = List.filter (fun s -> s <> "") path_parts in
  let length = List.length non_empty_parts in

  (* Validate drop parameters *)
  if drop_begin < 0 then
    invalid_arg (Printf.sprintf "sub_path: drop_begin must be non-negative: %d" drop_begin)
  else if drop_end < 0 then
    invalid_arg (Printf.sprintf "sub_path: drop_end must be non-negative: %d" drop_end)
  else if drop_begin + drop_end > length then
    invalid_arg (Printf.sprintf "sub_path: cannot drop more elements than available: drop_begin:%d + drop_end:%d > length:%d"
                   drop_begin
                   drop_end
                   length);

  (* Calculate the range to keep *)
  let keep_start = drop_begin in
  let keep_end = length - drop_end in

  let sub_parts =
    non_empty_parts
    |> List.drop keep_start
    |> List.take (keep_end - keep_start)
  in

  (* Reconstruct the path *)
  match sub_parts with
  | [] -> ""
  | _ -> "/" ^ String.concat "/" sub_parts

(** [param_name_from_path path] extracts a parameter name from a path string.
    The path is relative to the device XML element. This function removes the
    first path component (device name) and joins the rest with "/".

    Examples:
    - "DeviceName/Param" -> "Param"
    - "DeviceName/Nested/Param" -> "Nested/Param"
    - "Param" -> "Param"
    - "" -> ""
*)
(* NOTE: This function was keep because all XML files like
   tests/compressor.xml that made for unit testing each individual module.
   In a full .als XML file, extract parameter name from a path doesn't need such complex logic.
   But in the other hand, unit testing against a full .als XML file is *SLOW*.
*)
let param_name_from_path (path : string) : string =
  let path_parts = String.split_on_char '/' path in
  let non_empty_parts = List.filter (fun s -> s <> "") path_parts in
  match non_empty_parts with
  | [] -> ""
  | [single] -> single (* Only one part, return it as-is *)
  | _ :: rest -> String.concat "/" rest (* Skip first part, join rest *)


(* ================== Common modules ================== *)
module MIDIMapping = struct
  type mapping_kind = Continuous | OnOff [@@deriving eq]

  type t = {
    target : int;               (* NoteOrController *) [@id.id] [@patch.skip]
    channel : int;              (* 0-15 for MIDI, 16 for Macro *)
    kind : mapping_kind;        [@patch.skip]
    low : int;
    high : int;
    (* TODO: MIDI Note mapping *)
  } [@@deriving eq, id, patch] [@@patch.generate_diff]

  let is_midi m = m.channel >= 0 && m.channel <= 15 (* MIDI Channel starts from 0 to 15 in Ableton Live .als XML *)
  let is_macro m = m.channel = 16

  (** Extract range from MidiControllerRange (continuous parameters) *)
  let extract_continuous_range (xml : Xml.t) : (int * int) option =
    match
      (Upath.get_int_attr_opt "/MidiControllerRange/Min" "Value" xml,
       Upath.get_int_attr_opt "/MidiControllerRange/Max" "Value" xml)
    with
    | (Some min_val, Some max_val) ->
      Some (min_val, max_val)
    | _ -> None

  (** Extract range from MidiCCOnOffThresholds (On/Off parameters) *)
  let extract_onoff_range (xml : Xml.t) : (int * int) option =
    match
      (Upath.get_int_attr_opt "/MidiCCOnOffThresholds/Min" "Value" xml,
       Upath.get_int_attr_opt "/MidiCCOnOffThresholds/Max" "Value" xml)
    with
    | (Some min_val, Some max_val) -> Some (min_val, max_val)
    | _ -> None

  (** [has_macro_mapping xml] checks if an XML elements has a macro mapping *)
  let has_macro_mapping (xml : Xml.t) : bool =
    let keymidi_xml = Upath.find "/KeyMidi" xml |> snd in
    let is_note = Upath.get_bool_attr "IsNote" "Value" keymidi_xml in
    let controller_map_mode = Upath.get_int_attr "ControllerMapMode" "Value" keymidi_xml in
    is_note && controller_map_mode <> 0

  let create (xml : Xml.t) : t =
    if not (has_macro_mapping xml) then
      raise (Xml.Xml_error (xml, "It's not a MIDIMapping"))
    else
      let target = Upath.get_int_attr "/KeyMidi/NoteOrController" "Value" xml in
      let channel = Upath.get_int_attr "/KeyMidi/Channel" "Value" xml in
      let continuous = extract_continuous_range xml in
      let onoff = extract_onoff_range xml in
      let (low, high, kind) = match (continuous, onoff) with
        | Some (l, h), None -> (l, h, Continuous)
        | None, Some (l, h) -> (l, h, OnOff)
        | _ -> raise (Xml.Xml_error (xml, "Invalid XML for creating a MIDIMapping"))
      in
      { target; channel; kind; low; high }

  let create_opt (xml : Xml.t) : t option = try Some (create xml) with _ -> None

  (** [make_opt results ~qid_base] constructs a MIDIMapping from Upath2 results.
      Expects 8 consecutive qids starting at qid_base:
      0=NoteOrController, 1=Channel, 2=IsNote, 3=ControllerMapMode,
      4=MidiControllerRange/Min, 5=Max, 6=MidiCCOnOffThresholds/Min, 7=Max *)
  let make_opt results ~qid_base : t option =
    let is_note = Upath2.query_bool_attr results (qid_base + 2) "Value"
      |> Option.value ~default:false in
    let map_mode = Upath2.query_int_attr results (qid_base + 3) "Value"
      |> Option.value ~default:0 in
    if is_note && map_mode <> 0 then begin
      let target = Upath2.query_int_attr results (qid_base + 0) "Value"
        |> Option.value ~default:0 in
      let channel = Upath2.query_int_attr results (qid_base + 1) "Value"
        |> Option.value ~default:0 in
      let cont_min = Upath2.query_int_attr results (qid_base + 4) "Value" in
      let cont_max = Upath2.query_int_attr results (qid_base + 5) "Value" in
      let onoff_min = Upath2.query_int_attr results (qid_base + 6) "Value" in
      let onoff_max = Upath2.query_int_attr results (qid_base + 7) "Value" in
      match (cont_min, cont_max), (onoff_min, onoff_max) with
      | (Some l, Some h), (None, None) ->
        Some { target; channel; kind = Continuous; low = l; high = h }
      | (None, None), (Some l, Some h) ->
        Some { target; channel; kind = OnOff; low = l; high = h }
      | _ -> None
    end else None

  (** [create_head_key_midi xml] create a [MIDIMapping] from a [<HeadKeyMidi></HeadKeyMidi>] element *)
  let create_head_key_midi (xml : Xml.t) : t =
    let target = Upath.get_int_attr "/NoteOrController" "Value" xml in
    let channel = Upath.get_int_attr "/Channel" "Value" xml in
    (* NOTE: This function was only used for creating a MIDIMapping.t for Solo in the Mixer. Solo only supports on/off mapping *)
    { target; channel; kind = OnOff; low = 64; high = 127; }
end

module GenericParam = struct
  type t = {
    name : string;              [@patch.identity]
    value : param_value;
    automation : int;
    modulation : int;           (* parameter cannot modulated will be set to a negative number *)
    mapping : MIDIMapping.t option;  [@patch.skip]
  } [@@deriving eq, patch] [@@patch.generate_diff]

  let queries = [
    Upath2.query_of_path "/AutomationTarget@Id";
    Upath2.query_of_path "/ModulationTarget@Id";
    Upath2.query_of_path "/Manual@Value";
    (* MIDIMapping *)
    Upath2.query_of_path "/KeyMidi/NoteOrController@Value";
    Upath2.query_of_path "/KeyMidi/Channel@Value";
    Upath2.query_of_path "/KeyMidi/IsNote@Value";
    Upath2.query_of_path "/KeyMidi/ControllerMapMode@Value";
    Upath2.query_of_path "/MidiControllerRange/Min@Value";
    Upath2.query_of_path "/MidiControllerRange/Max@Value";
    Upath2.query_of_path "/MidiCCOnOffThresholds/Min@Value";
    Upath2.query_of_path "/MidiCCOnOffThresholds/Max@Value";
  ]

  let make ~root_name ~parse_value results =
    let name = root_name in
    let value = parse_value results in
    let automation = Upath2.query_int_attr results 0 "Id"
      |> Option.value ~default:0 in
    let modulation = Upath2.query_int_attr results 1 "Id"
      |> Option.value ~default:0 in
    let mapping = MIDIMapping.make_opt results ~qid_base:3 in
    { name; value; automation; modulation; mapping }

  let create ~parse_value xml =
    let root_name = Xml.get_name xml in
    let stream = Upath2.stream_of_xml xml in
    let nfa = Upath2.compile queries in
    let results = Upath2.evaluate nfa stream in
    make ~root_name ~parse_value results

  let create_int_manual xml =
    create xml ~parse_value:(fun results ->
        Int (Option.get (Upath2.query_int_attr results 2 "Value")))

  let create_float_manual xml =
    create xml ~parse_value:(fun results ->
        Float (Option.get (Upath2.query_float_attr results 2 "Value")))

  let create_bool_manual xml =
    create xml ~parse_value:(fun results ->
        Bool (Option.get (Upath2.query_bool_attr results 2 "Value")))
end


module NameIdGenericParam = struct
  include GenericParam

  let has_same_id a b = a.name = b.name
  let id_hash t = Hashtbl.hash t.name
end


module DeviceParam = struct
  type t = {
    base : GenericParam.t;
  } [@@deriving eq, patch] [@@patch.generate_diff]

  let has_same_id a b = a.base.name = b.base.name
  let id_hash t = Hashtbl.hash t.base.name

  let queries = GenericParam.queries

  let make ~path results =
    let name = param_name_from_path path in
    let parse_value results =
      let literal = Option.get (Upath2.query_attr results 2 "Value") in
      match literal with
      | "true" | "false" -> Bool (bool_of_string literal)
      | _ -> Float (float_of_string literal)
    in
    let base = GenericParam.make ~root_name:name ~parse_value results in
    { base }

  let create (path : string) (xml : Xml.t) : t =
    match xml with
    | Xml.Element _ ->
      let stream = Upath2.stream_of_xml xml in
      let nfa = Upath2.compile queries in
      let results = Upath2.evaluate nfa stream in
      make ~path results
    | _ -> raise (Xml.Xml_error (xml, "Invalid XML element for creating DeviceParam"))

  let create_from_upath_find (path, xml) = create path xml
end


module PresetRef = struct
  type preset_type =
    | UserPreset
    | DefaultPreset
  [@@deriving eq]

  type t = {
    id : int;                   (* not unique *) [@id.id] [@patch.skip]
    name : string;              [@patch.skip]
    preset_type : preset_type;  [@patch.skip]
    relative_path : string;
    path : string;
    pack_name : string;
    pack_id : int;
    file_size : int;
    crc : int;
  } [@@deriving eq, id, patch] [@@patch.generate_diff]

  let queries = [
    Upath2.query_of_path "/FileRef/RelativePath@Value";
    Upath2.query_of_path "/FileRef/Path@Value";
    Upath2.query_of_path "/FileRef/LivePackName@Value";
    Upath2.query_of_path "/FileRef/LivePackId@Value";
    Upath2.query_of_path "/FileRef/OriginalFileSize@Value";
    Upath2.query_of_path "/FileRef/OriginalCrc@Value";
    Upath2.query_of_path "/DeviceId@Name";
  ]

  let make ~root_name ~root_attrs results =
    let id = int_of_string (List.assoc "Id" root_attrs) in
    let preset_type = match root_name with
      | "FilePresetRef" -> UserPreset
      | "AbletonDefaultPresetRef" -> DefaultPreset
      | _ -> failwith "Unknown PresetRef type"
    in
    let relative_path = Option.get (Upath2.query_attr results 0 "Value") in
    let path = Option.get (Upath2.query_attr results 1 "Value") in
    let preset_file_name = Filename.basename path |> Filename.remove_extension in
    let name = match preset_type with
      | UserPreset -> preset_file_name
      | DefaultPreset ->
        let device_name = Option.value (Upath2.query_attr results 6 "Name") ~default:"" in
        if device_name <> "" then device_name else preset_file_name
    in
    let pack_name = Option.get (Upath2.query_attr results 2 "Value") in
    let pack_id = Upath2.query_int_attr results 3 "Value" |> Option.value ~default:0 in
    let file_size = Option.get (Upath2.query_int_attr results 4 "Value") in
    let crc = Upath2.query_int_attr results 5 "Value" |> Option.value ~default:0 in
    { id; name; preset_type; relative_path; path; pack_name; pack_id; file_size; crc }

  let create (xml : Xml.t) : t =
    match xml with
    | Xml.Element { name = root_name; attrs = root_attrs; _ } ->
      let stream = Upath2.stream_of_xml xml in
      let nfa = Upath2.compile queries in
      let results = Upath2.evaluate nfa stream in
      make ~root_name ~root_attrs results
    | _ -> raise (Xml.Xml_error (xml, "Invalid XML element for creating PresetRef"))
end


(* ================== M4L PatchRef module ================== *)
module PatchRef = struct
  type t = {
    id : int;                              [@id.id] [@patch.skip]
    name : string;                         [@patch.skip]
    preset_type : PresetRef.preset_type;   [@patch.skip]
    relative_path : string;
    path : string;
    pack_name : string;
    pack_id : int;
    file_size : int;
    crc : int;
    last_mod_date : int;  (* LastModDate Value attribute - UNIX timestamp *)
  } [@@deriving eq, id, patch] [@@patch.generate_diff]

  let queries = [
    Upath2.query_of_path "/FileRef/RelativePath@Value";
    Upath2.query_of_path "/FileRef/Path@Value";
    Upath2.query_of_path "/FileRef/LivePackName@Value";
    Upath2.query_of_path "/FileRef/LivePackId@Value";
    Upath2.query_of_path "/FileRef/OriginalFileSize@Value";
    Upath2.query_of_path "/FileRef/OriginalCrc@Value";
    Upath2.query_of_path "/LastModDate@Value";
  ]

  let make ~root_attrs results =
    let id = int_of_string (List.assoc "Id" root_attrs) in
    let preset_type = PresetRef.UserPreset in
    let relative_path = Option.get (Upath2.query_attr results 0 "Value") in
    let path = Option.get (Upath2.query_attr results 1 "Value") in
    let name = Filename.basename path |> Filename.remove_extension in
    let last_mod_date = Upath2.query_int_attr results 6 "Value" |> Option.value ~default:0 in
    let pack_name = Option.get (Upath2.query_attr results 2 "Value") in
    let pack_id = Upath2.query_int_attr results 3 "Value" |> Option.value ~default:0 in
    let file_size = Option.get (Upath2.query_int_attr results 4 "Value") in
    let crc = Upath2.query_int_attr results 5 "Value" |> Option.value ~default:0 in
    { id; name; preset_type; relative_path; path; pack_name; pack_id; file_size; crc; last_mod_date }

  let create (xml : Xml.t) : t =
    match xml with
    | Xml.Element { name = "MxPatchRef"; attrs = root_attrs; _ } ->
      let stream = Upath2.stream_of_xml xml in
      let nfa = Upath2.compile queries in
      let results = Upath2.evaluate nfa stream in
      make ~root_attrs results
    | _ -> raise (Xml.Xml_error (xml, "Invalid XML element for creating PatchRef (expected MxPatchRef)"))
end


(* ================== Plugin related modules ================== *)
module PluginParam = struct
  type t = {
    id : int;
    base : GenericParam.t;
  } [@@deriving eq]

  let queries = [
    Upath2.query_of_path "/ParameterName@Value";
    Upath2.query_of_path "/ParameterValue/Manual@Value";
    Upath2.query_of_path "/ParameterValue/AutomationTarget@Id";
    Upath2.query_of_path "/ParameterValue/ModulationTarget@Id";
    Upath2.query_of_path "/ParameterValue/KeyMidi/NoteOrController@Value";
    Upath2.query_of_path "/ParameterValue/KeyMidi/Channel@Value";
    Upath2.query_of_path "/ParameterValue/KeyMidi/IsNote@Value";
    Upath2.query_of_path "/ParameterValue/KeyMidi/ControllerMapMode@Value";
    Upath2.query_of_path "/ParameterValue/MidiControllerRange/Min@Value";
    Upath2.query_of_path "/ParameterValue/MidiControllerRange/Max@Value";
    Upath2.query_of_path "/ParameterValue/MidiCCOnOffThresholds/Min@Value";
    Upath2.query_of_path "/ParameterValue/MidiCCOnOffThresholds/Max@Value";
  ]

  let make ~root_name ~root_attrs results =
    let id = int_of_string (List.assoc "Id" root_attrs) in
    let name = Option.get (Upath2.query_attr results 0 "Value") in
    let value = match root_name with
      | "PluginFloatParameter" ->
        Float (Option.get (Upath2.query_float_attr results 1 "Value"))
      | "PluginIntParameter" ->
        Int (Option.get (Upath2.query_int_attr results 1 "Value"))
      | "PluginBoolParameter" ->
        Bool (Option.get (Upath2.query_bool_attr results 1 "Value"))
      | "PluginEnumParameter" ->
        Int (Option.get (Upath2.query_int_attr results 1 "Value"))
      | _ -> failwith ("Invalid parameter type " ^ root_name)
    in
    let automation = Upath2.query_int_attr results 2 "Id"
      |> Option.value ~default:0 in
    let modulation = Upath2.query_int_attr results 3 "Id"
      |> Option.value ~default:0 in
    let mapping = MIDIMapping.make_opt results ~qid_base:6 in
    { id; base = { GenericParam.name; value; automation; modulation; mapping } }

  let create (xml : Xml.t) : t =
    let root_name = Xml.get_name xml in
    let root_attrs = (match xml with Xml.Element { attrs; _ } -> attrs | _ -> []) in
    let stream = Upath2.stream_of_xml xml in
    let nfa = Upath2.compile queries in
    let results = Upath2.evaluate nfa stream in
    try make ~root_name ~root_attrs results
    with Failure msg -> raise (Xml.Xml_error (xml, msg))

  module Patch = struct
    type t = {
      id : int;
      base : GenericParam.Patch.t structured_update;
    }

    let is_empty p = is_unchanged_update (module GenericParam.Patch) p.base
  end

  (* NOTE: From my observation, ParameterId seems always equal to -1, which is useless for an identifier.
     We use the Id attribute as the identifier for PluginParam since it's unique and stable *)
  let has_same_id a b = a.id = b.id

  let id_hash t = Hashtbl.hash t.id

  let diff (old_param : t) (new_param : t) : Patch.t =
    if not (has_same_id old_param new_param) then
      failwith "cannot diff two PluginParams with different identifiers"
    else
      let module PluginSpecializedGenericParam = struct
        include GenericParam

        let has_same_id _ _ = true
        let id_hash _ = 0
      end in

      let base_change = diff_complex_value_id (module PluginSpecializedGenericParam) old_param.base new_param.base in
      { id = new_param.id; base = base_change }
end


module PluginDesc = struct
  type plugin_type = Vst2 | Vst3 | Auv2 [@@deriving eq]

  module PluginTypeEq = Equality.MakeDefaultEq(struct type t = plugin_type end)

  type t = {
    name : string;
    uid : string;
    plugin_type : plugin_type;
    processor_state : string;
    (* NOTE: Since the controller state in VST3 is the state of the GUI, which has
       nothing to do with actual audio/dsp processing, we just ignore it for god's sake *)
    (* controller_state : string; *)
  } [@@deriving eq]

  let plugin_type_to_string = function
    | Vst2 -> "VST2"
    | Vst3 -> "VST3"
    | Auv2 -> "AUv2"

  let parse_vst3_uid xml =
    let fields = Upath.find_all "/'Fields\\.[0-9]+$'" xml in
    let sorted_fields = List.sort (fun (a, _) (b, _) -> String.compare a b) fields in
    sorted_fields
    |> List.map (fun (_, field_xml) -> Xml.get_attr "Value" field_xml)
    |> String.concat "-"

  let re_whitespaces = Re.compile (Re.Pcre.re "\\s+")
  let reformat_blob_str s =
    Re.replace_string ~all:true re_whitespaces ~by:"" s

  let parse_vst3_processor_state plugin_info_xml =
    Upath.find_opt "/Preset/Vst3Preset/ProcessorState" plugin_info_xml
    |> Option.map (fun x -> snd x |> Xml.get_data_child |> reformat_blob_str)
    |> Option.value ~default:""

  let parse_vst3_info plugin_info_xml =
    (* Get plugin name - try different possible locations *)
    let name =
      match Upath.get_attr_opt "/Name" "Value" plugin_info_xml with
      | Some value -> value
      | None ->
        match Upath.get_attr_opt "/PlugName" "Value" plugin_info_xml with
        | Some plug_name -> plug_name
        | None -> "Unknown VST3 Plugin"
    in

    (* Get UID from VST3 specific structure *)
    let uid = Upath.find "/Uid" plugin_info_xml |> snd |> parse_vst3_uid in

    (* Get processor state from VST3 preset *)
    let processor_state = parse_vst3_processor_state plugin_info_xml in
    (name, uid, processor_state)

  let parse_vst2_processor_state plugin_info_xml =
    (* VST2 plugins typically don't have processor state in the same way as VST3 *)
    Upath.find_opt "/Preset/VstPreset/State" plugin_info_xml
    |> Option.map (fun x -> snd x |> Xml.get_data_child |> reformat_blob_str)
    |> Option.value ~default:""

  let parse_vst2_info plugin_info_xml =
    (* Get plugin name - try different possible locations for VST2 *)
    let name =
      match Upath.get_attr_opt "/PlugName" "Value" plugin_info_xml with
      | Some plug_name -> plug_name
      | None ->
        match Upath.get_attr_opt "/Name" "Value" plugin_info_xml with
        | Some value -> value
        | None -> "Unknown VST2 Plugin"
    in

    (* Get UID from VST2 specific structure *)
    let uid =
      match Upath.get_attr_opt "/UniqueId" "Value" plugin_info_xml with
      | Some unique_id -> unique_id
      | None ->
        (* Try to extract from path as fallback *)
        match Upath.get_attr_opt "/Path" "Value" plugin_info_xml with
        | Some path ->
          (* Extract plugin name from path and create a simple hash *)
          let filename = Filename.basename path in
          string_of_int (Hashtbl.hash filename)
        | None -> "0"
    in

    (* Get processor state from VST2 preset *)
    let processor_state = parse_vst2_processor_state plugin_info_xml in

    (name, uid, processor_state)

  let parse_au_processor_state plugin_info_xml =
    (* AU plugins use Buffer element for state data *)
    Upath.find_opt "/Preset/AuPreset/Buffer" plugin_info_xml
    |> Option.map (fun x -> snd x |> Xml.get_data_child |> reformat_blob_str)
    |> Option.value ~default:""

  let parse_au_info plugin_info_xml =
    (* Get plugin name for AU plugins *)
    let name =
      match Upath.get_attr_opt "/Name" "Value" plugin_info_xml with
      | Some value -> value
      | None -> "Unknown AU Plugin"
    in

    (* Create UID from AU component identifiers *)
    let uid =
      match Upath.get_attr_opt "/ComponentType" "Value" plugin_info_xml,
            Upath.get_attr_opt "/ComponentSubType" "Value" plugin_info_xml,
            Upath.get_attr_opt "/ComponentManufacturer" "Value" plugin_info_xml with
      | Some comp_type, Some comp_subtype, Some comp_manufacturer ->
        Printf.sprintf "%s-%s-%s" comp_type comp_subtype comp_manufacturer
      | _ ->
        (* Fallback: try to use manufacturer and name *)
        match Upath.get_attr_opt "/Manufacturer" "Value" plugin_info_xml with
        | Some manufacturer ->
          let combined = manufacturer ^ "-" ^ name in
          string_of_int (Hashtbl.hash combined)
        | None ->
          (* Last resort: hash the plugin name *)
          string_of_int (Hashtbl.hash name)
    in

    (* Get processor state from AU preset buffer *)
    let processor_state = parse_au_processor_state plugin_info_xml in

    (name, uid, processor_state)

  let create (xml : Xml.t) : t =
    (* Extract plugin type based on the element name *)
    let plugin_info_xml = Xml.get_childs xml |> List.hd in

    let plugin_type =
      match Xml.get_name plugin_info_xml with
      | "Vst3PluginInfo" -> Vst3
      | "VstPluginInfo" -> Vst2
      | "AuPluginInfo" -> Auv2
      | name -> raise (Xml.Xml_error (plugin_info_xml, "Unsupported plugin type: " ^ name))
    in

    let (name, uid, processor_state) =
      match plugin_type with
      | Vst3 -> parse_vst3_info plugin_info_xml
      | Vst2 -> parse_vst2_info plugin_info_xml
      | Auv2 -> parse_au_info plugin_info_xml
    in

    { name; uid; plugin_type; processor_state }

  module Patch = struct
    type t = {
      name : string atomic_update;
      uid : string atomic_update;
      plugin_type : plugin_type atomic_update;
      state : string atomic_update;
    }

    let is_empty p =
      is_unchanged_atomic_update p.name &&
      is_unchanged_atomic_update p.uid &&
      is_unchanged_atomic_update p.plugin_type &&
      is_unchanged_atomic_update p.state
  end

  let diff (old_desc : t) (new_desc : t) : Patch.t =
    if old_desc.uid <> new_desc.uid then
      failwith "cannot diff two PluginDesc with different UIDs"
    else
      let name_change = diff_atomic_value (module String) old_desc.name new_desc.name in
      let uid_change = diff_atomic_value (module String) old_desc.uid new_desc.uid in
      let plugin_type_change = diff_atomic_value (module PluginTypeEq) old_desc.plugin_type new_desc.plugin_type in
      let state_change = diff_atomic_value (module String) old_desc.processor_state new_desc.processor_state in
      {
        Patch.name = name_change;
        uid = uid_change;
        plugin_type = plugin_type_change;
        state = state_change;
      }

  let has_same_id a b = a.uid = b.uid

  let id_hash t = Hashtbl.hash t.uid

end


(* ================== Max4Live device related modules ================== *)
module Max4LiveParam = struct
  type t = {
    id : int;                   (* ParameterId *) [@id.id] [@patch.skip]
    index : int;                (* VisualIndex *)
    base : GenericParam.t;
  } [@@deriving eq, id, patch] [@@patch.generate_diff]

  let queries = [
    Upath2.query_of_path "/Name@Value";
    Upath2.query_of_path "/Index@Value";
    Upath2.query_of_path "/Timeable/Manual@Value";
    Upath2.query_of_path "/Timeable/AutomationTarget@Id";
    Upath2.query_of_path "/Timeable/ModulationTarget@Id";
    Upath2.query_of_path "/Timeable/KeyMidi/NoteOrController@Value";
    Upath2.query_of_path "/Timeable/KeyMidi/Channel@Value";
    Upath2.query_of_path "/Timeable/KeyMidi/IsNote@Value";
    Upath2.query_of_path "/Timeable/KeyMidi/ControllerMapMode@Value";
    Upath2.query_of_path "/Timeable/MidiControllerRange/Min@Value";
    Upath2.query_of_path "/Timeable/MidiControllerRange/Max@Value";
    Upath2.query_of_path "/Timeable/MidiCCOnOffThresholds/Min@Value";
    Upath2.query_of_path "/Timeable/MidiCCOnOffThresholds/Max@Value";
    Upath2.query_of_path "/Names/Name/Name@Value";
  ]

  let make ~root_name ~root_attrs results =
    let id = int_of_string (List.assoc "Id" root_attrs) in
    let index = Option.get (Upath2.query_int_attr results 1 "Value") in
    let name = Option.get (Upath2.query_attr results 0 "Value") in
    let value = match root_name with
      | "MxDFloatParameter" ->
        Float (Option.get (Upath2.query_float_attr results 2 "Value"))
      | "MxDIntParameter" ->
        Int (Option.get (Upath2.query_int_attr results 2 "Value"))
      | "MxDBoolParameter" ->
        Bool (Option.get (Upath2.query_bool_attr results 2 "Value"))
      | "MxDEnumParameter" ->
        let enum_value = Option.get (Upath2.query_int_attr results 2 "Value") in
        let enums =
          Upath2.find_all_results results 13
          |> List.map (fun (r : Upath2.match_result) ->
              Option.get (Upath2.get_attr r "Value"))
          |> Array.of_list
        in
        if Array.length enums > 0 then
          Enum (enum_value, { min = 0; max = Array.length enums - 1; enums })
        else
          failwith "MxDEnumParameter: no enum definitions found"
      | _ -> failwith ("Invalid M4L parameter type: " ^ root_name)
    in
    let automation = Upath2.query_int_attr results 3 "Id"
      |> Option.value ~default:0 in
    let modulation = Upath2.query_int_attr results 4 "Id"
      |> Option.value ~default:0 in
    let mapping = MIDIMapping.make_opt results ~qid_base:7 in
    { id; index; base = { GenericParam.name; value; automation; modulation; mapping } }

  let create (_path : string) (xml : Xml.t) : t =
    let root_name = Xml.get_name xml in
    let root_attrs = (match xml with Xml.Element { attrs; _ } -> attrs | _ -> []) in
    let stream = Upath2.stream_of_xml xml in
    let nfa = Upath2.compile queries in
    let results = Upath2.evaluate nfa stream in
    make ~root_name ~root_attrs results

  let create_from_upath_find (path, xml) = create path xml
end


(* ================== Group device related modules ================== *)
(* The rack chain's mixer is different to track's mixer *)
module MixerDevice = struct
  type t = {
    on : DeviceParam.t;
    speaker : DeviceParam.t;     (* mute or not *)
    volume : DeviceParam.t;      (* volume *)
    pan : DeviceParam.t;         (* panorama/panning *)
  } [@@deriving eq, patch] [@@patch.generate_diff]

  let dp_queries prefix =
    let p = "/" ^ prefix in
    [
      Upath2.query_of_path (p ^ "/AutomationTarget@Id");
      Upath2.query_of_path (p ^ "/ModulationTarget@Id");
      Upath2.query_of_path (p ^ "/Manual@Value");
      Upath2.query_of_path (p ^ "/KeyMidi/NoteOrController@Value");
      Upath2.query_of_path (p ^ "/KeyMidi/Channel@Value");
      Upath2.query_of_path (p ^ "/KeyMidi/IsNote@Value");
      Upath2.query_of_path (p ^ "/KeyMidi/ControllerMapMode@Value");
      Upath2.query_of_path (p ^ "/MidiControllerRange/Min@Value");
      Upath2.query_of_path (p ^ "/MidiControllerRange/Max@Value");
      Upath2.query_of_path (p ^ "/MidiCCOnOffThresholds/Min@Value");
      Upath2.query_of_path (p ^ "/MidiCCOnOffThresholds/Max@Value");
    ]

  let queries =
    dp_queries "On" @ dp_queries "Speaker"
    @ dp_queries "Volume" @ dp_queries "Panorama"

  let make_dp results ~path ~qid_base =
    let name = param_name_from_path path in
    let literal = Option.get (Upath2.query_attr results (qid_base + 2) "Value") in
    let value = match literal with
      | "true" | "false" -> Bool (bool_of_string literal)
      | _ -> Float (float_of_string literal)
    in
    let automation = Upath2.query_int_attr results (qid_base + 0) "Id"
      |> Option.value ~default:0 in
    let modulation = Upath2.query_int_attr results (qid_base + 1) "Id"
      |> Option.value ~default:0 in
    let mapping = MIDIMapping.make_opt results ~qid_base:(qid_base + 3) in
    { DeviceParam.base = { GenericParam.name; value; automation; modulation; mapping } }

  let make results =
    { on = make_dp results ~path:"On" ~qid_base:0;
      speaker = make_dp results ~path:"Speaker" ~qid_base:11;
      volume = make_dp results ~path:"Volume" ~qid_base:22;
      pan = make_dp results ~path:"Panorama" ~qid_base:33 }

  let create (xml : Xml.t) : t =
    match xml with
    | Xml.Element { name = "MixerDevice"; _ } ->
      let stream = Upath2.stream_of_xml xml in
      let nfa = Upath2.compile queries in
      let results = Upath2.evaluate nfa stream in
      make results
    | _ -> raise (Xml.Xml_error (xml, "Invalid XML element for creating MixerDevice"))

  (* MixerDevice doesn't have a natural ID, so use placeholder interface *)
  let has_same_id _ _ = true
  let id_hash _ = Hashtbl.hash 0
end


(** [extract_index_from_name element_name] extracts the numeric index from
    element names like "MacroValues.3", "MacroControls.15", etc.
    Raises [Failure] if the element name doesn't contain a valid index. *)
let extract_index_from_name (element_name : string) : int =
  let parts = String.split_on_char '.' element_name in
  match List.rev parts with
  | index :: _ -> int_of_string index
  | [] -> failwith ("Invalid element name: " ^ element_name)

(* Helper function for diffing lists of atomic values *)
let diff_atomic_list (old_list : float list) (new_list : float list) : float atomic_change list =
  if List.length old_list <> List.length new_list then
    failwith "diff_atomic_list requires lists of same length"
  else
    List.map2 (fun old_elem new_elem ->
        (diff_atomic_value (module Float) old_elem new_elem :> float atomic_change)
      ) old_list new_list


module Macro = struct
  type t = {
    id : int;               [@id.id] [@patch.skip]
    base : GenericParam.t;
  } [@@deriving eq, id, patch] [@@patch.generate_diff]

  let create (name_xml : Xml.t) (control_xml : Xml.t) : t =
    (* Extract the macro name from MacroDisplayNames element *)
    let name_id = extract_index_from_name @@ Xml.get_name name_xml in
    let control_id = extract_index_from_name @@ Xml.get_name control_xml in
    let base = GenericParam.create_float_manual control_xml in
    if name_id <> control_id then
      raise (Xml.Xml_error (name_xml, "Macro name ID " ^ string_of_int name_id ^ " does not match control ID " ^ string_of_int control_id ^ ". Macro names and controls must be paired correctly."))
    else
      { id=name_id; base; }
end


module Snapshot = struct
  type t = {
    id : int;               [@id.id] [@patch.skip]
    name : string;
    values : float list;
  } [@@deriving eq, id, patch]

  let queries = [
    Upath2.query_of_path "/SnapshotName@Value";
    Upath2.query_of_path "/'MacroValues\\.[0-9]+'@Value";
  ]

  let make ~root_attrs results =
    let id = int_of_string (List.assoc "Id" root_attrs) in
    let name = Option.get (Upath2.query_attr results 0 "Value") in
    let values =
      Upath2.find_all_results results 1
      |> List.map (fun (r : Upath2.match_result) ->
          let index = extract_index_from_name r.Upath2.element_name in
          let value = Option.get (Upath2.get_float_attr r "Value") in
          (index, value))
      |> List.sort (fun (i1, _) (i2, _) -> Stdlib.compare i1 i2)
      |> List.map snd
    in
    { id; name; values }

  let create (xml : Xml.t) : t =
    match xml with
    | Xml.Element { name = "MacroSnapshot"; attrs = root_attrs; _ } ->
      let stream = Upath2.stream_of_xml xml in
      let nfa = Upath2.compile queries in
      let results = Upath2.evaluate nfa stream in
      make ~root_attrs results
    | _ -> raise (Xml.Xml_error (xml, "Invalid XML element for creating Snapshot"))

  let diff (old_snapshot : t) (new_snapshot : t) : Patch.t =
    if old_snapshot.id <> new_snapshot.id then
      failwith "cannot diff two Snapshots with different Ids"
    else
      let name_change = diff_atomic_value (module String) old_snapshot.name new_snapshot.name in
      let values_changes = diff_atomic_list old_snapshot.values new_snapshot.values in

      {
        name = name_change;
        values = values_changes;
      }
end


(* ================== Type definitions ================== *)
(* JEEZ, CIRCULR DEPENDENCIES *)
type device =
  | Regular of regular_device
  | Plugin of plugin_device
  | Max4Live of max4live_device
  | Group of group_device [@@deriving eq]

and regular_device = {
  id : int;
  device_name : string;
  display_name : string;        (* either UserName or PresetName *)
  pointee : int;
  enabled : DeviceParam.t;
  params : DeviceParam.t list;
  preset : PresetRef.t option;
} [@@deriving eq]

and plugin_device = {
  id : int;
  device_name : string;
  display_name : string;
  pointee : int;
  enabled : DeviceParam.t;
  desc : PluginDesc.t;
  params : PluginParam.t list;
  preset : PresetRef.t option;
  (* TODO: Support sidechain and MPE settigns *)
} [@@deriving eq]

and max4live_device = {
  id : int;
  device_name : string;
  display_name : string;
  pointee : int;
  enabled : DeviceParam.t;
  patch_ref : PatchRef.t;       (* the .amxd file *)
  params : Max4LiveParam.t list;
  preset : PresetRef.t option;
} [@@deriving eq]

and branch = {
  id : int;
  devices : device list;
  mixer : MixerDevice.t;
} [@@deriving eq]
and group_device = {
  id : int;
  device_name : string;
  display_name : string;
  pointee : int;
  enabled : DeviceParam.t;
  branches : branch list;
  macros : Macro.t list;
  snapshots : Snapshot.t list;
  preset : PresetRef.t option;
} [@@deriving eq]

type device_patch =
  | RegularPatch of regular_device_patch
  | PluginPatch of plugin_device_patch
  | Max4LivePatch of max4live_device_patch
  | GroupPatch of group_device_patch

and regular_device_patch = {
  id : int;                    (* immutable identity *)
  device_name : string;         (* immutable identity *)
  display_name : string atomic_update;

  (* parameters can be added or removed due to Ableton updates on their built-in devices,
     so change is more semantically correct than update *)
  params : (DeviceParam.t, DeviceParam.Patch.t) structured_change list;

  preset : (PresetRef.t, PresetRef.Patch.t) structured_change;
}

and plugin_device_patch = {
  id : int;                    (* immutable identity *)
  device_name : string;         (* immutable identity *)
  display_name : string atomic_update;
  enabled : DeviceParam.Patch.t structured_update;
  desc : PluginDesc.Patch.t structured_update;
  params : (PluginParam.t, PluginParam.Patch.t) structured_change list;
  preset : (PresetRef.t, PresetRef.Patch.t) structured_change;
}

and max4live_device_patch = {
  id : int;                    (* immutable identity *)
  device_name : string;         (* immutable identity *)
  display_name : string atomic_update;
  enabled : DeviceParam.Patch.t structured_update;
  patch_ref : (PatchRef.t, PatchRef.Patch.t) structured_change;
  params : (Max4LiveParam.t, Max4LiveParam.Patch.t) structured_change list;
  preset : (PresetRef.t, PresetRef.Patch.t) structured_change;
}

and branch_patch = {
  id : int atomic_update;
  devices : (device, device_patch) structured_change list;
  mixer : MixerDevice.Patch.t structured_update;
}
and group_device_patch = {
  id : int;                    (* immutable identity *)
  device_name : string;         (* immutable identity *)
  display_name : string atomic_update;
  enabled : DeviceParam.Patch.t structured_update;

  (* devices always have preset, its either user-defined one or the defualt one,
     so only Unchanged/Modified cases *)
  branches : (branch, branch_patch) structured_change list;
  macros : (Macro.t, Macro.Patch.t) structured_change list;
  snapshots : (Snapshot.t, Snapshot.Patch.t) structured_change list;
  preset : (PresetRef.t, PresetRef.Patch.t) structured_change;
}


(* ================== Forward Reference Declarations ================== *)
(* These mutable references are used to break the circular dependency between
   device, branch patch is_empty functions and the diff functions. They are
   initialized at the end of the file after all types and modules are defined. *)
let device_patch_is_empty_ref : (device_patch -> bool) ref =
  ref (fun _ -> failwith "device_patch_is_empty not initialized")
let branch_patch_is_empty_ref : (branch_patch -> bool) ref =
  ref (fun _ -> failwith "branch_patch_is_empty not initialized")

(* Helper function to get device type name and id for error messages *)
let get_device_type_and_id = function
  | Regular r -> "Regular", r.id
  | Plugin p -> "Plugin", p.id
  | Group g -> "Group", g.id
  | Max4Live m -> "Max4Live", m.id

(* regular_device diff functions *)
let rec regular_device_diff (old_device : regular_device) (new_device : regular_device) : regular_device_patch =
  if old_device.id <> new_device.id && old_device.device_name <> new_device.device_name  then
    failwith (Printf.sprintf "Cannot diff two RegularDevices with different Ids & Device names: %d/%s vs %d/%s"
                old_device.id old_device.device_name new_device.id new_device.device_name)
  else
    let display_name_change = diff_atomic_value (module String) old_device.display_name new_device.display_name in
    let preset_change = diff_complex_value_opt (module PresetRef) old_device.preset new_device.preset in
    let params_changes =
      diff_list_id (module DeviceParam) old_device.params new_device.params
      |> filter_changes (module DeviceParam.Patch)
    in
    {
      id = new_device.id;
      device_name = new_device.device_name;
      display_name = display_name_change;
      preset = preset_change;
      params = params_changes;
    }
(* plugin_device diff functions *)
and plugin_device_diff (old_device : plugin_device) (new_device : plugin_device) : plugin_device_patch =
  if old_device.id <> new_device.id then
    failwith (Printf.sprintf "Cannot diff two PluginDevices with different IDs: %d vs %d (Name: %s)"
                old_device.id new_device.id old_device.device_name)
  else
    let display_name_change =
      diff_atomic_value (module String) old_device.display_name new_device.display_name
    in
    let enabled_change =
      diff_complex_value (module DeviceParam) old_device.enabled new_device.enabled
    in

    let desc_change =
      diff_complex_value (module PluginDesc) old_device.desc new_device.desc
    in

    let params_change =
      diff_list_id (module PluginParam) old_device.params new_device.params
      |> filter_changes (module PluginParam.Patch)
    in

    let preset_change =
      diff_complex_value_opt (module PresetRef) old_device.preset new_device.preset
    in

    {
      id = new_device.id;
      device_name = new_device.device_name;
      display_name = display_name_change;
      enabled = enabled_change;
      desc = desc_change;
      params = params_change;
      preset = preset_change
    }

(* max4live_device diff functions *)
and max4live_device_diff (old_device : max4live_device) (new_device : max4live_device) : max4live_device_patch =
  if old_device.id <> new_device.id then
    failwith (Printf.sprintf "Cannot diff two Max4LiveDevices with different IDs: %d vs %d (Name: %s)"
                old_device.id new_device.id old_device.device_name)
  else
    let display_name_change =
      diff_atomic_value (module String) old_device.display_name new_device.display_name
    in
    let enabled_change =
      diff_complex_value (module DeviceParam) old_device.enabled new_device.enabled
    in
    let patch_ref_change =
      (diff_complex_value (module PatchRef) old_device.patch_ref new_device.patch_ref :> (PatchRef.t, PatchRef.Patch.t) structured_change)
    in
    let params_change =
      diff_list_id (module Max4LiveParam) old_device.params new_device.params
      |> filter_changes (module Max4LiveParam.Patch)
    in
    let preset_change =
      diff_complex_value_opt (module PresetRef) old_device.preset new_device.preset
    in
    {
      id = new_device.id;
      device_name = new_device.device_name;
      display_name = display_name_change;
      enabled = enabled_change;
      patch_ref = patch_ref_change;
      params = params_change;
      preset = preset_change
    }

(* group_device diff functions *)
and  branch_diff (old_branch : branch) (new_branch : branch) =
  if old_branch.id <> new_branch.id then
    failwith (Printf.sprintf "Cannot diff two Branches with different Ids: %d vs %d" old_branch.id new_branch.id)
  else
    let id_change = `Unchanged in (* IDs must be the same *)
    (* Minimal delegation module to avoid circular dependencies *)
    let module DeviceId = struct
      type t = device
      let equal = (=)
      let has_same_id a b =
        match a, b with
        | Regular ra, Regular rb -> ra.id = rb.id
        | Plugin pa, Plugin pb -> pa.id = pb.id
        | Group ga, Group gb -> ga.id = gb.id
        | Max4Live ma, Max4Live mb -> ma.id = mb.id
        | _ -> false
      let id_hash = function
        | Regular r -> Hashtbl.hash r.id
        | Plugin p -> Hashtbl.hash p.id
        | Group g -> Hashtbl.hash g.id
        | Max4Live m -> Hashtbl.hash m.id

      module Patch = struct
        type t = device_patch
        (* Use mutable references that will be initialized at module load time.
           The device_patch_is_empty_ref will be set after the mutually recursive
           helpers are defined at the end of the file. *)
        let is_empty p = !device_patch_is_empty_ref p
      end

      let diff old_dev new_dev =
        match old_dev, new_dev with
        | Regular ro, Regular rn -> RegularPatch (regular_device_diff ro rn)
        | Plugin po, Plugin pn -> PluginPatch (plugin_device_diff po pn)
        | Group go, Group gn -> GroupPatch (group_device_diff go gn)
        | Max4Live mo, Max4Live mn -> Max4LivePatch (max4live_device_diff mo mn)
        | _ ->
          let t1, id1 = get_device_type_and_id old_dev in
          let t2, id2 = get_device_type_and_id new_dev in
          failwith (Printf.sprintf "Cannot diff devices of different types: %s(Id=%d) vs %s(Id=%d)"
                      t1 id1 t2 id2)
    end in
    let devices_changes =
      diff_list_id (module DeviceId) old_branch.devices new_branch.devices
      |> filter_changes (module DeviceId.Patch)
    in
    let mixer_change = diff_complex_value_id (module MixerDevice) old_branch.mixer new_branch.mixer in
    {
      id = id_change;
      devices = devices_changes;
      mixer = mixer_change;
    }
and group_device_diff (old_group : group_device) (new_group : group_device) =
  if old_group.id <> new_group.id then
    failwith (Printf.sprintf "Cannot diff two GroupDevices with different Ids: %d vs %d (Name: %s)"
                old_group.id new_group.id old_group.device_name)
  else
    let display_name_change = diff_atomic_value (module String) old_group.display_name new_group.display_name in
    let enabled_change = diff_complex_value (module DeviceParam) old_group.enabled new_group.enabled in
    let preset_change = diff_complex_value_opt (module PresetRef) old_group.preset new_group.preset in
    let branches_changes =
      let module BranchId = struct
        type t = branch
        let equal = (=)
        let has_same_id (a : t) (b : t) = a.id = b.id
        let id_hash (t : t) = Hashtbl.hash t.id
        module Patch = struct
          type t = branch_patch
          (* Use mutable reference that will be initialized at module load time *)
          let is_empty p = !branch_patch_is_empty_ref p
        end
        let diff = branch_diff
      end in
      diff_list_id (module BranchId) old_group.branches new_group.branches
      |> filter_changes (module BranchId.Patch)
    in
    let macros_changes =
      diff_list_id (module Macro) old_group.macros new_group.macros
      |> filter_changes (module Macro.Patch)
    in
    let snapshots_changes =
      diff_list_id (module Snapshot) old_group.snapshots new_group.snapshots
      |> filter_changes (module Snapshot.Patch)
    in
    {
      id = new_group.id;
      device_name = new_group.device_name;
      display_name = display_name_change;
      enabled = enabled_change;
      preset = preset_change;
      branches = branches_changes;
      macros = macros_changes;
      snapshots = snapshots_changes;
    }




(* ================== Device modules ================== *)
module RegularDevice = struct
  (** All the built-in devices *)
  type t = regular_device [@@deriving eq]

  let queries = [
    Upath2.query_of_path "/Pointee@Id";
    Upath2.query_of_path "/ShouldShowPresetName@Value";
    Upath2.query_of_path "/UserName@Value";
    (* PresetRef inlined *)
    Upath2.query_of_path "/LastPresetRef/Value/*/FileRef/RelativePath@Value";
    Upath2.query_of_path "/LastPresetRef/Value/*/FileRef/Path@Value";
    Upath2.query_of_path "/LastPresetRef/Value/*/FileRef/LivePackName@Value";
    Upath2.query_of_path "/LastPresetRef/Value/*/FileRef/LivePackId@Value";
    Upath2.query_of_path "/LastPresetRef/Value/*/FileRef/OriginalFileSize@Value";
    Upath2.query_of_path "/LastPresetRef/Value/*/FileRef/OriginalCrc@Value";
    Upath2.query_of_path "/LastPresetRef/Value/*/DeviceId@Name";
    Upath2.query_of_path "/LastPresetRef/Value/*";
    (* Enabled DeviceParam inlined *)
    Upath2.query_of_path "/On/AutomationTarget@Id";
    Upath2.query_of_path "/On/ModulationTarget@Id";
    Upath2.query_of_path "/On/Manual@Value";
    Upath2.query_of_path "/On/KeyMidi/NoteOrController@Value";
    Upath2.query_of_path "/On/KeyMidi/Channel@Value";
    Upath2.query_of_path "/On/KeyMidi/IsNote@Value";
    Upath2.query_of_path "/On/KeyMidi/ControllerMapMode@Value";
    Upath2.query_of_path "/On/MidiControllerRange/Min@Value";
    Upath2.query_of_path "/On/MidiControllerRange/Max@Value";
    Upath2.query_of_path "/On/MidiCCOnOffThresholds/Min@Value";
    Upath2.query_of_path "/On/MidiCCOnOffThresholds/Max@Value";
  ]

  let create (xml : Xml.t) : t =
    match xml with
    | Xml.Element { name; attrs = root_attrs; _ } ->
      let stream = Upath2.stream_of_xml xml in
      let nfa = Upath2.compile queries in
      let results = Upath2.evaluate nfa stream in
      let id = int_of_string (List.assoc "Id" root_attrs) in
      let pointee = Option.get (Upath2.query_int_attr results 0 "Id") in
      (* PresetRef from inlined queries *)
      let preset_root = Upath2.find_result results 10 in
      let preset_type = match preset_root with
        | Some r -> (match r.Upath2.element_name with
            | "AbletonDefaultPresetRef" -> PresetRef.DefaultPreset
            | _ -> PresetRef.UserPreset)
        | None -> PresetRef.UserPreset
      in
      let preset_id = match preset_root with
        | Some r -> (match Upath2.get_attr r "Id" with
            | Some v -> int_of_string v
            | None -> 0)
        | None -> 0
      in
      let preset =
        match Upath2.query_attr results 4 "Value" with
        | Some _ ->
          let relative_path = Option.get (Upath2.query_attr results 3 "Value") in
          let path = Option.get (Upath2.query_attr results 4 "Value") in
          let preset_file_name =
            Filename.basename path |> Filename.remove_extension
          in
          let name = match preset_type with
            | PresetRef.UserPreset -> preset_file_name
            | PresetRef.DefaultPreset ->
              let device_name = Option.value
                  (Upath2.query_attr results 9 "Name") ~default:"" in
              if device_name <> "" then device_name else preset_file_name
          in
          let pack_name = Option.get (Upath2.query_attr results 5 "Value") in
          let pack_id =
            Upath2.query_int_attr results 6 "Value"
            |> Option.value ~default:0
          in
          let file_size =
            Option.get (Upath2.query_int_attr results 7 "Value")
          in
          let crc = Upath2.query_int_attr results 8 "Value"
            |> Option.value ~default:0
          in
          Some { PresetRef.id = preset_id; name; preset_type;
                 relative_path; path; pack_name; pack_id; file_size; crc }
        | None -> None
      in
      let display_name =
        let show_preset = Upath2.query_bool_attr results 1 "Value"
          |> Option.value ~default:false in
        if show_preset && Option.is_some preset then
          Option.get preset |> fun p -> p.PresetRef.name
        else
          let user_name = Upath2.query_attr results 2 "Value"
            |> Option.value ~default:"" in
          if user_name <> "" then user_name else name
      in
      (* Enabled DeviceParam from results *)
      let enabled = MixerDevice.make_dp results ~path:"On" ~qid_base:11 in
      (* Multi-match params: DOM fallback *)
      let params = Upath.find_all "/**/LomId/../Manual/.." xml
        |> List.map DeviceParam.create_from_upath_find
      in
      { id; device_name = name; display_name; pointee; enabled; params; preset }
    | _ -> raise (Xml.Xml_error (xml, "Invalid XML element for creating Device"))

  module Patch = struct
    type t = regular_device_patch

    let is_empty_ref : (t -> bool) ref = ref (fun _ -> failwith "RegularDevice.Patch.is_empty not initialized")
    let is_empty p = !is_empty_ref p
  end

  let diff (old_device : t) (new_device : t) : Patch.t =
    regular_device_diff old_device new_device
end


module PluginDevice = struct
  type t = plugin_device [@@deriving eq]

  let queries = [
    Upath2.query_of_path "/Pointee@Id";
    Upath2.query_of_path "/ShouldShowPresetName@Value";
    Upath2.query_of_path "/UserName@Value";
    (* PresetRef inlined *)
    Upath2.query_of_path "/LastPresetRef/Value/*/FileRef/RelativePath@Value";
    Upath2.query_of_path "/LastPresetRef/Value/*/FileRef/Path@Value";
    Upath2.query_of_path "/LastPresetRef/Value/*/FileRef/LivePackName@Value";
    Upath2.query_of_path "/LastPresetRef/Value/*/FileRef/LivePackId@Value";
    Upath2.query_of_path "/LastPresetRef/Value/*/FileRef/OriginalFileSize@Value";
    Upath2.query_of_path "/LastPresetRef/Value/*/FileRef/OriginalCrc@Value";
    Upath2.query_of_path "/LastPresetRef/Value/*/DeviceId@Name";
    Upath2.query_of_path "/LastPresetRef/Value/*";
    (* Enabled DeviceParam inlined *)
    Upath2.query_of_path "/On/AutomationTarget@Id";
    Upath2.query_of_path "/On/ModulationTarget@Id";
    Upath2.query_of_path "/On/Manual@Value";
    Upath2.query_of_path "/On/KeyMidi/NoteOrController@Value";
    Upath2.query_of_path "/On/KeyMidi/Channel@Value";
    Upath2.query_of_path "/On/KeyMidi/IsNote@Value";
    Upath2.query_of_path "/On/KeyMidi/ControllerMapMode@Value";
    Upath2.query_of_path "/On/MidiControllerRange/Min@Value";
    Upath2.query_of_path "/On/MidiControllerRange/Max@Value";
    Upath2.query_of_path "/On/MidiCCOnOffThresholds/Min@Value";
    Upath2.query_of_path "/On/MidiCCOnOffThresholds/Max@Value";
  ]

  let create (xml : Xml.t) : t =
    match xml with
    | Xml.Element { name; attrs = root_attrs; _ } ->
      let stream = Upath2.stream_of_xml xml in
      let nfa = Upath2.compile queries in
      let results = Upath2.evaluate nfa stream in
      let id = int_of_string (List.assoc "Id" root_attrs) in
      let pointee = Option.get (Upath2.query_int_attr results 0 "Id") in
      (* PresetRef from inlined queries *)
      let preset_root = Upath2.find_result results 10 in
      let preset_type = match preset_root with
        | Some r -> (match r.Upath2.element_name with
            | "AbletonDefaultPresetRef" -> PresetRef.DefaultPreset
            | _ -> PresetRef.UserPreset)
        | None -> PresetRef.UserPreset
      in
      let preset_id = match preset_root with
        | Some r -> (match Upath2.get_attr r "Id" with
            | Some v -> int_of_string v
            | None -> 0)
        | None -> 0
      in
      let preset =
        match Upath2.query_attr results 4 "Value" with
        | Some _ ->
          let relative_path = Option.get (Upath2.query_attr results 3 "Value") in
          let path = Option.get (Upath2.query_attr results 4 "Value") in
          let preset_file_name =
            Filename.basename path |> Filename.remove_extension
          in
          let name = match preset_type with
            | PresetRef.UserPreset -> preset_file_name
            | PresetRef.DefaultPreset ->
              let device_name = Option.value
                  (Upath2.query_attr results 9 "Name") ~default:"" in
              if device_name <> "" then device_name else preset_file_name
          in
          let pack_name = Option.get (Upath2.query_attr results 5 "Value") in
          let pack_id =
            Upath2.query_int_attr results 6 "Value"
            |> Option.value ~default:0
          in
          let file_size =
            Option.get (Upath2.query_int_attr results 7 "Value")
          in
          let crc = Upath2.query_int_attr results 8 "Value"
            |> Option.value ~default:0
          in
          Some { PresetRef.id = preset_id; name; preset_type;
                 relative_path; path; pack_name; pack_id; file_size; crc }
        | None -> None
      in
      let display_name =
        let show_preset = Upath2.query_bool_attr results 1 "Value"
          |> Option.value ~default:false in
        if show_preset && Option.is_some preset then
          Option.get preset |> fun p -> p.PresetRef.name
        else
          let user_name = Upath2.query_attr results 2 "Value"
            |> Option.value ~default:"" in
          if user_name <> "" then user_name else name
      in
      let enabled = MixerDevice.make_dp results ~path:"On" ~qid_base:11 in
      (* DOM fallback for PluginDesc *)
      let plugin_desc_xml = Upath.find "/PluginDesc" xml |> snd in
      let desc = PluginDesc.create plugin_desc_xml in
      let device_name = desc.name in
      (* DOM fallback for multi-match PluginParams *)
      let params =
        Upath.find_all "/ParameterList/*" xml
        |> List.map snd
        |> List.map PluginParam.create
      in
      { id; device_name; display_name; pointee; enabled; desc; params; preset }
    | _ -> raise (Xml.Xml_error (xml, "Invalid XML element for creating PluginDevice"))


  module Patch = struct
    type t = plugin_device_patch

    (* Use reference to break circular dependency - initialized after
       the mutually recursive helpers are defined at end of file *)
    let is_empty_ref : (t -> bool) ref = ref (fun _ -> failwith "PluginDevice.Patch.is_empty not initialized")
    let is_empty p = !is_empty_ref p
  end

  let has_same_id (a : t) (b : t) = a.id = b.id

  let diff = plugin_device_diff
end


module Branch = struct
  type t = branch [@@deriving eq]

  let has_same_id a b = a.id = b.id

  let id_hash t = Hashtbl.hash t.id

  let create (device_creator : Xml.t -> device) (xml : Xml.t) : t =
    let id = Xml.get_int_attr "Id" xml in
    let mixer = Upath.find "MixerDevice" xml |> snd |> MixerDevice.create in
    let devices = Upath.find "/DeviceChain/*/Devices" xml
      |> snd
      |> Xml.get_childs
      |> List.map device_creator
    in
    { id; devices; mixer }

  module Patch = struct
    type t = branch_patch

    (* Use references to break circular dependency - initialized after
       the mutually recursive helpers are defined at end of file *)
    let is_empty_ref : (t -> bool) ref = ref (fun _ -> failwith "Branch.Patch.is_empty not initialized")
    let is_empty p = !is_empty_ref p
  end

  let diff = branch_diff
end


module GroupDevice = struct
  type t = group_device [@@deriving eq]

  let has_same_id a b = a.id = b.id

  let id_hash t = Hashtbl.hash t.id

  let queries = [
    Upath2.query_of_path "/Pointee@Id";
    Upath2.query_of_path "/ShouldShowPresetName@Value";
    Upath2.query_of_path "/UserName@Value";
    (* PresetRef inlined *)
    Upath2.query_of_path "/LastPresetRef/Value/*/FileRef/RelativePath@Value";
    Upath2.query_of_path "/LastPresetRef/Value/*/FileRef/Path@Value";
    Upath2.query_of_path "/LastPresetRef/Value/*/FileRef/LivePackName@Value";
    Upath2.query_of_path "/LastPresetRef/Value/*/FileRef/LivePackId@Value";
    Upath2.query_of_path "/LastPresetRef/Value/*/FileRef/OriginalFileSize@Value";
    Upath2.query_of_path "/LastPresetRef/Value/*/FileRef/OriginalCrc@Value";
    Upath2.query_of_path "/LastPresetRef/Value/*/DeviceId@Name";
    Upath2.query_of_path "/LastPresetRef/Value/*";
    (* Enabled DeviceParam inlined *)
    Upath2.query_of_path "/On/AutomationTarget@Id";
    Upath2.query_of_path "/On/ModulationTarget@Id";
    Upath2.query_of_path "/On/Manual@Value";
    Upath2.query_of_path "/On/KeyMidi/NoteOrController@Value";
    Upath2.query_of_path "/On/KeyMidi/Channel@Value";
    Upath2.query_of_path "/On/KeyMidi/IsNote@Value";
    Upath2.query_of_path "/On/KeyMidi/ControllerMapMode@Value";
    Upath2.query_of_path "/On/MidiControllerRange/Min@Value";
    Upath2.query_of_path "/On/MidiControllerRange/Max@Value";
    Upath2.query_of_path "/On/MidiCCOnOffThresholds/Min@Value";
    Upath2.query_of_path "/On/MidiCCOnOffThresholds/Max@Value";
  ]

  let create (device_creator : Xml.t -> device) (xml : Xml.t) : t =
    match xml with
    | Xml.Element { name; attrs = root_attrs; _ } ->
      let stream = Upath2.stream_of_xml xml in
      let nfa = Upath2.compile queries in
      let results = Upath2.evaluate nfa stream in
      let id = int_of_string (List.assoc "Id" root_attrs) in
      let pointee = Option.get (Upath2.query_int_attr results 0 "Id") in
      (* PresetRef from inlined queries *)
      let preset_root = Upath2.find_result results 10 in
      let preset_type = match preset_root with
        | Some r -> (match r.Upath2.element_name with
            | "AbletonDefaultPresetRef" -> PresetRef.DefaultPreset
            | _ -> PresetRef.UserPreset)
        | None -> PresetRef.UserPreset
      in
      let preset_id = match preset_root with
        | Some r -> (match Upath2.get_attr r "Id" with
            | Some v -> int_of_string v
            | None -> 0)
        | None -> 0
      in
      let preset =
        match Upath2.query_attr results 4 "Value" with
        | Some _ ->
          let relative_path = Option.get (Upath2.query_attr results 3 "Value") in
          let path = Option.get (Upath2.query_attr results 4 "Value") in
          let preset_file_name =
            Filename.basename path |> Filename.remove_extension
          in
          let name = match preset_type with
            | PresetRef.UserPreset -> preset_file_name
            | PresetRef.DefaultPreset ->
              let device_name = Option.value
                  (Upath2.query_attr results 9 "Name") ~default:"" in
              if device_name <> "" then device_name else preset_file_name
          in
          let pack_name = Option.get (Upath2.query_attr results 5 "Value") in
          let pack_id =
            Upath2.query_int_attr results 6 "Value"
            |> Option.value ~default:0
          in
          let file_size =
            Option.get (Upath2.query_int_attr results 7 "Value")
          in
          let crc = Upath2.query_int_attr results 8 "Value"
            |> Option.value ~default:0
          in
          Some { PresetRef.id = preset_id; name; preset_type;
                 relative_path; path; pack_name; pack_id; file_size; crc }
        | None -> None
      in
      let display_name =
        let show_preset = Upath2.query_bool_attr results 1 "Value"
          |> Option.value ~default:false in
        if show_preset && Option.is_some preset then
          Option.get preset |> fun p -> p.PresetRef.name
        else
          let user_name = Upath2.query_attr results 2 "Value"
            |> Option.value ~default:"" in
          if user_name <> "" then user_name else name
      in
      let enabled = MixerDevice.make_dp results ~path:"On" ~qid_base:11 in
      (* DOM fallback for branches, macros, snapshots *)
      let branches = Upath.find "/Branches" xml
        |> snd
        |> Xml.get_childs
        |> List.map (Branch.create device_creator)
      in
      let macro_names_xml = Upath.find_all "/'MacroDisplayNames\\.[0-9]+$'" xml in
      let macro_controls_xml = Upath.find_all "/'MacroControls\\.[0-9]+$'" xml in
      let macros =
        List.combine macro_names_xml macro_controls_xml
        |> List.map (fun (n,c) ->
            let element_name = match (snd n) with
              | Xml.Element { name = ename; _ } -> ename
              | Xml.Data _ -> raise (Xml.Xml_error (snd n, "Expected Element, got Data"))
            in
            let index = extract_index_from_name element_name in
            let macro = Macro.create (snd n) (snd c) in
            (index, macro)
          )
        |> List.sort (fun (i1, _) (i2, _) -> Stdlib.compare i1 i2)
        |> List.map snd
      in
      let snapshots =
        Upath.find_all "/MacroVariations/MacroSnapshots/MacroSnapshot" xml
        |> List.map snd
        |> List.map Snapshot.create
      in
      { id; device_name=name; display_name; pointee; enabled; branches; macros; snapshots; preset }
    | _ -> invalid_arg "Cannot create a GroupDevice on Data"

  module Patch = struct
    type t = group_device_patch

    (* Use reference to break circular dependency - initialized after
       the mutually recursive helpers are defined at end of file *)
    let is_empty_ref : (t -> bool) ref = ref (fun _ -> failwith "GroupDevice.Patch.is_empty not initialized")
    let is_empty p = !is_empty_ref p
  end

  let diff = group_device_diff
end


module Max4LiveDevice = struct
  type t = max4live_device [@@deriving eq]

  let queries = [
    Upath2.query_of_path "/Pointee@Id";
    Upath2.query_of_path "/ShouldShowPresetName@Value";
    Upath2.query_of_path "/UserName@Value";
    (* PresetRef inlined *)
    Upath2.query_of_path "/LastPresetRef/Value/*/FileRef/RelativePath@Value";
    Upath2.query_of_path "/LastPresetRef/Value/*/FileRef/Path@Value";
    Upath2.query_of_path "/LastPresetRef/Value/*/FileRef/LivePackName@Value";
    Upath2.query_of_path "/LastPresetRef/Value/*/FileRef/LivePackId@Value";
    Upath2.query_of_path "/LastPresetRef/Value/*/FileRef/OriginalFileSize@Value";
    Upath2.query_of_path "/LastPresetRef/Value/*/FileRef/OriginalCrc@Value";
    Upath2.query_of_path "/LastPresetRef/Value/*/DeviceId@Name";
    Upath2.query_of_path "/LastPresetRef/Value/*";
    (* PatchRef inlined *)
    Upath2.query_of_path "/PatchSlot/Value/MxPatchRef/FileRef/RelativePath@Value";
    Upath2.query_of_path "/PatchSlot/Value/MxPatchRef/FileRef/Path@Value";
    Upath2.query_of_path "/PatchSlot/Value/MxPatchRef/FileRef/LivePackName@Value";
    Upath2.query_of_path "/PatchSlot/Value/MxPatchRef/FileRef/LivePackId@Value";
    Upath2.query_of_path "/PatchSlot/Value/MxPatchRef/FileRef/OriginalFileSize@Value";
    Upath2.query_of_path "/PatchSlot/Value/MxPatchRef/FileRef/OriginalCrc@Value";
    Upath2.query_of_path "/PatchSlot/Value/MxPatchRef/LastModDate@Value";
    (* Enabled DeviceParam inlined *)
    Upath2.query_of_path "/On/AutomationTarget@Id";
    Upath2.query_of_path "/On/ModulationTarget@Id";
    Upath2.query_of_path "/On/Manual@Value";
    Upath2.query_of_path "/On/KeyMidi/NoteOrController@Value";
    Upath2.query_of_path "/On/KeyMidi/Channel@Value";
    Upath2.query_of_path "/On/KeyMidi/IsNote@Value";
    Upath2.query_of_path "/On/KeyMidi/ControllerMapMode@Value";
    Upath2.query_of_path "/On/MidiControllerRange/Min@Value";
    Upath2.query_of_path "/On/MidiControllerRange/Max@Value";
    Upath2.query_of_path "/On/MidiCCOnOffThresholds/Min@Value";
    Upath2.query_of_path "/On/MidiCCOnOffThresholds/Max@Value";
  ]

  let create (xml : Xml.t) : t =
    match xml with
    | Xml.Element { name; attrs = root_attrs; _ } ->
      let stream = Upath2.stream_of_xml xml in
      let nfa = Upath2.compile queries in
      let results = Upath2.evaluate nfa stream in
      let id = int_of_string (List.assoc "Id" root_attrs) in
      let pointee = Option.get (Upath2.query_int_attr results 0 "Id") in
      (* PresetRef from inlined queries *)
      let preset_root = Upath2.find_result results 10 in
      let preset_type = match preset_root with
        | Some r -> (match r.Upath2.element_name with
            | "AbletonDefaultPresetRef" -> PresetRef.DefaultPreset
            | _ -> PresetRef.UserPreset)
        | None -> PresetRef.UserPreset
      in
      let preset_id = match preset_root with
        | Some r -> (match Upath2.get_attr r "Id" with
            | Some v -> int_of_string v
            | None -> 0)
        | None -> 0
      in
      let preset =
        match Upath2.query_attr results 4 "Value" with
        | Some _ ->
          let relative_path = Option.get (Upath2.query_attr results 3 "Value") in
          let path = Option.get (Upath2.query_attr results 4 "Value") in
          let preset_file_name =
            Filename.basename path |> Filename.remove_extension
          in
          let name = match preset_type with
            | PresetRef.UserPreset -> preset_file_name
            | PresetRef.DefaultPreset ->
              let device_name = Option.value
                  (Upath2.query_attr results 9 "Name") ~default:"" in
              if device_name <> "" then device_name else preset_file_name
          in
          let pack_name = Option.get (Upath2.query_attr results 5 "Value") in
          let pack_id =
            Upath2.query_int_attr results 6 "Value"
            |> Option.value ~default:0
          in
          let file_size =
            Option.get (Upath2.query_int_attr results 7 "Value")
          in
          let crc = Upath2.query_int_attr results 8 "Value"
            |> Option.value ~default:0
          in
          Some { PresetRef.id = preset_id; name; preset_type;
                 relative_path; path; pack_name; pack_id; file_size; crc }
        | None -> None
      in
      let display_name =
        let show_preset = Upath2.query_bool_attr results 1 "Value"
          |> Option.value ~default:false in
        if show_preset && Option.is_some preset then
          Option.get preset |> fun p -> p.PresetRef.name
        else
          let user_name = Upath2.query_attr results 2 "Value"
            |> Option.value ~default:"" in
          if user_name <> "" then user_name else name
      in
      let enabled = MixerDevice.make_dp results ~path:"On" ~qid_base:18 in
      (* PatchRef from inlined queries *)
      let patch_ref =
        let relative_path = Option.get (Upath2.query_attr results 11 "Value") in
        let path = Option.get (Upath2.query_attr results 12 "Value") in
        let patch_name = Filename.basename path |> Filename.remove_extension in
        let pack_name = Option.get (Upath2.query_attr results 13 "Value") in
        let pack_id = Upath2.query_int_attr results 14 "Value" |> Option.value ~default:0 in
        let file_size = Option.get (Upath2.query_int_attr results 15 "Value") in
        let crc = Upath2.query_int_attr results 16 "Value" |> Option.value ~default:0 in
        let last_mod_date = Upath2.query_int_attr results 17 "Value" |> Option.value ~default:0 in
        { PatchRef.id = 0; name = patch_name;
          preset_type = UserPreset; relative_path; path;
          pack_name; pack_id; file_size; crc; last_mod_date }
      in
      let device_name = patch_ref.PatchRef.name in
      (* DOM fallback for multi-match M4L params *)
      let float_params = Alsdiff_base.Upath.find_all "**/MxDFloatParameter" xml in
      let int_params = Alsdiff_base.Upath.find_all "**/MxDIntParameter" xml in
      let bool_params = Alsdiff_base.Upath.find_all "**/MxDBoolParameter" xml in
      let enum_params = Alsdiff_base.Upath.find_all "**/MxDEnumParameter" xml in
      let all_params = float_params @ int_params @ bool_params @ enum_params in
      let params = List.map Max4LiveParam.create_from_upath_find all_params in
      { id; device_name; display_name; pointee; enabled; patch_ref; params; preset }
    | _ -> raise (Xml.Xml_error (xml, "Invalid XML element for creating Max4LiveDevice"))

  module Patch = struct
    type t = max4live_device_patch

    (* Use reference to break circular dependency - initialized after
       the mutually recursive helpers are defined at end of file *)
    let is_empty_ref : (t -> bool) ref = ref (fun _ -> failwith "Max4LiveDevice.Patch.is_empty not initialized")
    let is_empty p = !is_empty_ref p
  end

  let has_same_id (a : t) (b : t) = a.id = b.id

  let diff = max4live_device_diff
end


type t = device [@@deriving eq]

let rec create (xml : Xml.t) : t =
  match xml with
  | Xml.Element { name; _ } ->
    (match name with
     | "InstrumentGroupDevice" | "DrumGroupDevice" | "MidiEffectGroupDevice" | "AudioEffectGroupDevice" ->
       Group (GroupDevice.create create xml)
     | "PluginDevice" | "AuPluginDevice" ->
       Plugin (PluginDevice.create xml)
     | "MxDeviceInstrument" | "MxDeviceAudioEffect" | "MxDeviceMidiEffect" ->
       Max4Live (Max4LiveDevice.create xml)
     | _ -> Regular (RegularDevice.create xml))

  | _ -> invalid_arg "Cannot create a Device on Data"

let has_same_id old_device new_device =
  match old_device, new_device with
  | Regular old_reg, Regular new_reg -> old_reg.id = new_reg.id
  | Plugin old_plug, Plugin new_plug -> old_plug.id = new_plug.id
  | Group old_group, Group new_group -> old_group.id = new_group.id
  | Max4Live old_m4l, Max4Live new_m4l -> old_m4l.id = new_m4l.id
  | _ -> false

let id_hash device =
  match device with
  | Regular reg -> Hashtbl.hash reg.id
  | Plugin plug -> Hashtbl.hash plug.id
  | Max4Live m4l -> Hashtbl.hash m4l.id
  | Group group -> Hashtbl.hash group.id

module Patch = struct
  type t =
    | RegularPatch of RegularDevice.Patch.t
    | PluginPatch of PluginDevice.Patch.t
    | Max4LivePatch of Max4LiveDevice.Patch.t
    | GroupPatch of GroupDevice.Patch.t

  (* Use reference to break circular dependency - initialized after
     the mutually recursive helpers are defined *)
  let is_empty_ref : (t -> bool) ref = ref (fun _ -> failwith "is_empty not initialized")
  let is_empty p = !is_empty_ref p
end

(* ================== Mutually Recursive is_empty Helpers ================== *)
(* These functions are defined here at the end of the file where all modules
   are available, to properly handle the circular dependency between
   Branch.Patch and Device.Patch. *)

let rec is_unchanged_branch_change = function
  | `Added _ | `Removed _ -> false
  | `Unchanged -> true
  | `Modified p -> branch_patch_is_empty p

and is_unchanged_device_change = function
  | `Added _ | `Removed _ -> false
  | `Unchanged -> true
  | `Modified p -> device_patch_is_empty p

and branch_patch_is_empty (p : branch_patch) =
  is_unchanged_atomic_update p.id &&
  is_unchanged_update (module MixerDevice.Patch) p.mixer &&
  List.for_all is_unchanged_device_change p.devices

and device_patch_is_empty = function
  | RegularPatch rp ->
    is_unchanged_atomic_update rp.display_name &&
    is_unchanged_change (module PresetRef.Patch) rp.preset &&
    List.for_all (is_unchanged_change (module DeviceParam.Patch)) rp.params
  | PluginPatch pp ->
    is_unchanged_atomic_update pp.display_name &&
    is_unchanged_update (module DeviceParam.Patch) pp.enabled &&
    is_unchanged_update (module PluginDesc.Patch) pp.desc &&
    is_unchanged_change (module PresetRef.Patch) pp.preset &&
    List.for_all (is_unchanged_change (module PluginParam.Patch)) pp.params
  | Max4LivePatch mp ->
    is_unchanged_atomic_update mp.display_name &&
    is_unchanged_update (module DeviceParam.Patch) mp.enabled &&
    is_unchanged_change (module PatchRef.Patch) mp.patch_ref &&
    is_unchanged_change (module PresetRef.Patch) mp.preset &&
    List.for_all (is_unchanged_change (module Max4LiveParam.Patch)) mp.params
  | GroupPatch gp ->
    is_unchanged_atomic_update gp.display_name &&
    is_unchanged_update (module DeviceParam.Patch) gp.enabled &&
    is_unchanged_change (module PresetRef.Patch) gp.preset &&
    List.for_all is_unchanged_branch_change gp.branches &&
    List.for_all (is_unchanged_change (module Macro.Patch)) gp.macros &&
    List.for_all (is_unchanged_change (module Snapshot.Patch)) gp.snapshots

(* ================== Initialize Forward References ================== *)
(* Initialize all mutable references for is_empty functions.
   This must come after the mutually recursive helpers are defined. *)

(* Note: Patch.t and device_patch are nominally different types in OCaml,
   so we need this wrapper to convert between them *)
let () = Patch.is_empty_ref := (function
    | Patch.RegularPatch p -> device_patch_is_empty (RegularPatch p)
    | Patch.PluginPatch p -> device_patch_is_empty (PluginPatch p)
    | Patch.Max4LivePatch p -> device_patch_is_empty (Max4LivePatch p)
    | Patch.GroupPatch p -> device_patch_is_empty (GroupPatch p))

let () = device_patch_is_empty_ref := device_patch_is_empty
let () = branch_patch_is_empty_ref := branch_patch_is_empty
let () = Branch.Patch.is_empty_ref := branch_patch_is_empty

(* Initialize submodule is_empty references using centralized logic *)
let () = RegularDevice.Patch.is_empty_ref := (fun p -> device_patch_is_empty (RegularPatch p))
let () = PluginDevice.Patch.is_empty_ref := (fun p -> device_patch_is_empty (PluginPatch p))
let () = Max4LiveDevice.Patch.is_empty_ref := (fun p -> device_patch_is_empty (Max4LivePatch p))
let () = GroupDevice.Patch.is_empty_ref := (fun p -> device_patch_is_empty (GroupPatch p))



let diff (old_device : t) (new_device : t) : Patch.t =
  match (old_device, new_device) with
  | (Regular old_reg, Regular new_reg) ->
    let patch = RegularDevice.diff old_reg new_reg in
    Patch.RegularPatch patch
  | (Group old_group, Group new_group) ->
    let patch = GroupDevice.diff old_group new_group in
    Patch.GroupPatch patch
  | (Plugin old_plug, Plugin new_plug) ->
    let patch = PluginDevice.diff old_plug new_plug in
    Patch.PluginPatch patch
  | (Max4Live old_m4l, Max4Live new_m4l) ->
    let patch = Max4LiveDevice.diff old_m4l new_m4l in
    Patch.Max4LivePatch patch
  | _ ->
    let t1, id1 = get_device_type_and_id old_device in
    let t2, id2 = get_device_type_and_id new_device in
    failwith (Printf.sprintf "Cannot diff devices of different types: %s(Id=%d) vs %s(Id=%d)"
                t1 id1 t2 id2)
