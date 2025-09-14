(*
type envelope_event = {
  id : int;
  time : float;
  value : float;
  others : (string * float) list;
}

type envelope = {
  id : int;
  target : int;
  events : envelope_event array;
}

(* let diff_envelope e1 e2 = *)
(*   assert (e1.id = e2.id && e1.target = e2.target); *)
(*   let diff_events ev1 ev2 = *)
(*     let len1 = Array.length ev1 and *)
(*     let len2 = Array.length ev2 in *)
(*     if len1 > len2 then *)
(*       "envelope events deleted" *)
(*     else if len1 < len2 then *)
(*       "envelope events added" *)
(*     else *)
(*       let sorted_e1 = *)

type live_object_location = {
  track : int;
  parent : int;
}

type send = {
  target : int;   (** The Id of targeting return track *)
  value : float;  (** The value of the send, usually between 0.0 and 1.0 *)
}

type mixer = {
  on : bool;
  solo : bool;
  volume : float;
  pan : float;
  sends : send array; (** The sends to other tracks *)
}

(** A function to compare two mixers and return a list of changes *)
let diff_mixer m1 m2 =
  let changes = [] in
  if m1.on <> m2.on then
    (if m1.on then "on" else "off") :: changes
  else if m1.solo <> m2.solo then
    (if m1.solo then "solo on" else "solo off") :: changes
  else if m1.volume <> m2.volume then
    Printf.sprintf "volume changed from %f to %f" m1.volume m2.volume :: changes
  else if m1.pan <> m2.pan then
    Printf.sprintf "pan changed from %f to %f" m1.pan m2.pan :: changes
  else
    changes

type midi_clip = {
  id : int;
  name : string;
  start : float;
  len : float;
}

type sample_ref = {
  id : int;
  name : string;
  file_path : string;
  crc : string;
}

type audio_clip = {
  id : int;
  name : string;
  start : float;
  len : float;
  sample : sample_ref;
}

type track_color = int

type automation_envelope = {
  id : int;
  selected_device : int;
  selected_envelope : int;
  lane_height : int;
  is_content_selected : bool;
}

type mixer_settings = {
  on : bool;
  solo : bool;
  volume : float;
  pan : float;
  sends : send array;
}

type device_chain = {
  automation_lanes : automation_envelope array;
  devices : device array;
  signal_modulations : signal_modulation array;
}

and device = {
  id : int;
  lom_id : int;
  device_type : string;
  parameters : device_parameter array;
  is_active : bool;
}

and device_parameter = {
  name : string;
  value : float;
  is_value_sample_based : bool;
}

and signal_modulation = {
  source : int;
  target : int;
  amount : float;
}

type clip_slot = {
  id : int;
  lom_id : int;
  has_clip : bool;
  clip : clip option;
}

type _ clip =
  | AudioClip : audio_clip -> audio_clip clip
  | MIDIClip : midi_clip -> midi_clip clip

and audio_clip = {
  id : int;
  name : string;
  start : float;
  len : float;
  sample : sample_ref;
  warp_mode : string;
  warp_on : bool;
}

and midi_clip = {
  id : int;
  name : string;
  start : float;
  len : float;
  notes : midi_note array;
  loop_on : bool;
}

and midi_note = {
  pitch : int;
  velocity : int;
  start_time : float;
  duration : float;
}

type track = {
  id : int;
  lom_id : int;
  track_type : track_type;
  name : string;
  user_name : string;
  annotation : string;
  color : track_color;
  mixer : mixer_settings;
  device_chain : device_chain;
  clip_slots : clip_slot array;
  is_folded : bool;
  is_frozen : bool;
  track_group_id : int;
}

and track_type =
  | AudioTrack
  | MidiTrack
  | GroupTrack
  | ReturnTrack
  | MasterTrack

type scene = {
  id : int;
  name : string;
  clip_slots : (int * clip_slot) array; (* track_id * clip_slot *)
  tempo : float;
  time_signature : time_signature;
}

and time_signature = {
  numerator : int;
  denominator : int;
}

type arrangement = {
  clips : clip array;
  automation : automation_envelope array;
  markers : marker array;
}

and marker = {
  time : float;
  name : string;
  color : track_color;
}

type live_set = {
  major_version : int;
  minor_version : string;
  creator : string;
  revision : string;
  tracks : track array;
  scenes : scene array;
  arrangement : arrangement;
  tempo : float;
  time_signature : time_signature;
  global_quantization : string;
}

type patch =
  [ `EnvelopePatch of string * float array (* (path, events) *)
  | `SampleRefPatch of string * string * int64 * int64 (* path, filename, size, modified date *)
  | `DeviceParameterPatch of int * string * float (* track_id, parameter_name, new_value *)
  | `ClipModificationPatch of int * clip_modification (* track_id, clip_changes *)
  | `TrackMixerPatch of int * mixer_settings (* track_id, new_mixer_settings *)
  ]

and clip_modification = {
  clip_id : int;
  start_time : float option;
  duration : float option;
  name : string option;
  warp_mode : string option;
}

(* Utility functions for Live Object Model *)
let track_type_to_string = function
  | AudioTrack -> "AudioTrack"
  | MidiTrack -> "MidiTrack"
  | GroupTrack -> "GroupTrack"
  | ReturnTrack -> "ReturnTrack"
  | MasterTrack -> "MasterTrack"

let string_to_track_type = function
  | "AudioTrack" -> AudioTrack
  | "MidiTrack" -> MidiTrack
  | "GroupTrack" -> GroupTrack
  | "ReturnTrack" -> ReturnTrack
  | "MasterTrack" -> MasterTrack
  | _ -> AudioTrack (* default *)

(* XML parsing utilities for Live Object Model *)
let parse_bool_value = function
  | "true" -> true
  | "false" -> false
  | _ -> false

let parse_int_value s = try int_of_string s with _ -> 0
let parse_float_value s = try float_of_string s with _ -> 0.0

let parse_track_attributes attrs =
  let id = ref 0 in
  let track_type = ref AudioTrack in
  List.iter (function
    | ("Id", v) -> id := parse_int_value v
    | ("SelectedToolPanel", _) -> () (* Ignored for now *)
    | ("SelectedTransformationName", _) -> () (* Ignored for now *)
    | ("SelectedGeneratorName", _) -> () (* Ignored for now *)
    | _ -> ()
  ) attrs;
  (!id, !track_type)

let parse_value_element name value =
  match name with
  | "LomId" -> `LomId (parse_int_value value)
  | "LomIdView" -> `LomIdView (parse_int_value value)
  | "IsContentSelectedInDocument" -> `IsContentSelected (parse_bool_value value)
  | "PreferredContentViewMode" -> `ContentViewMode (parse_int_value value)
  | "Value" -> `Value (parse_float_value value)
  | "IsValueSampleBased" -> `IsValueSampleBased (parse_bool_value value)
  | "EffectiveName" -> `EffectiveName value
  | "UserName" -> `UserName value
  | "Annotation" -> `Annotation value
  | "Color" -> `Color (parse_int_value value)
  | "TrackGroupId" -> `TrackGroupId (parse_int_value value)
  | "TrackUnfolded" -> `TrackUnfolded (parse_bool_value value)
  | "Freeze" -> `Freeze (parse_bool_value value)
  | "NeedArrangerRefreeze" -> `NeedArrangerRefreeze (parse_bool_value value)
  | _ -> `Unknown (name, value)

(* Basic Live Set parser skeleton *)
let parse_live_set xml_content =
  (* This would be implemented using xmlm or another XML parser *)
  (* For now, return a skeleton live_set structure *)
  {
    major_version = 5;
    minor_version = "12.0_12203";
    creator = "Ableton Live 12.2b9";
    revision = "f032d990e585d9c595825ea7b8a2d3216c174e70";
    tracks = [||];
    scenes = [||];
    arrangement = { clips = [||]; automation = [||]; markers = [||] };
    tempo = 120.0;
    time_signature = { numerator = 4; denominator = 4 };
    global_quantization = "None";
  }

(* Device chain parsing utilities *)
let parse_device_chain _ =
  {
    automation_lanes = [||];
    devices = [||];
    signal_modulations = [||];
  }

(* Track parsing utilities *)
let parse_track track_type attrs content =
  let id, _ = parse_track_attributes attrs in
  {
    id;
    lom_id = 0;
    track_type;
    name = "";
    user_name = "";
    annotation = "";
    color = 0;
    mixer = { on = true; solo = false; volume = 1.0; pan = 0.0; sends = [||] };
    device_chain = parse_device_chain ();
    clip_slots = [||];
    is_folded = false;
    is_frozen = false;
    track_group_id = -1;
  }

(* Live Set creation utilities *)
let create_empty_live_set () =
  {
    major_version = 5;
    minor_version = "0.0";
    creator = "live_git";
    revision = "";
    tracks = [||];
    scenes = [||];
    arrangement = { clips = [||]; automation = [||]; markers = [||] };
    tempo = 120.0;
    time_signature = { numerator = 4; denominator = 4 };
    global_quantization = "None";
  }
*)