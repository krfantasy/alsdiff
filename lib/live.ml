
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

type _ clip =
  | AudioClip : audio_clip -> audio_clip clip
  | MIDIClip : midi_clip -> midi_clip clip

type patch =
  [ `EnvelopePatch of string * float array (* (path, events) *)
  | `SampleRefPatch of string * string * int64 * int64 (* path, filename, size, modified date *)
  ]
