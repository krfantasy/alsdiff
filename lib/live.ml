module type LIVE_TYPE = sig
  type t
  val create : Xml.t -> t
end

module TimeSignature = struct
  type t = { numer : int; denom : int } [@@deriving eq]
end

module MidiNote = struct
  type t = {
    time : int;
    duration : int;
    velocity : int;
    off_velocity : int;
    note : int;
  } [@@deriving eq]
end

module LoopSection = struct
  type t = {
    start : int;
    end_ : int;
    on : bool;
  } [@@deriving eq]
end

module Content = struct
  type t =
    | MIDIContent of MidiNote.t list
    | AudioContent of { file_path : string; sample_rate : int; bit_depth : int }
  [@@deriving eq]
end

module Clip = struct
  type t = {
    id : int;
    start : float;
    duration : float;
    loop : LoopSection.t option;
    signature : TimeSignature.t;
    content : Content.t;
  } [@@deriving eq]
end

module EnvelopeEvent = struct
  type t = {
    time : float;
    value : float;
  } [@@deriving eq]

  let create (xml : Xml.t) : t =
    let open Xml in
    let get_attr name attrs =
      try List.assoc name attrs
      with Not_found -> failwith ("Attribute " ^ name ^ " not found")
    in
    match xml with
    | Element { attrs; _ } ->
        {
          time = float_of_string (get_attr "Time" attrs);
          value = float_of_string (get_attr "Value" attrs);
        }
    | Data _ -> failwith "Event must be an element"
end

module AutomationEnvelope = struct
  type t = {
    id : int;
    target : int;
    events : EnvelopeEvent.t list;
  } [@@deriving eq]

  let create (xml : Xml.t) : t =
    let open Xml in
    let get_attr name attrs =
      try List.assoc name attrs
      with Not_found -> failwith ("Attribute " ^ name ^ " not found")
    in
    match xml with
    | Element { attrs; _ } as envelope_node ->
        let id = int_of_string (get_attr "Id" attrs) in
        let target =
          match Upath.find envelope_node "/EnvelopeTarget/PointeeId@Value" with
          | Some (_, Element { attrs = p_attrs; _ }) ->
              int_of_string (get_attr "Value" p_attrs)
          | _ -> failwith "Cannot find target"
        in
        let events =
          Upath.find_all envelope_node "/Automation/Events/FloatEvent"
          |> List.map (fun (_, event) -> EnvelopeEvent.create event)
        in
        { id; target; events }
    | Data _ -> failwith "Envelope must be an element"
end

module Automation = struct
  type t = {
    automation_envelopes : AutomationEnvelope.t list;
  } [@@deriving eq]

  let create (xml : Xml.t) : t =
    let envelopes =
      Upath.find_all xml "/Envelopes/AutomationEnvelope"
      |> List.map (fun (_, envelope) -> AutomationEnvelope.create envelope)
    in
    { automation_envelopes = envelopes }
end

module Send = struct
  type t = {
    target : int;
    amount : float;
  } [@@deriving eq]
end

module Mixer = struct
  type t = {
    volume : float;
    pan : float;
    mute : bool;
    solo : bool;
  } [@@deriving eq]
end

type audio_track
type midi_track
type return_track
type master_track

type _ track =
  | AudioTrack : audio_track -> audio_track track
  | MIDITrack : midi_track -> midi_track track
  | ReturnTrack : return_track -> return_track track
  | MasterTrack : master_track -> master_track track
