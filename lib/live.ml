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
end

module AutomationEnvelope = struct
  type t = {
    id : int;
    target : int;
    events : EnvelopeEvent.t list;
  } [@@deriving eq]
end

module Automation = struct
  type t = {
    automation_envelopes : AutomationEnvelope.t list;
  } [@@deriving eq]
end


(** create [automation] from the given [xml] node,
    which is expected to be the root node of an [AutomationEnvelopes] element.
    @raise Failure if the given [xml] is not the expected format.
  *)
let create_automation (xml : Xml.t) : Automation.t =
  let open Xml in
  let get_attr name attrs =
    try List.assoc name attrs
    with Not_found -> failwith ("Attribute " ^ name ^ " not found")
  in
  let parse_event : Xml.t -> EnvelopeEvent.t = function
    | Element { attrs; _ } ->
        {
          time = float_of_string (get_attr "Time" attrs);
          value = float_of_string (get_attr "Value" attrs);
        }
    | Data _ -> failwith "Event must be an element"
  in
  let parse_envelope : Xml.t -> AutomationEnvelope.t = function
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
          |> List.map (fun (_, event) -> parse_event event)
        in
        { id; target; events }
    | Data _ -> failwith "Envelope must be an element"
  in
  let envelopes =
    Upath.find_all xml "/Envelopes/AutomationEnvelope"
    |> List.map (fun (_, envelope) -> parse_envelope envelope)
  in
  { automation_envelopes = envelopes }

type audio_track
type midi_track
type return_track
type master_track

type _ track =
  | AudioTrack : audio_track -> audio_track track
  | MIDITrack : midi_track -> midi_track track
  | ReturnTrack : return_track -> return_track track
  | MasterTrack : master_track -> master_track track
