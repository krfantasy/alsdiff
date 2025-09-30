

type time_signature = { numerator : int; denominator : int }

type midi_note = {
  time : int;
  duration : int;
  velocity : int;
  off_velocity : int;
  note : int;
}

type loop_section = {
  start : int;
  end_ : int;
  on : bool;
}


type content =
  | MIDIContent of midi_note list
  | AudioContent of { file_path : string; sample_rate : int; bit_depth : int }

type clip = {
  id : int;
  start : float;
  duration : float;
  loop : loop_section option;
  signature : time_signature;
  content : content;
}

type envelope_event = {
  time : float;
  value : float;
}

type automation_envelope = {
  id : int;
  target : int;
  events : envelope_event list;
}

type automation = {
  automation_envelopes : automation_envelope list;
}

(** create [automation] from the given [xml] node,
    which is expected to be the root node of an [AutomationEnvelopes] element.
    @raise Failure if the given [xml] is not the expected format.
  *)
let create_automation (xml : Xml.t) : automation =
  let open Xml in
  let get_attr name attrs =
    try List.assoc name attrs
    with Not_found -> failwith ("Attribute " ^ name ^ " not found")
  in
  let parse_event = function
    | Element { attrs; _ } ->
        {
          time = float_of_string (get_attr "Time" attrs);
          value = float_of_string (get_attr "Value" attrs);
        }
    | Data _ -> failwith "Event must be an element"
  in
  let parse_envelope = function
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
