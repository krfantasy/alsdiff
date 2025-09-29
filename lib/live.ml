

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
  let find_child_element name (node : t) : t option =
    match node with
    | Element { childs; _ } ->
        List.find_opt (function
          | Element { name = child_name; _ } -> child_name = name
          | Data _ -> false
        ) childs
    | Data _ -> None
  in
  let filter_child_elements name (node : t) : t list =
    match node with
    | Element { childs; _ } ->
        List.filter (function
          | Element { name = child_name; _ } -> child_name = name
          | Data _ -> false
        ) childs
    | Data _ -> []
  in
  let get_element_path path start_node =
    (* For now, we'll use a simple approach for common paths *)
    let components = String.split_on_char '/' path in
    let rec find_element node path_components =
      match path_components with
      | [] -> Some node
      | component :: rest ->
          match node with
          | Element { childs; _ } ->
              let found = List.find_opt (function
                | Element { name; _ } -> name = component
                | Data _ -> false
              ) childs in
              (match found with
              | Some child_node -> find_element child_node rest
              | None -> None)
          | Data _ -> None
    in
    find_element start_node components
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
          match get_element_path "EnvelopeTarget/PointeeId" envelope_node with
          | Some (Element { attrs = p_attrs; _ }) ->
              int_of_string (get_attr "Value" p_attrs)
          | _ -> failwith "Cannot find target"
        in
        let events =
          match get_element_path "Automation/Events" envelope_node with
          | Some events_node ->
              filter_child_elements "FloatEvent" events_node |> List.map parse_event
          | None -> failwith "Cannot find events"
        in
        { id; target; events }
    | Data _ -> failwith "Envelope must be an element"
  in
  let envelopes =
    match find_child_element "Envelopes" xml with
    | Some envelopes_node ->
        filter_child_elements "AutomationEnvelope" envelopes_node
    | None -> failwith "Cannot find Envelopes"
  in
  { automation_envelopes = List.map parse_envelope envelopes }
