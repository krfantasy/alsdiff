open Alsdiff_lib_base

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
