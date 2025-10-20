open Alsdiff_base

module EnvelopeEvent = struct
  type t = {
    time : float;
    value : float;
  } [@@deriving eq]

  let create (xml : Alsdiff_base.Xml.t) : t =
    {
      time = Alsdiff_base.Xml.get_float_attr "Time" xml;
      value = Alsdiff_base.Xml.get_float_attr "Value" xml;
    }
end

module AutomationEnvelope = struct
  type t = {
    id : int;
    target : int;
    events : EnvelopeEvent.t list;
  } [@@deriving eq]

  let create (xml : Alsdiff_base.Xml.t) : t =
    let id = Alsdiff_base.Xml.get_int_attr "Id" xml in
    let target = Upath.get_int_attr "/EnvelopeTarget/PointeeId" "Value" xml in
    let events =
      xml
      |> Upath.find_all "/Automation/Events/FloatEvent"
      |> List.map (fun (_, event) -> EnvelopeEvent.create event)
    in
    { id; target; events }
end


type t = {
    automation_envelopes : AutomationEnvelope.t list;
  } [@@deriving eq]

let create (xml : Alsdiff_base.Xml.t) : t =
  let envelopes =
    xml
    |> Upath.find_all "/Envelopes/AutomationEnvelope"
    |> List.map (fun (_, envelope) -> AutomationEnvelope.create envelope)
  in
  { automation_envelopes = envelopes }
