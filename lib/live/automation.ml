open Alsdiff_base
open Alsdiff_base.Diff


module CurveControls = struct
  type t = {
    curve1_x : float;
    curve1_y : float;
    curve2_x : float;
    curve2_y : float;
  } [@@deriving eq, patch] [@@patch.generate_diff]
end


type event_value =
  | FloatEvent of float
  | IntEvent of int
  | EnumEvent of int
[@@deriving eq]

module EnvelopeEvent = struct
  type t = {
    id : int; [@id.id] [@patch.skip]
    time : float;
    value : event_value;
    curve : CurveControls.t option;
  } [@@deriving eq, id, patch] [@@patch.generate_diff]

  let create (xml : Xml.t) : t =
    let tag_name = Xml.get_name xml in
    let id = Xml.get_int_attr "Id" xml in
    let time = Xml.get_float_attr "Time" xml in
    let curve =
      match (Xml.get_float_attr_opt "CurveControl1X" xml,
             Xml.get_float_attr_opt "CurveControl1Y" xml,
             Xml.get_float_attr_opt "CurveControl2X" xml,
             Xml.get_float_attr_opt "CurveControl2Y" xml) with
      | (Some c1x, Some c1y, Some c2x, Some c2y) ->
        Some { CurveControls.curve1_x = c1x; curve1_y = c1y;
               curve2_x = c2x; curve2_y = c2y }
      | _ -> None
    in
    let value = match tag_name with
      | "FloatEvent" -> FloatEvent (Xml.get_float_attr "Value" xml)
      | "IntEvent" -> IntEvent (Xml.get_int_attr "Value" xml)
      | "EnumEvent" -> EnumEvent (Xml.get_int_attr "Value" xml)
      | _ -> raise (Xml.Xml_error (xml, "Unknown event type: " ^ tag_name))
    in
    { id; time; value; curve }

  let make_from_result (r : Upath2.match_result) : t =
    let id = Option.get (Upath2.get_int_attr r "Id") in
    let time = Option.get (Upath2.get_float_attr r "Time") in
    let curve =
      match (Upath2.get_float_attr r "CurveControl1X",
             Upath2.get_float_attr r "CurveControl1Y",
             Upath2.get_float_attr r "CurveControl2X",
             Upath2.get_float_attr r "CurveControl2Y") with
      | (Some c1x, Some c1y, Some c2x, Some c2y) ->
        Some { CurveControls.curve1_x = c1x; curve1_y = c1y;
               curve2_x = c2x; curve2_y = c2y }
      | _ -> None
    in
    let value = match r.Upath2.element_name with
      | "FloatEvent" -> FloatEvent (Option.get (Upath2.get_float_attr r "Value"))
      | "IntEvent" -> IntEvent (Option.get (Upath2.get_int_attr r "Value"))
      | "EnumEvent" -> EnumEvent (Option.get (Upath2.get_int_attr r "Value"))
      | name -> failwith ("Unknown event type: " ^ name)
    in
    { id; time; value; curve }
end


type t = {
  id : int; [@id.id] [@patch.identity]
  target : int; [@id.id] [@patch.identity]
  events : EnvelopeEvent.t list;
} [@@deriving eq, id, patch] [@@patch.generate_diff]

(* Automation contains a list of EnvelopeEvents and is therefore
   a structured type at a higher level of abstraction. *)

let queries = [
  Upath2.query_of_path "/EnvelopeTarget/PointeeId@Value";
  Upath2.query_of_path "/Automation/Events/'(Float|Int|Enum)Event'";
]

let make ~root_attrs results =
  let id = int_of_string (List.assoc "Id" root_attrs) in
  let target = Option.get (Upath2.query_int_attr results 0 "Value") in
  let events =
    Upath2.find_all_results results 1
    |> List.map EnvelopeEvent.make_from_result
    |> List.sort (fun a b -> Float.compare a.EnvelopeEvent.time b.EnvelopeEvent.time)
  in
  { id; target; events }

let create (xml : Xml.t) : t =
  let root_attrs = (match xml with Xml.Element { attrs; _ } -> attrs | _ -> []) in
  let stream = Upath2.stream_of_xml xml in
  let nfa = Upath2.compile queries in
  let results = Upath2.evaluate nfa stream in
  make ~root_attrs results
