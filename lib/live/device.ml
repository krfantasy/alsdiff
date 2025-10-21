open Alsdiff_base

module Param = struct
  type t = {
    name : string;
    value : float;
    automation : int;
  } [@@deriving eq]

  let create (xml : Xml.t) : t =
    match xml with
    | Xml.Element { name; _ } ->
      let value = Upath.get_float_attr "/Manual" "Value" xml in
      let automation = Upath.get_int_attr "/AutomationTarget" "Id" xml in
      { name; value; automation }
    | _ -> failwith "Invalid XML element for creating Param"
end

type t = {
  id : int;
  device_name : string;
  preset_name : string;
  pointee : int;
  params : Param.t list;
} [@@deriving eq]
