type param = {
  id: int;
  name : string;
  value : float;
} [@@deriving eq]


type t = {
  id : int;
  device_name : string;
  preset_name : string;
  params : param list;
} [@@deriving eq]

let has_same_ident a b =
  a.id = b.id && a.device_name = b.device_name && a.preset_name = b.preset_name
