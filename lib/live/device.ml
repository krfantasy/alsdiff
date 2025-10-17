
module Param = struct
  type t = {
    name : string;
    value : float;
    automation : int;
  } [@@deriving eq]

end

type t = {
  id : int;
  device_name : string;
  preset_name : string;
  params : Param.t list;
} [@@deriving eq]
