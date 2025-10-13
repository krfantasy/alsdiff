open Alsdiff_lib_base

type track_ident = {
  id : int;
}

type audio_clip

module AudioTrack = struct
  type t = {
    ident : track_ident;
    name : string;
    clips : audio_clip list;
  }
end


module Send = struct
  type t = {
    target : int;
    amount : float;
  } [@@deriving eq]

  let equal s1 s2 = s1.target = s2.target && s1.amount = s2.amount

  let create (xml : Alsdiff_lib_base.Xml.t) : t =
    let amount =
      match Upath.find xml "/Manual@Value" with
      | Some (_, xml_elem) -> Alsdiff_lib_base.Xml.get_float_attr "Value" xml_elem
      | _ -> 0.0
    in
    (* As mentioned in TODO.org, the target track's ID is currently unknown *)
    { target = 0; amount }
end

module Mixer = struct
  type t = {
    volume : float;
    pan : float;
    mute : bool;
    solo : bool;
    sends : Send.t list;
  } [@@deriving eq]

  let create (xml : Alsdiff_lib_base.Xml.t) : t =
    let get_float_attr path attr_name =
      match Upath.find xml (path ^ "/Manual@" ^ attr_name) with
      | Some (_, xml_elem) -> Alsdiff_lib_base.Xml.get_float_attr "Value" xml_elem
      | _ -> failwith ("Cannot find " ^ path)
    in
    let get_bool_attr path attr_name =
      match Upath.find xml (path ^ "/Manual@" ^ attr_name) with
      | Some (_, xml_elem) ->
        Option.value ~default:false (Alsdiff_lib_base.Xml.get_bool_attr_opt "Value" xml_elem)
      | None -> failwith ("Cannot find " ^ path)
    in
    let volume = get_float_attr "/Volume" "Value" in
    let pan = get_float_attr "/Pan" "Value" in
    let mute = get_bool_attr "/On" "Value" in
    let solo =
      match Upath.find xml "/SoloSink@Value" with
      | Some (_, xml_elem) ->
        Option.value ~default:false (Alsdiff_lib_base.Xml.get_bool_attr_opt "Value" xml_elem)
      | None -> false
    in
    let sends =
      Upath.find_all xml "/Sends/TrackSendHolder/Send"
      |> List.map (fun (_, send_node) -> Send.create send_node)
    in
    { volume; pan; mute; solo; sends }
end


type _ t =
  | AudioTrack : AudioTrack.t -> AudioTrack.t t
