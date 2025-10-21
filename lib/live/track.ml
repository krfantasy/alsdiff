open Alsdiff_base

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

  let create (xml : Xml.t) : t =
    let amount = Upath.get_float_attr "Manual" "Value" xml in
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

  let create (xml : Xml.t) : t =
    let volume = Upath.get_float_attr "/Volume/Manual" "Value" xml in
    let pan = Upath.get_float_attr "/Pan/Manual" "Value" xml in
    let mute = Upath.get_bool_attr "/On/Manual" "Value" xml in
    let solo = Upath.get_bool_attr "/SoloSink" "Value" xml in
    let sends =
      xml
      |> Upath.find_all "/Sends/TrackSendHolder/Send"
      |> List.map (fun (_, send_node) -> Send.create send_node)
    in
    { volume; pan; mute; solo; sends }
end

type _ t =
  | AudioTrack : AudioTrack.t -> AudioTrack.t t
