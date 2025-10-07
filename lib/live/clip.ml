open Alsdiff_lib_base

module TimeSignature = struct
  type t = { numer : int; denom : int } [@@deriving eq]
end

module MidiNote = struct
  type t = {
    time : int;
    duration : int;
    velocity : int;
    off_velocity : int;
    note : int;
  } [@@deriving eq]
end

module LoopSection = struct
  type t = {
    start : int;
    end_ : int;
    on : bool;
  } [@@deriving eq]
end

module Content = struct
  type t =
    | MIDIContent of MidiNote.t list
    | AudioContent of { file_path : string; sample_rate : int; bit_depth : int }
  [@@deriving eq]
end

module Clip = struct
  type t = {
    id : int;
    start : float;
    duration : float;
    loop : LoopSection.t option;
    signature : TimeSignature.t;
    content : Content.t;
  } [@@deriving eq]
end

module Send = struct
  type t = {
    target : int;
    amount : float;
  } [@@deriving eq]

  let create (xml : Xml.t) : t =
    let open Xml in
    let get_attr name attrs =
      try List.assoc name attrs
      with Not_found -> failwith ("Attribute " ^ name ^ " not found")
    in
    let amount =
      match Upath.find xml "/Manual@Value" with
      | Some (_, Element { attrs; _ }) -> float_of_string (get_attr "Value" attrs)
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

  let create (xml : Xml.t) : t =
    let open Xml in
    let get_attr name attrs =
      try List.assoc name attrs
      with Not_found -> failwith ("Attribute " ^ name ^ " not found")
    in
    let get_float_attr path attr_name =
      match Upath.find xml (path ^ "/Manual@" ^ attr_name) with
      | Some (_, Element { attrs; _ }) -> float_of_string (get_attr "Value" attrs)
      | _ -> failwith ("Cannot find " ^ path)
    in
    let get_bool_attr path attr_name =
      match Upath.find xml (path ^ "/Manual@" ^ attr_name) with
      | Some (_, Element { attrs; _ }) ->
          let value = get_attr "Value" attrs in
          value = "true"
      | _ -> failwith ("Cannot find " ^ path)
    in
    let volume = get_float_attr "/Volume" "Value" in
    let pan = get_float_attr "/Pan" "Value" in
    let mute = get_bool_attr "/On" "Value" in
    let solo =
      match Upath.find xml "/SoloSink@Value" with
      | Some (_, Element { attrs; _ }) ->
          let value = get_attr "Value" attrs in
          value = "true"
      | _ -> false
    in
    let sends =
      Upath.find_all xml "/Sends/TrackSendHolder/Send"
      |> List.map (fun (_, send_node) -> Send.create send_node)
    in
    { volume; pan; mute; solo; sends }
end
