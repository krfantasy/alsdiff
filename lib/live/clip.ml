open Alsdiff_base

module TimeSignature = struct
  type t = { numer : int; denom : int } [@@deriving eq]
  let create (xml : Xml.t) : t =
    match xml with
    | Xml.Element { name = "RemoteableTimeSignature"; _ } ->
      let numer = Upath.get_int_attr_opt "/Numerator" "Value" xml |> Option.value ~default:4 in
      let denom = Upath.get_int_attr_opt "/Denominator" "Value" xml |> Option.value ~default:4 in
      { numer; denom }
    | _ -> failwith "Invalid XML element for creating TimeSignature"
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
    start_time : float;
    end_time : float;
    on : bool;
  } [@@deriving eq]

  let create (xml : Xml.t) : t =
    match xml with
    | Xml.Element { name = "Loop"; _ } ->
      let start = Upath.get_float_attr_opt "/LoopStart" "Value" xml |> Option.value ~default:0.0 in
      let end_ = Upath.get_float_attr_opt "/LoopEnd" "Value" xml |> Option.value ~default:0.0 in
      let on = Upath.get_bool_attr_opt "/LoopOn" "Value" xml |> Option.value ~default:false in
      { start_time = start; end_time = end_; on; }
    | _ -> failwith "Invalid XML element for creating LoopSection"
end


module MidiClip = struct
  type t = {
    id : int;
    name : string;
    start_time : float;
    end_time : float;
    loop : LoopSection.t option;
    signature : TimeSignature.t;
    notes : MidiNote.t list;
  } [@@deriving eq]

  let create (xml : Xml.t) : t =
    match xml with
    | Xml.Element { name = "MidiClip"; _ } ->
      let id = Xml.get_int_attr "Id" xml in
      let name = Upath.get_attr_opt "/Name" "Value" xml |> Option.value ~default:"" in
      let start_time = Upath.get_float_attr_opt "/CurrentStart" "Value" xml |> Option.value ~default:0.0 in

      (* Extract end time from CurrentEnd *)
      let end_time = Upath.get_float_attr_opt "/CurrentEnd" "Value" xml |> Option.value ~default:start_time in

      (* Extract loop information *)
      let loop =
        try Upath.find_opt "/Loop" xml
          |> Option.map snd
          |> Option.map LoopSection.create with
        | _ -> None
      in

      (* Extract time signature *)
      let signature = Upath.find "/TimeSignature/TimeSignatures/RemoteableTimeSignature" xml |> snd |> TimeSignature.create in

      (* Extract MIDI notes from KeyTracks *)
      let notes =
        match Upath.find_opt "/Notes/KeyTracks" xml with
        | Some (_, key_tracks_elem) ->
          (match key_tracks_elem with
           | Xml.Element { childs = key_track_elements; _ } ->
             List.fold_left (fun acc key_track ->
               match key_track with
               | Xml.Element { name = "KeyTrack"; _ } ->
                 (* Get notes from this KeyTrack *)
                 let track_notes =
                   match Upath.find_opt "/Notes" key_track with
                   | Some (_, notes_elem) ->
                     (match notes_elem with
                      | Xml.Element { childs = notes_childs; _ } ->
                        List.fold_left (fun note_acc note ->
                          match note with
                          | Xml.Element { name = "MidiNoteEvent"; attrs = _; childs = _ } ->
                            let time = Xml.get_float_attr "Time" note |> int_of_float in
                            let duration = Xml.get_float_attr "Duration" note |> int_of_float in
                            let velocity = Xml.get_int_attr "Velocity" note in
                            let off_velocity = Xml.get_int_attr "OffVelocity" note in
                            let note_id = Xml.get_int_attr "NoteId" note in
                            { MidiNote.time; duration; velocity; off_velocity; note = note_id } :: note_acc
                          | _ -> note_acc
                        ) acc notes_childs
                      | _ -> acc)
                   | _ -> acc
                 in
                 track_notes
               | _ -> acc
             ) [] key_track_elements
           | _ -> [])
        | _ -> []
      in

      { id; name; start_time; end_time; loop; signature; notes }
    | _ -> failwith "Expected MidiClip element"
end


module SampleRef = struct
  type t = {
    file_path : string;
    crc : string;
    last_modified_date : int64; (* unix timestamp *)
  } [@@derving eq]

  let create (xml : Xml.t) : t =
    match xml with
    | Xml.Element { name = "SampleRef"; _ } ->
      let last_modified_date = Upath.get_int64_attr_opt "LastModDate" "Value" xml |> Option.value ~default:0L in
      let file_path = Upath.get_attr_opt "FileRef/Path" "Value" xml |> Option.value ~default:"" in
      let crc = Upath.get_attr_opt "FileRef/OriginalCrc" "Value" xml |> Option.value ~default:"" in
      { file_path; crc; last_modified_date }
    | _ -> failwith "Invalid XML element for creating SampleRef"
end


module AudioClip = struct
  (* TODO:
     1. support warp related settings
     2. fades support
  *)
  type t = {
    id : int;
    name : string;
    start_time : float;
    end_time : float;
    loop : LoopSection.t option;
    signature : TimeSignature.t;
    sample_ref : SampleRef.t;
  }

  let create (xml : Xml.t) : t =
    match xml with
    | Xml.Element { name = "AudioClip"; _ } ->
      let id = Xml.get_int_attr "Id" xml in
      let name = Upath.get_attr_opt "/AudioClip/Name" "Value" xml |> Option.value ~default:"" in
      let start_time = Upath.get_float_attr_opt "/CurrentStart" "Value" xml |> Option.value ~default:0.0 in
      let end_time = Upath.get_float_attr_opt "/CurrentEnd" "Value" xml |> Option.value ~default:start_time in

      (* Extract loop information *)
      (* TODO: what the fuck does `StartRelative` means in the `Loop` element *)
      let loop =
        try Upath.find_opt "/Loop" xml
          |> Option.map snd
          |> Option.map LoopSection.create with
        | _ -> None
      in
      (* Extract time signature *)
      (* TODO: support time signature automation *)
      let signature = Upath.find "/TimeSignature/TimeSignatures/RemoteableTimeSignature" xml |> snd |> TimeSignature.create in
      (* Extract sample reference *)
      let sample_ref = Upath.find "/SampleRef" xml |> snd |> SampleRef.create in
      { id; name; start_time; end_time; loop; signature; sample_ref }
    | _ -> failwith "Expected AudioClip element"
end


type t =
  | AudioClip of AudioClip.t
  | MidiClip of MidiClip.t


let create xml : t =
  match xml with
  | Xml.Element { name; _ } when name = "AudioClip" ->
    AudioClip (AudioClip.create xml)
  | Xml.Element { name; _ } when name = "MidiClip" ->
    MidiClip (MidiClip.create xml)
  | _ -> raise (Invalid_argument "Invalid XML")
