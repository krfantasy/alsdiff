open Alsdiff_base

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
    start_time : float;
    end_time : float;
    on : bool;
  } [@@deriving eq]
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
    | Xml.Element { name = "MidiClip"; parent = _; _ } ->
      let id = Xml.get_int_attr "Id" xml in
      let name =
        match Upath.find "/Name@Value" xml with
        | Some (_, xml_elem) -> Xml.get_attr "Value" xml_elem
        | _ -> ""
      in
      let start_time = Xml.get_float_attr "Time" xml in

      (* Extract end time from CurrentEnd *)
      let end_time =
        match Upath.find "/CurrentEnd@Value" xml with
        | Some (_, xml_elem) -> Xml.get_float_attr "Value" xml_elem
        | _ -> start_time  (* fallback to start_time if no end time found *)
      in

      (* Extract loop information *)
      let loop =
        match Upath.find "/Loop" xml with
        | Some (_, _) ->
          let start =
            match Upath.find "/Loop/LoopStart@Value" xml with
            | Some (_, xml_elem) -> Xml.get_float_attr "Value" xml_elem
            | _ -> 0.0
          in
          let end_ =
            match Upath.find "/Loop/LoopEnd@Value" xml with
            | Some (_, xml_elem) -> Xml.get_float_attr "Value" xml_elem
            | _ -> 0.0
          in
          let on =
            match Upath.find "/Loop/LoopOn@Value" xml with
            | Some (_, xml_elem) ->
              Option.value ~default:false (Xml.get_bool_attr_opt "Value" xml_elem)
            | None -> false
          in
          Some { LoopSection.start_time = start; end_time = end_; on }
        | _ -> None
      in

      (* Extract time signature *)
      let signature =
        match Upath.find "/TimeSignature/TimeSignatures/RemoteableTimeSignature" xml with
        | Some (_, _) ->
          let numer =
            match Upath.find "/TimeSignature/TimeSignatures/RemoteableTimeSignature/Numerator@Value" xml with
            | Some (_, xml_elem) -> Xml.get_int_attr "Value" xml_elem
            | _ -> 4
          in
          let denom =
            match Upath.find "/TimeSignature/TimeSignatures/RemoteableTimeSignature/Denominator@Value" xml with
            | Some (_, xml_elem) -> Xml.get_int_attr "Value" xml_elem
            | _ -> 4
          in
          { TimeSignature.numer; denom }
        | _ -> { TimeSignature.numer = 4; denom = 4 }  (* default time signature *)
      in

      (* Extract MIDI notes from KeyTracks *)
      let notes =
        match Upath.find "/Notes/KeyTracks" xml with
        | Some (_, key_tracks_elem) ->
          (match key_tracks_elem with
           | Xml.Element { childs = key_track_elements; parent = _; _ } ->
             List.fold_left (fun acc key_track ->
               match key_track with
               | Xml.Element { name = "KeyTrack"; parent = _; _ } ->
                 (* Get notes from this KeyTrack *)
                 let track_notes =
                   match Upath.find "/Notes" key_track with
                   | Some (_, notes_elem) ->
                     (match notes_elem with
                      | Xml.Element { childs = notes_childs; parent = _; _ } ->
                        List.fold_left (fun note_acc note ->
                          match note with
                          | Xml.Element { name = "MidiNoteEvent"; attrs = _; childs = _; parent = _ } ->
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
  }
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
    | Xml.Element { name = "AudioClip"; parent = _; _ } ->
      let id = Xml.get_int_attr "Id" xml in
      let name =
        match Upath.find "/Name@Value" xml with
        | Some (_, xml_elem) -> Xml.get_attr "Value" xml_elem
        | _ -> ""
      in
      let start_time = Xml.get_float_attr "Time" xml in

      (* Extract end time from CurrentEnd *)
      let end_time =
        match Upath.find "/CurrentEnd@Value" xml with
        | Some (_, xml_elem) -> Xml.get_float_attr "Value" xml_elem
        | _ -> start_time  (* fallback to start_time if no end time found *)
      in

      (* Extract loop information *)
      (* TODO: what the fuck does `StartRelative` means in the `Loop` element *)
      let loop =
        match Upath.find "/Loop" xml with
        | Some (_, _) ->
          let start =
            match Upath.find "/Loop/LoopStart@Value" xml with
            | Some (_, xml_elem) -> Xml.get_float_attr "Value" xml_elem
            | _ -> 0.0
          in
          let end_ =
            match Upath.find "/Loop/LoopEnd@Value" xml with
            | Some (_, xml_elem) -> Xml.get_float_attr "Value" xml_elem
            | _ -> 0.0
          in
          let on =
            match Upath.find "/Loop/LoopOn@Value" xml with
            | Some (_, xml_elem) ->
              Option.value ~default:false (Xml.get_bool_attr_opt "Value" xml_elem)
            | None -> false
          in
          Some { LoopSection.start_time = start; end_time = end_; on }
        | _ -> None
      in

      (* Extract time signature *)
      (* TODO: support time signature automation *)
      let signature =
        match Upath.find "/TimeSignature/TimeSignatures/RemoteableTimeSignature" xml with
        | Some (_, _) ->
          let numer =
            match Upath.find "/TimeSignature/TimeSignatures/RemoteableTimeSignature/Numerator@Value" xml with
            | Some (_, xml_elem) -> Xml.get_int_attr "Value" xml_elem
            | _ -> 4
          in
          let denom =
            match Upath.find "/TimeSignature/TimeSignatures/RemoteableTimeSignature/Denominator@Value" xml with
            | Some (_, xml_elem) -> Xml.get_int_attr "Value" xml_elem
            | _ -> 4
          in
          { TimeSignature.numer; denom }
        | _ -> { TimeSignature.numer = 4; denom = 4 }  (* default time signature *)
      in

      (* Extract sample reference *)
      let last_modified_date =
        match Upath.find "/SampleRef/LastModDate@Value" xml with
        | Some (_, xml_elem) -> Xml.get_attr "Value" xml_elem |> Int64.of_string
        | _ -> 0L
      in
      let file_path =
        match Upath.find "/SampleRef/FileRef/Path@Value" xml with
        | Some (_, xml_elem) -> Xml.get_attr "Value" xml_elem
        | _ -> ""
      in
      let crc =
        match Upath.find "/SampleRef/FileRef/OriginalCrc@Value" xml with
        | Some (_, xml_elem) -> Xml.get_attr "Value" xml_elem
        | _ -> "0"
      in
      let sample_ref = { SampleRef.file_path; crc; last_modified_date } in

      { id; name; start_time; end_time; loop; signature; sample_ref }
    | _ -> failwith "Expected AudioClip element"
end


type t =
  | AudioClip of AudioClip.t
  | MidiClip of MidiClip.t


let create xml : t =
  match xml with
  | Xml.Element { name; parent = _; _ } when name = "AudioClip" ->
    AudioClip (AudioClip.create xml)
  | Xml.Element { name; parent = _; _ } when name = "MidiClip" ->
    MidiClip (MidiClip.create xml)
  | _ -> raise (Invalid_argument "Invalid XML")
