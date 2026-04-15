open Alsdiff_base
open Alsdiff_base.Diff


module TimeSignature = struct
  type t = { numer : int; denom : int } [@@deriving eq, patch] [@@patch.generate_diff]

  let queries = [
    Upath2.query_of_path ~qid:0 ~path_str:"/Numerator" ~attr:(Some "Value");
    Upath2.query_of_path ~qid:1 ~path_str:"/Denominator" ~attr:(Some "Value");
  ]

  let make ~root_attrs:_ results =
    let numer = Option.get (Upath2.query_int_attr results 0 "Value") in
    let denom = Option.get (Upath2.query_int_attr results 1 "Value") in
    { numer; denom }

  let create (xml : Xml.t) : t =
    let root_attrs = (match xml with Xml.Element { attrs; _ } -> attrs | _ -> []) in
    let stream = Upath2.stream_of_xml xml in
    let nfa = Upath2.compile queries in
    let results = Upath2.evaluate nfa stream in
    make ~root_attrs results
end


module MidiNote = struct
  type t = {
    id : int; [@id.id] [@patch.skip]
    note : int;
    time : float;
    duration : float;
    velocity : float;
    off_velocity : float;
  } [@@deriving eq, id, patch] [@@patch.generate_diff]

  let make_from_result (note : int) (r : Upath2.match_result) : t =
    let id = Option.get (Upath2.get_int_attr r "NoteId") in
    let time = Option.get (Upath2.get_float_attr r "Time") in
    let duration = Option.get (Upath2.get_float_attr r "Duration") in
    let velocity = Option.get (Upath2.get_float_attr r "Velocity") in
    let off_velocity = Option.get (Upath2.get_float_attr r "OffVelocity") in
    { id; note; time; duration; velocity; off_velocity }

  let create (note: int) (xml : Xml.t) : t =
    match xml with
    | Xml.Element { name = "MidiNoteEvent"; attrs; _ } ->
      let id = int_of_string (List.assoc "NoteId" attrs) in
      let time = float_of_string (List.assoc "Time" attrs) in
      let duration = float_of_string (List.assoc "Duration" attrs) in
      let velocity = float_of_string (List.assoc "Velocity" attrs) in
      let off_velocity = float_of_string (List.assoc "OffVelocity" attrs) in
      { id; note; time; duration; velocity; off_velocity }
    | _ -> raise (Xml.Xml_error (xml, "Invalid XML element for creating MidiNote"))
end

module Loop = struct
  type t = {
    start_time : float;
    end_time : float;
    on : bool;
  } [@@deriving eq, patch] [@@patch.generate_diff]

  let queries = [
    Upath2.query_of_path ~qid:0 ~path_str:"/LoopStart" ~attr:(Some "Value");
    Upath2.query_of_path ~qid:1 ~path_str:"/LoopEnd" ~attr:(Some "Value");
    Upath2.query_of_path ~qid:2 ~path_str:"/LoopOn" ~attr:(Some "Value");
  ]

  let make ~root_attrs:_ results =
    let start_time = Option.get (Upath2.query_float_attr results 0 "Value") in
    let end_time = Option.get (Upath2.query_float_attr results 1 "Value") in
    let on = Option.get (Upath2.query_bool_attr results 2 "Value") in
    { start_time; end_time; on }

  let create (xml : Xml.t) : t =
    let root_attrs = (match xml with Xml.Element { attrs; _ } -> attrs | _ -> []) in
    let stream = Upath2.stream_of_xml xml in
    let nfa = Upath2.compile queries in
    let results = Upath2.evaluate nfa stream in
    make ~root_attrs results
end


module MidiClip = struct
  type t = {
    id : int; [@id.id] [@patch.identity]
    name : string;
    start_time : float;
    end_time : float;
    loop : Loop.t;
    signature : TimeSignature.t;
    notes : MidiNote.t list;
  } [@@deriving eq, id, patch] [@@patch.generate_diff]

  let queries = [
    Upath2.query_of_path ~qid:0 ~path_str:"/Name" ~attr:(Some "Value");
    Upath2.query_of_path ~qid:1 ~path_str:"/CurrentStart" ~attr:(Some "Value");
    Upath2.query_of_path ~qid:2 ~path_str:"/CurrentEnd" ~attr:(Some "Value");
    (* Loop children *)
    Upath2.query_of_path ~qid:3 ~path_str:"/Loop/LoopStart" ~attr:(Some "Value");
    Upath2.query_of_path ~qid:4 ~path_str:"/Loop/LoopEnd" ~attr:(Some "Value");
    Upath2.query_of_path ~qid:5 ~path_str:"/Loop/LoopOn" ~attr:(Some "Value");
    (* TimeSignature *)
    Upath2.query_of_path ~qid:6 ~path_str:"/TimeSignature/TimeSignatures/RemoteableTimeSignature/Numerator" ~attr:(Some "Value");
    Upath2.query_of_path ~qid:7 ~path_str:"/TimeSignature/TimeSignatures/RemoteableTimeSignature/Denominator" ~attr:(Some "Value");
    (* KeyTracks + MidiKey + MidiNoteEvents *)
    Upath2.query_of_path ~qid:8 ~path_str:"/Notes/KeyTracks/KeyTrack/MidiKey" ~attr:(Some "Value");
    Upath2.query_of_path ~qid:9 ~path_str:"/Notes/KeyTracks/KeyTrack/Notes/MidiNoteEvent" ~attr:None;
  ]

  (** Group MidiNoteEvent results by their parent KeyTrack.
      In the XML, MidiNoteEvents appear BEFORE their sibling MidiKey element:
        <KeyTrack><Notes><MidiNoteEvent/>...</Notes><MidiKey Value="60"/></KeyTrack>
      So in document-order results: notes for key N, then MidiKey for key N.
      We buffer MidiNoteEvents and assign them to the next MidiKey we see. *)
  let group_notes_by_key (all_results : Upath2.match_result list) : MidiNote.t list =
    let rec walk results buf acc =
      match results with
      | [] -> List.rev acc
      | r :: rest ->
        if r.Upath2.query_id = 9 then
          walk rest (r :: buf) acc
        else if r.Upath2.query_id = 8 then begin
          let midi_key = Option.get (Upath2.get_int_attr r "Value") in
          let notes = List.map (MidiNote.make_from_result midi_key) (List.rev buf) in
          walk rest [] (List.rev_append notes acc)
        end else
          walk rest buf acc
    in
    walk all_results [] []

  let make ~root_attrs results =
    let id = int_of_string (List.assoc "Id" root_attrs) in
    let name = Option.get (Upath2.query_attr results 0 "Value") in
    let start_time = Option.get (Upath2.query_float_attr results 1 "Value") in
    let end_time = Option.get (Upath2.query_float_attr results 2 "Value") in
    let loop = {
      Loop.start_time = Option.get (Upath2.query_float_attr results 3 "Value");
      end_time = Option.get (Upath2.query_float_attr results 4 "Value");
      on = Option.get (Upath2.query_bool_attr results 5 "Value");
    } in
    let signature = {
      TimeSignature.numer = Option.get (Upath2.query_int_attr results 6 "Value");
      denom = Option.get (Upath2.query_int_attr results 7 "Value");
    } in
    let notes = group_notes_by_key results in
    { id; name; start_time; end_time; loop; signature; notes }

  let create (xml : Xml.t) : t =
    let root_attrs = (match xml with Xml.Element { attrs; _ } -> attrs | _ -> []) in
    let stream = Upath2.stream_of_xml xml in
    let nfa = Upath2.compile queries in
    let results = Upath2.evaluate nfa stream in
    make ~root_attrs results
end


module SampleRef = struct
  type t = {
    file_path : string;
    crc : string;
    last_modified_date : int; (* unix timestamp *)
  } [@@deriving eq, patch] [@@patch.generate_diff]

  let queries = [
    Upath2.query_of_path ~qid:0 ~path_str:"/FileRef/Path" ~attr:(Some "Value");
    Upath2.query_of_path ~qid:1 ~path_str:"/FileRef/OriginalCrc" ~attr:(Some "Value");
    Upath2.query_of_path ~qid:2 ~path_str:"/LastModDate" ~attr:(Some "Value");
  ]

  let make ~root_attrs:_ results =
    let file_path = Option.get (Upath2.query_attr results 0 "Value") in
    let crc = Option.get (Upath2.query_attr results 1 "Value") in
    let last_modified_date = Option.get (Upath2.query_int_attr results 2 "Value") in
    { file_path; crc; last_modified_date }

  let create (xml : Xml.t) : t =
    let root_attrs = (match xml with Xml.Element { attrs; _ } -> attrs | _ -> []) in
    let stream = Upath2.stream_of_xml xml in
    let nfa = Upath2.compile queries in
    let results = Upath2.evaluate nfa stream in
    make ~root_attrs results
end


module Fade = struct
  type t = {
    fade_in_length : float;
    fade_out_length : float;
    is_initialized : bool;
    crossfade_state : int;
    fade_in_curve_skew : float;
    fade_in_curve_slope : float;
    fade_out_curve_skew : float;
    fade_out_curve_slope : float;
    is_default_fade_in : bool;
    is_default_fade_out : bool;
  } [@@deriving eq, patch] [@@patch.generate_diff]

  let queries = [
    Upath2.query_of_path ~qid:0 ~path_str:"/FadeInLength" ~attr:(Some "Value");
    Upath2.query_of_path ~qid:1 ~path_str:"/FadeOutLength" ~attr:(Some "Value");
    Upath2.query_of_path ~qid:2 ~path_str:"/ClipFadesAreInitialized" ~attr:(Some "Value");
    Upath2.query_of_path ~qid:3 ~path_str:"/CrossfadeInState" ~attr:(Some "Value");
    Upath2.query_of_path ~qid:4 ~path_str:"/FadeInCurveSkew" ~attr:(Some "Value");
    Upath2.query_of_path ~qid:5 ~path_str:"/FadeInCurveSlope" ~attr:(Some "Value");
    Upath2.query_of_path ~qid:6 ~path_str:"/FadeOutCurveSkew" ~attr:(Some "Value");
    Upath2.query_of_path ~qid:7 ~path_str:"/FadeOutCurveSlope" ~attr:(Some "Value");
    Upath2.query_of_path ~qid:8 ~path_str:"/IsDefaultFadeIn" ~attr:(Some "Value");
    Upath2.query_of_path ~qid:9 ~path_str:"/IsDefaultFadeOut" ~attr:(Some "Value");
  ]

  let make ~root_attrs:_ results =
    let fade_in_length = Option.get (Upath2.query_float_attr results 0 "Value") in
    let fade_out_length = Option.get (Upath2.query_float_attr results 1 "Value") in
    let is_initialized = Option.get (Upath2.query_bool_attr results 2 "Value") in
    let crossfade_state = Option.get (Upath2.query_int_attr results 3 "Value") in
    let fade_in_curve_skew = Option.get (Upath2.query_float_attr results 4 "Value") in
    let fade_in_curve_slope = Option.get (Upath2.query_float_attr results 5 "Value") in
    let fade_out_curve_skew = Option.get (Upath2.query_float_attr results 6 "Value") in
    let fade_out_curve_slope = Option.get (Upath2.query_float_attr results 7 "Value") in
    let is_default_fade_in = Option.get (Upath2.query_bool_attr results 8 "Value") in
    let is_default_fade_out = Option.get (Upath2.query_bool_attr results 9 "Value") in
    {
      fade_in_length; fade_out_length; is_initialized;
      crossfade_state; fade_in_curve_skew; fade_in_curve_slope;
      fade_out_curve_skew; fade_out_curve_slope;
      is_default_fade_in; is_default_fade_out;
    }

  let create (xml : Xml.t) : t =
    let root_attrs = (match xml with Xml.Element { attrs; _ } -> attrs | _ -> []) in
    let stream = Upath2.stream_of_xml xml in
    let nfa = Upath2.compile queries in
    let results = Upath2.evaluate nfa stream in
    make ~root_attrs results
end


module AudioClip = struct
  (* TODO: support warp related settings *)
  type t = {
    id : int; [@id.id] [@patch.identity]
    name : string;
    start_time : float;
    end_time : float;
    loop : Loop.t;
    signature : TimeSignature.t;
    sample_ref : SampleRef.t;
    fade : Fade.t option;
  } [@@deriving eq, id, patch]

  let queries = [
    Upath2.query_of_path ~qid:0 ~path_str:"/Name" ~attr:(Some "Value");
    Upath2.query_of_path ~qid:1 ~path_str:"/CurrentStart" ~attr:(Some "Value");
    Upath2.query_of_path ~qid:2 ~path_str:"/CurrentEnd" ~attr:(Some "Value");
    (* Loop children *)
    Upath2.query_of_path ~qid:3 ~path_str:"/Loop/LoopStart" ~attr:(Some "Value");
    Upath2.query_of_path ~qid:4 ~path_str:"/Loop/LoopEnd" ~attr:(Some "Value");
    Upath2.query_of_path ~qid:5 ~path_str:"/Loop/LoopOn" ~attr:(Some "Value");
    (* TimeSignature *)
    Upath2.query_of_path ~qid:6 ~path_str:"/TimeSignature/TimeSignatures/RemoteableTimeSignature/Numerator" ~attr:(Some "Value");
    Upath2.query_of_path ~qid:7 ~path_str:"/TimeSignature/TimeSignatures/RemoteableTimeSignature/Denominator" ~attr:(Some "Value");
    (* SampleRef *)
    Upath2.query_of_path ~qid:8 ~path_str:"/SampleRef/FileRef/Path" ~attr:(Some "Value");
    Upath2.query_of_path ~qid:9 ~path_str:"/SampleRef/FileRef/OriginalCrc" ~attr:(Some "Value");
    Upath2.query_of_path ~qid:10 ~path_str:"/SampleRef/LastModDate" ~attr:(Some "Value");
    (* Fade enable + Fades children *)
    Upath2.query_of_path ~qid:11 ~path_str:"/Fade" ~attr:(Some "Value");
    Upath2.query_of_path ~qid:12 ~path_str:"/Fades/FadeInLength" ~attr:(Some "Value");
    Upath2.query_of_path ~qid:13 ~path_str:"/Fades/FadeOutLength" ~attr:(Some "Value");
    Upath2.query_of_path ~qid:14 ~path_str:"/Fades/ClipFadesAreInitialized" ~attr:(Some "Value");
    Upath2.query_of_path ~qid:15 ~path_str:"/Fades/CrossfadeInState" ~attr:(Some "Value");
    Upath2.query_of_path ~qid:16 ~path_str:"/Fades/FadeInCurveSkew" ~attr:(Some "Value");
    Upath2.query_of_path ~qid:17 ~path_str:"/Fades/FadeInCurveSlope" ~attr:(Some "Value");
    Upath2.query_of_path ~qid:18 ~path_str:"/Fades/FadeOutCurveSkew" ~attr:(Some "Value");
    Upath2.query_of_path ~qid:19 ~path_str:"/Fades/FadeOutCurveSlope" ~attr:(Some "Value");
    Upath2.query_of_path ~qid:20 ~path_str:"/Fades/IsDefaultFadeIn" ~attr:(Some "Value");
    Upath2.query_of_path ~qid:21 ~path_str:"/Fades/IsDefaultFadeOut" ~attr:(Some "Value");
  ]

  let make ~root_attrs results =
    let id = int_of_string (List.assoc "Id" root_attrs) in
    let name = Option.get (Upath2.query_attr results 0 "Value") in
    let start_time = Option.get (Upath2.query_float_attr results 1 "Value") in
    let end_time = Option.get (Upath2.query_float_attr results 2 "Value") in
    let loop = {
      Loop.start_time = Option.get (Upath2.query_float_attr results 3 "Value");
      end_time = Option.get (Upath2.query_float_attr results 4 "Value");
      on = Option.get (Upath2.query_bool_attr results 5 "Value");
    } in
    let signature = {
      TimeSignature.numer = Option.get (Upath2.query_int_attr results 6 "Value");
      denom = Option.get (Upath2.query_int_attr results 7 "Value");
    } in
    let sample_ref = {
      SampleRef.file_path = Option.get (Upath2.query_attr results 8 "Value");
      crc = Option.get (Upath2.query_attr results 9 "Value");
      last_modified_date = Option.get (Upath2.query_int_attr results 10 "Value");
    } in
    let fade =
      let fade_enabled = Option.get (Upath2.query_bool_attr results 11 "Value") in
      if fade_enabled then
        Some {
          Fade.fade_in_length = Option.get (Upath2.query_float_attr results 12 "Value");
          fade_out_length = Option.get (Upath2.query_float_attr results 13 "Value");
          is_initialized = Option.get (Upath2.query_bool_attr results 14 "Value");
          crossfade_state = Option.get (Upath2.query_int_attr results 15 "Value");
          fade_in_curve_skew = Option.get (Upath2.query_float_attr results 16 "Value");
          fade_in_curve_slope = Option.get (Upath2.query_float_attr results 17 "Value");
          fade_out_curve_skew = Option.get (Upath2.query_float_attr results 18 "Value");
          fade_out_curve_slope = Option.get (Upath2.query_float_attr results 19 "Value");
          is_default_fade_in = Option.get (Upath2.query_bool_attr results 20 "Value");
          is_default_fade_out = Option.get (Upath2.query_bool_attr results 21 "Value");
        }
      else
        None
    in
    { id; name; start_time; end_time; loop; signature; sample_ref; fade }

  let create (xml : Xml.t) : t =
    let root_attrs = (match xml with Xml.Element { attrs; _ } -> attrs | _ -> []) in
    let stream = Upath2.stream_of_xml xml in
    let nfa = Upath2.compile queries in
    let results = Upath2.evaluate nfa stream in
    make ~root_attrs results

  let diff (old_clip : t) (new_clip : t) : Patch.t =
    let { id = old_id; name = old_name; start_time = old_start; end_time = old_end; loop = old_loop; signature = old_sig; sample_ref = old_sample; fade = old_fade } = old_clip in
    let { id = new_id; name = new_name; start_time = new_start; end_time = new_end; loop = new_loop; signature = new_sig; sample_ref = new_sample; fade = new_fade } = new_clip in

    (* Only compare clips with the same id *)
    if old_id <> new_id then
      failwith "cannot diff two clips with different Id"
    else
      let name_change = diff_atomic_value (module String) old_name new_name in
      let start_time_change = diff_atomic_value (module Float) old_start new_start in
      let end_time_change = diff_atomic_value (module Float) old_end new_end in
      let loop_change = diff_complex_value (module Loop) old_loop new_loop in
      let signature_change = diff_complex_value (module TimeSignature) old_sig new_sig in
      let sample_ref_change = diff_complex_value (module SampleRef) old_sample new_sample in

      (* Handle fade diffing - both are option types *)
      let fade_change = match old_fade, new_fade with
        | None, None -> `Unchanged
        | None, Some new_fade -> `Added new_fade
        | Some old_fade, None -> `Removed old_fade
        | Some old_fade, Some new_fade ->
          let patch = Fade.diff old_fade new_fade in
          if Fade.Patch.is_empty patch then `Unchanged else `Modified patch
      in

      {
        id = new_id;
        name = name_change;
        start_time = start_time_change;
        end_time = end_time_change;
        loop = loop_change;
        signature = signature_change;
        sample_ref = sample_ref_change;
        fade = fade_change;
      }
end
