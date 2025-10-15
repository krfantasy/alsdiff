open Alsdiff_lib_live

(* Use the same type definitions as in diff.ml to avoid conflicts *)
type 'a modified = { old : 'a; new_ : 'a }

type 'a flat_change = [
  | `Unchanged
  | `Added of 'a
  | `Removed of 'a
  | `Modified of 'a modified
]

(* Equality module interface for local diff implementation *)
module type EQUALABLE = sig
  type t
  val equal : t -> t -> bool
end

(* Local implementation of ordered diffing (similar to diff_list_ord but without external dependency) *)
let diff_list_ord_local (type a) (module Eq : EQUALABLE with type t = a) (old_list : a list) (new_list : a list) : a flat_change list =
  (* Quick optimization: identical lists return empty *)
  if old_list = new_list then
    []
  else
    let old_len = List.length old_list in
    let new_len = List.length new_list in

    (* Convert lists to arrays for efficient indexing *)
    let old_arr = Array.of_list old_list in
    let new_arr = Array.of_list new_list in

    (* Create DP table for longest common subsequence *)
    let dp = Array.make_matrix (old_len + 1) (new_len + 1) 0 in

    (* Fill DP table *)
    for i = 1 to old_len do
      for j = 1 to new_len do
        if Eq.equal old_arr.(i - 1) new_arr.(j - 1) then
          dp.(i).(j) <- dp.(i - 1).(j - 1) + 1
        else
          dp.(i).(j) <- max dp.(i - 1).(j) dp.(i).(j - 1)
      done
    done;

    (* Convert DP table back to diff operations *)
    let rec backtrack i j acc =
      if i = 0 && j = 0 then
        acc
      else if i = 0 then
        backtrack i (j - 1) (`Added new_arr.(j - 1) :: acc)
      else if j = 0 then
        backtrack (i - 1) j (`Removed old_arr.(i - 1) :: acc)
      else if Eq.equal old_arr.(i - 1) new_arr.(j - 1) then
        backtrack (i - 1) (j - 1) (`Unchanged :: acc)
      else if dp.(i).(j - 1) >= dp.(i - 1).(j) then
        backtrack i (j - 1) (`Added new_arr.(j - 1) :: acc)
      else
        backtrack (i - 1) j (`Removed old_arr.(i - 1) :: acc)
    in

    backtrack old_len new_len []

module LoopSectionPatch = struct
  type t = {
    start_time : float flat_change;
    end_time : float flat_change;
    on : bool flat_change;
  }

  let diff (old_loop : Clip.LoopSection.t option) (new_loop : Clip.LoopSection.t option) : t option =
    match old_loop, new_loop with
    | None, None -> None
    | None, Some { Clip.LoopSection.start_time; end_time; on } ->
        Some {
          start_time = `Added start_time;
          end_time = `Added end_time;
          on = `Added on;
        }
    | Some { Clip.LoopSection.start_time; end_time; on }, None ->
        Some {
          start_time = `Removed start_time;
          end_time = `Removed end_time;
          on = `Removed on;
        }
    | Some { Clip.LoopSection.start_time = old_start; end_time = old_end; on = old_on },
      Some { Clip.LoopSection.start_time = new_start; end_time = new_end; on = new_on } ->
        let start_time_change =
          if old_start = new_start then `Unchanged
          else `Modified { old = old_start; new_ = new_start }
        in
        let end_time_change =
          if old_end = new_end then `Unchanged
          else `Modified { old = old_end; new_ = new_end }
        in
        let on_change =
          if old_on = new_on then `Unchanged
          else `Modified { old = old_on; new_ = new_on }
        in
        (* Check if anything changed *)
        if start_time_change = `Unchanged && end_time_change = `Unchanged && on_change = `Unchanged then
          None
        else
          Some { start_time = start_time_change; end_time = end_time_change; on = on_change }
end

module SampleRefPatch = struct
  type t = {
    file_path : string flat_change;
    crc : string flat_change;
    last_modified_date : int64 flat_change;
  }

  let diff (old_sample_ref : Clip.SampleRef.t) (new_sample_ref : Clip.SampleRef.t) : t option =
    let { Clip.SampleRef.file_path = old_file_path; crc = old_crc; last_modified_date = old_date } = old_sample_ref in
    let { Clip.SampleRef.file_path = new_file_path; crc = new_crc; last_modified_date = new_date } = new_sample_ref in

    let file_path_change =
      if old_file_path = new_file_path then
        `Unchanged
      else
        `Modified { old = old_file_path; new_ = new_file_path }
    in
    let crc_change =
      if old_crc = new_crc then
        `Unchanged
      else
        `Modified { old = old_crc; new_ = new_crc }
    in
    let last_modified_date_change =
      if old_date = new_date then
        `Unchanged
      else
        `Modified { old = old_date; new_ = new_date }
    in
    (* Check if anything changed *)
    if file_path_change = `Unchanged && crc_change = `Unchanged && last_modified_date_change = `Unchanged then
      None
    else
      Some { file_path = file_path_change; crc = crc_change; last_modified_date = last_modified_date_change }
end

module AudioClipPatch = struct
  type t = {
    name : string flat_change;
    start_time : float flat_change;
    end_time : float flat_change;
    loop : LoopSectionPatch.t option;
    signature : Clip.TimeSignature.t flat_change;
    sample_ref : SampleRefPatch.t option;
  }

  let diff (old_clip : Clip.AudioClip.t) (new_clip : Clip.AudioClip.t) : t option =
    let { Clip.AudioClip.id = old_id; name = old_name; start_time = old_start; end_time = old_end; loop = old_loop; signature = old_sig; sample_ref = old_sample } = old_clip in
    let { Clip.AudioClip.id = new_id; name = new_name; start_time = new_start; end_time = new_end; loop = new_loop; signature = new_sig; sample_ref = new_sample } = new_clip in

    (* Only compare clips with the same id *)
    if old_id <> new_id then
      None
    else
      let name_change =
        if old_name = new_name then `Unchanged
        else `Modified { old = old_name; new_ = new_name }
      in
      let start_time_change =
        if old_start = new_start then `Unchanged
        else `Modified { old = old_start; new_ = new_start }
      in
      let end_time_change =
        if old_end = new_end then `Unchanged
        else `Modified { old = old_end; new_ = new_end }
      in
      let loop_patch = LoopSectionPatch.diff old_loop new_loop in
      let signature_change =
        if Clip.TimeSignature.equal old_sig new_sig then `Unchanged
        else `Modified { old = old_sig; new_ = new_sig }
      in
      let sample_ref_patch = SampleRefPatch.diff old_sample new_sample in

      (* Check if anything changed *)
      if name_change = `Unchanged &&
         start_time_change = `Unchanged &&
         end_time_change = `Unchanged &&
         loop_patch = None &&
         signature_change = `Unchanged &&
         sample_ref_patch = None then
        None
      else
        Some {
          name = name_change;
          start_time = start_time_change;
          end_time = end_time_change;
          loop = loop_patch;
          signature = signature_change;
          sample_ref = sample_ref_patch;
        }
end


module MidiNotePatch = struct
  type t = {
    time : int flat_change;
    duration : int flat_change;
    velocity : int flat_change;
    off_velocity : int flat_change;
    note : int flat_change;
  }

  let diff (old_note : Clip.MidiNote.t) (new_note : Clip.MidiNote.t) : t option =
    let { Clip.MidiNote.time = old_time; duration = old_duration; velocity = old_velocity; off_velocity = old_off_velocity; note = old_note } = old_note in
    let { Clip.MidiNote.time = new_time; duration = new_duration; velocity = new_velocity; off_velocity = new_off_velocity; note = new_note } = new_note in

    let time_change =
      if old_time = new_time then `Unchanged
      else `Modified { old = old_time; new_ = new_time }
    in
    let duration_change =
      if old_duration = new_duration then `Unchanged
      else `Modified { old = old_duration; new_ = new_duration }
    in
    let velocity_change =
      if old_velocity = new_velocity then `Unchanged
      else `Modified { old = old_velocity; new_ = new_velocity }
    in
    let off_velocity_change =
      if old_off_velocity = new_off_velocity then `Unchanged
      else `Modified { old = old_off_velocity; new_ = new_off_velocity }
    in
    let note_change =
      if old_note = new_note then `Unchanged
      else `Modified { old = old_note; new_ = new_note }
    in

    (* Check if anything changed *)
    if time_change = `Unchanged &&
       duration_change = `Unchanged &&
       velocity_change = `Unchanged &&
       off_velocity_change = `Unchanged &&
       note_change = `Unchanged then
      None
    else
      Some {
        time = time_change;
        duration = duration_change;
        velocity = velocity_change;
        off_velocity = off_velocity_change;
        note = note_change;
      }
end

module MidiClipPatch = struct
  type t = {
    name : string flat_change;
    start_time : float flat_change;
    end_time : float flat_change;
    loop : LoopSectionPatch.t option;
    signature : Clip.TimeSignature.t flat_change;
    notes : Clip.MidiNote.t flat_change list;
  }

  let diff (old_clip : Clip.MidiClip.t) (new_clip : Clip.MidiClip.t) : t option =
    let { Clip.MidiClip.id = old_id; name = old_name; start_time = old_start; end_time = old_end; loop = old_loop; signature = old_sig; notes = old_notes } = old_clip in
    let { Clip.MidiClip.id = new_id; name = new_name; start_time = new_start; end_time = new_end; loop = new_loop; signature = new_sig; notes = new_notes } = new_clip in

    (* Only compare clips with the same id *)
    if old_id <> new_id then
      None
    else
      let name_change =
        if old_name = new_name then `Unchanged
        else `Modified { old = old_name; new_ = new_name }
      in
      let start_time_change =
        if old_start = new_start then `Unchanged
        else `Modified { old = old_start; new_ = new_start }
    in
    let end_time_change =
      if old_end = new_end then `Unchanged
      else `Modified { old = old_end; new_ = new_end }
    in
    let loop_patch = LoopSectionPatch.diff old_loop new_loop in
    let signature_change =
      if Clip.TimeSignature.equal old_sig new_sig then `Unchanged
      else `Modified { old = old_sig; new_ = new_sig }
    in

    (* Use local diff_list_ord for notes - cleaner and more consistent *)
    let notes_change =
      let module MidiNoteEq = struct
        type t = Clip.MidiNote.t
        let equal = (=)
      end in
      let (module Eq) = (module MidiNoteEq : EQUALABLE with type t = Clip.MidiNote.t) in
      diff_list_ord_local (module Eq) old_notes new_notes
    in

      (* Check if anything changed *)
      if name_change = `Unchanged &&
         start_time_change = `Unchanged &&
         end_time_change = `Unchanged &&
         loop_patch = None &&
         signature_change = `Unchanged &&
         notes_change = [] then
        None
      else
        Some {
          name = name_change;
          start_time = start_time_change;
          end_time = end_time_change;
          loop = loop_patch;
          signature = signature_change;
          notes = notes_change;
        }
end
