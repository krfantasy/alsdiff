open Alsdiff_lib_live

(* Re-export the flat_change type from diff.ml *)
type 'a modified = { old : 'a; new_ : 'a }

type 'a flat_change = [
  | `Unchanged
  | `Added of 'a
  | `Removed of 'a
  | `Modified of 'a modified
]

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
    id : int flat_change;
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

    let id_change =
      if old_id = new_id then `Unchanged
      else `Modified { old = old_id; new_ = new_id }
    in
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
    if id_change = `Unchanged &&
       name_change = `Unchanged &&
       start_time_change = `Unchanged &&
       end_time_change = `Unchanged &&
       loop_patch = None &&
       signature_change = `Unchanged &&
       sample_ref_patch = None then
      None
    else
      Some {
        id = id_change;
        name = name_change;
        start_time = start_time_change;
        end_time = end_time_change;
        loop = loop_patch;
        signature = signature_change;
        sample_ref = sample_ref_patch;
      }
end
