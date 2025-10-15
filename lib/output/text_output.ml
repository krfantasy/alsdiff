open Alsdiff_lib_live.Automation
open Alsdiff_lib_live.Track
open Alsdiff_lib_live.Clip
open Alsdiff_lib_diff.Diff
open Alsdiff_lib_diff

(* Import Clip_patch with an alias to avoid conflicts *)
module CP = Alsdiff_lib_diff.Clip_patch

type t = string

let render_event_change change =
  match change with
  | `Unchanged -> ""
  | `Added event ->
      Printf.sprintf "    + Event at time %.2f with value %.4f"
        event.EnvelopeEvent.time event.EnvelopeEvent.value
  | `Removed event ->
      Printf.sprintf "    - Event at time %.2f with value %.4f"
        event.EnvelopeEvent.time event.EnvelopeEvent.value
  | `Modified m ->
      Printf.sprintf "    ~ Event at time %.2f changed value from %.4f to %.4f"
        m.old.EnvelopeEvent.time m.old.EnvelopeEvent.value m.new_.EnvelopeEvent.value

let render_envelope_patch (patch : Automation_patch.AutomationEnvelopePatch.t) =
  let open Alsdiff_lib_diff.Automation_patch in
  let header =
    Printf.sprintf "  ~ Patched Envelope (Id: %d, Target: %d):"
      patch.AutomationEnvelopePatch.id patch.AutomationEnvelopePatch.target
  in
  let event_lines = List.map render_event_change patch.AutomationEnvelopePatch.events in
  let non_empty_lines = List.filter (fun s -> s <> "") event_lines in
  String.concat "\n" (header :: non_empty_lines)

let render_envelope_op op =
  match op with
  | `Unchanged -> ""
  | `Added env ->
      Printf.sprintf "+ Added Envelope (Id: %d, Target: %d)"
        env.AutomationEnvelope.id env.AutomationEnvelope.target
  | `Removed env ->
      Printf.sprintf "- Removed Envelope (Id: %d, Target: %d)"
        env.AutomationEnvelope.id env.AutomationEnvelope.target
  | `Patched patch ->
      render_envelope_patch patch

let render_automation_patch (patch : Automation_patch.t) =
  let header = "Automation Patch:\n" in
  let change_lines = List.map render_envelope_op patch.Automation_patch.envelope_changes in
  let filtered_lines = List.filter (fun s -> s <> "") change_lines in
  header ^ (String.concat "\n" filtered_lines)

let render_mixer (patch : Track_patch.MixerPatch.t) =
    let header = "Mixer Patch:" in

    (* Render volume change *)
    let volume_line =
      let open Track_patch in
      match patch.MixerPatch.volume with
      | `Unchanged -> ""
      | `Added v -> Printf.sprintf "  + Volume: %.4f" v
      | `Removed v -> Printf.sprintf "  - Volume: %.4f" v
      | `Modified m -> Printf.sprintf "  ~ Volume changed from %.4f to %.4f" m.old m.new_
    in

    (* Render pan change *)
    let pan_line =
      let open Track_patch in
      match patch.MixerPatch.pan with
      | `Unchanged -> ""
      | `Added p -> Printf.sprintf "  + Pan: %.4f" p
      | `Removed p -> Printf.sprintf "  - Pan: %.4f" p
      | `Modified m -> Printf.sprintf "  ~ Pan changed from %.4f to %.4f" m.old m.new_
    in

    (* Render mute change *)
    let mute_line =
      let open Track_patch in
      match patch.MixerPatch.mute with
      | `Unchanged -> ""
      | `Added b -> Printf.sprintf "  + Mute: %b" b
      | `Removed b -> Printf.sprintf "  - Mute: %b" b
      | `Modified m -> Printf.sprintf "  ~ Mute changed from %b to %b" m.old m.new_
    in

    (* Render solo change *)
    let solo_line =
      let open Track_patch in
      match patch.MixerPatch.solo with
      | `Unchanged -> ""
      | `Added b -> Printf.sprintf "  + Solo: %b" b
      | `Removed b -> Printf.sprintf "  - Solo: %b" b
      | `Modified m -> Printf.sprintf "  ~ Solo changed from %b to %b" m.old m.new_
    in

    (* Render send changes *)
    let render_send_change change =
      match change with
      | `Unchanged -> ""
      | `Added send -> Printf.sprintf "  + Send to track %d with amount %.4f" send.Send.target send.Send.amount
      | `Removed send -> Printf.sprintf "  - Send to track %d with amount %.4f" send.Send.target send.Send.amount
      | `Patched patch ->
          (* For a patched send, we have a SendPatch.t *)
          let target_change_part =
            let open Track_patch in
            match patch.SendPatch.target with
            | `Unchanged -> None
            | `Modified m -> Some (Printf.sprintf "target: %d->%d" m.old m.new_)
            | `Added t -> Some (Printf.sprintf "target: ->%d" t)
            | `Removed t -> Some (Printf.sprintf "target: %d->" t)
          in
          let amount_change_part =
            let open Track_patch in
            match patch.SendPatch.amount with
            | `Unchanged -> None
            | `Modified m -> Some (Printf.sprintf "amount: %.4f->%.4f" m.old m.new_)
            | `Added v -> Some (Printf.sprintf "amount: ->%.4f" v)
            | `Removed v -> Some (Printf.sprintf "amount: %.4f->" v)
          in
          let parts = List.filter_map (fun x -> x) [target_change_part; amount_change_part] in
          match parts with
          | [] -> ""
          | [part] -> Printf.sprintf "    ~ Send modified (%s)" part
          | parts -> Printf.sprintf "    ~ Send modified (%s)" (String.concat ", " parts)
    in

    let send_lines = List.map render_send_change patch.Track_patch.MixerPatch.sends in
    let non_empty_send_lines = List.filter (fun s -> s <> "") send_lines in
    let send_section =
      if List.length non_empty_send_lines > 0 then
        "  Send Changes:\n" ^ (String.concat "\n" (List.map (fun s -> "  " ^ s) non_empty_send_lines))
      else
        ""
    in

    let all_lines = [header; volume_line; pan_line; mute_line; solo_line; send_section] in
    let non_empty_lines = List.filter (fun s -> s <> "") all_lines in
    String.concat "\n" non_empty_lines

  let render_loop_section_patch (patch : CP.LoopSectionPatch.t) =
    (* Render start time change *)
    let start_line =
      match patch.CP.LoopSectionPatch.start_time with
      | `Unchanged -> ""
      | `Added v -> Printf.sprintf "    + Loop start: %.2f" v
      | `Removed v -> Printf.sprintf "    - Loop start: %.2f" v
      | `Modified m -> Printf.sprintf "    ~ Loop start changed from %.2f to %.2f" m.old m.new_
    in

    (* Render end time change *)
    let end_line =
      match patch.CP.LoopSectionPatch.end_time with
      | `Unchanged -> ""
      | `Added v -> Printf.sprintf "    + Loop end: %.2f" v
      | `Removed v -> Printf.sprintf "    - Loop end: %.2f" v
      | `Modified m -> Printf.sprintf "    ~ Loop end changed from %.2f to %.2f" m.old m.new_
    in

    (* Render loop on/off change *)
    let on_line =
      match patch.CP.LoopSectionPatch.on with
      | `Unchanged -> ""
      | `Added b -> Printf.sprintf "    + Loop enabled: %b" b
      | `Removed b -> Printf.sprintf "    - Loop enabled: %b" b
      | `Modified m -> Printf.sprintf "    ~ Loop enabled changed from %b to %b" m.old m.new_
    in

    let all_lines = [start_line; end_line; on_line] in
    let non_empty_lines = List.filter (fun s -> s <> "") all_lines in
    String.concat "\n" non_empty_lines

  let render_sample_ref_patch (patch : CP.SampleRefPatch.t) =
    (* Render file path change *)
    let file_path_line =
      match patch.CP.SampleRefPatch.file_path with
      | `Unchanged -> ""
      | `Added p -> Printf.sprintf "    + File path: %s" p
      | `Removed p -> Printf.sprintf "    - File path: %s" p
      | `Modified m -> Printf.sprintf "    ~ File path changed from %s to %s" m.old m.new_
    in

    (* Render CRC change *)
    let crc_line =
      match patch.CP.SampleRefPatch.crc with
      | `Unchanged -> ""
      | `Added c -> Printf.sprintf "    + CRC: %s" c
      | `Removed c -> Printf.sprintf "    - CRC: %s" c
      | `Modified m -> Printf.sprintf "    ~ CRC changed from %s to %s" m.old m.new_
    in

    (* Render last modified date change *)
    let last_modified_line =
      match patch.CP.SampleRefPatch.last_modified_date with
      | `Unchanged -> ""
      | `Added d -> Printf.sprintf "    + Last modified: %Ld" d
      | `Removed d -> Printf.sprintf "    - Last modified: %Ld" d
      | `Modified m -> Printf.sprintf "    ~ Last modified changed from %Ld to %Ld" m.old m.new_
    in

    let all_lines = [file_path_line; crc_line; last_modified_line] in
    let non_empty_lines = List.filter (fun s -> s <> "") all_lines in
    String.concat "\n" non_empty_lines

  let render_audio_clip (patch : CP.AudioClipPatch.t) =
    let header = "Audio Clip Patch:" in

    (* Render name change *)
    let name_line =
      match patch.CP.AudioClipPatch.name with
      | `Unchanged -> ""
      | `Added n -> Printf.sprintf "  + Name: %s" n
      | `Removed n -> Printf.sprintf "  - Name: %s" n
      | `Modified m -> Printf.sprintf "  ~ Name changed from %s to %s" m.old m.new_
    in

    (* Render start time change *)
    let start_time_line =
      match patch.CP.AudioClipPatch.start_time with
      | `Unchanged -> ""
      | `Added t -> Printf.sprintf "  + Start time: %.2f" t
      | `Removed t -> Printf.sprintf "  - Start time: %.2f" t
      | `Modified m -> Printf.sprintf "  ~ Start time changed from %.2f to %.2f" m.old m.new_
    in

    (* Render end time change *)
    let end_time_line =
      match patch.CP.AudioClipPatch.end_time with
      | `Unchanged -> ""
      | `Added t -> Printf.sprintf "  + End time: %.2f" t
      | `Removed t -> Printf.sprintf "  - End time: %.2f" t
      | `Modified m -> Printf.sprintf "  ~ End time changed from %.2f to %.2f" m.old m.new_
    in

    (* Render signature change *)
    let signature_line =
      match patch.CP.AudioClipPatch.signature with
      | `Unchanged -> ""
      | `Added signature -> Printf.sprintf "  + Time signature: %d/%d" signature.TimeSignature.numer signature.TimeSignature.denom
      | `Removed signature -> Printf.sprintf "  - Time signature: %d/%d" signature.TimeSignature.numer signature.TimeSignature.denom
      | `Modified m -> Printf.sprintf "  ~ Time signature changed from %d/%d to %d/%d"
          m.old.TimeSignature.numer m.old.TimeSignature.denom
          m.new_.TimeSignature.numer m.new_.TimeSignature.denom
    in

    (* Render loop change *)
    let loop_section =
      match patch.CP.AudioClipPatch.loop with
      | None -> ""
      | Some loop_patch ->
          let loop_content = render_loop_section_patch loop_patch in
          if loop_content = "" then ""
          else "  Loop Changes:\n" ^ (String.concat "\n" (List.map (fun s -> "  " ^ s) [loop_content]))
    in

    (* Render sample reference change *)
    let sample_ref_section =
      match patch.CP.AudioClipPatch.sample_ref with
      | None -> ""
      | Some sample_ref_patch ->
          let sample_ref_content = render_sample_ref_patch sample_ref_patch in
          if sample_ref_content = "" then ""
          else "  Sample Reference Changes:\n" ^ (String.concat "\n" (List.map (fun s -> "  " ^ s) [sample_ref_content]))
    in

    let all_lines = [header; name_line; start_time_line; end_time_line; signature_line; loop_section; sample_ref_section] in
    let non_empty_lines = List.filter (fun s -> s <> "") all_lines in
    String.concat "\n" non_empty_lines

  let render_midi_clip (patch : CP.MidiClipPatch.t) =
    let header = "Midi Clip Patch:" in

    (* Render name change *)
    let name_line =
      match patch.CP.MidiClipPatch.name with
      | `Unchanged -> ""
      | `Added n -> Printf.sprintf "  + Name: %s" n
      | `Removed n -> Printf.sprintf "  - Name: %s" n
      | `Modified m -> Printf.sprintf "  ~ Name changed from %s to %s" m.old m.new_
    in

    (* Render start time change *)
    let start_time_line =
      match patch.CP.MidiClipPatch.start_time with
      | `Unchanged -> ""
      | `Added t -> Printf.sprintf "  + Start time: %.2f" t
      | `Removed t -> Printf.sprintf "  - Start time: %.2f" t
      | `Modified m -> Printf.sprintf "  ~ Start time changed from %.2f to %.2f" m.old m.new_
    in

    (* Render end time change *)
    let end_time_line =
      match patch.CP.MidiClipPatch.end_time with
      | `Unchanged -> ""
      | `Added t -> Printf.sprintf "  + End time: %.2f" t
      | `Removed t -> Printf.sprintf "  - End time: %.2f" t
      | `Modified m -> Printf.sprintf "  ~ End time changed from %.2f to %.2f" m.old m.new_
    in

    (* Render signature change *)
    let signature_line =
      match patch.CP.MidiClipPatch.signature with
      | `Unchanged -> ""
      | `Added signature -> Printf.sprintf "  + Time signature: %d/%d" signature.TimeSignature.numer signature.TimeSignature.denom
      | `Removed signature -> Printf.sprintf "  - Time signature: %d/%d" signature.TimeSignature.numer signature.TimeSignature.denom
      | `Modified m -> Printf.sprintf "  ~ Time signature changed from %d/%d to %d/%d"
          m.old.TimeSignature.numer m.old.TimeSignature.denom
          m.new_.TimeSignature.numer m.new_.TimeSignature.denom
    in

    (* Render loop change *)
    let loop_section =
      match patch.CP.MidiClipPatch.loop with
      | None -> ""
      | Some loop_patch ->
          let loop_content = render_loop_section_patch loop_patch in
          if loop_content = "" then ""
          else "  Loop Changes:\n" ^ (String.concat "\n" (List.map (fun s -> "  " ^ s) [loop_content]))
    in

    (* Render notes changes *)
    let render_note_change (change : MidiNote.t CP.flat_change) =
      match change with
      | `Unchanged -> ""
      | `Added note -> Printf.sprintf "    + Note: time=%d, duration=%d, velocity=%d, note=%d"
          note.MidiNote.time note.MidiNote.duration note.MidiNote.velocity note.MidiNote.note
      | `Removed note -> Printf.sprintf "    - Note: time=%d, duration=%d, velocity=%d, note=%d"
          note.MidiNote.time note.MidiNote.duration note.MidiNote.velocity note.MidiNote.note
      | `Modified m -> Printf.sprintf "    ~ Note changed: time %d->%d, duration %d->%d, velocity %d->%d, note %d->%d"
          m.old.MidiNote.time m.new_.MidiNote.time
          m.old.MidiNote.duration m.new_.MidiNote.duration
          m.old.MidiNote.velocity m.new_.MidiNote.velocity
          m.old.MidiNote.note m.new_.MidiNote.note
    in
    let notes_section =
      let note_lines = List.map render_note_change patch.CP.MidiClipPatch.notes in
      let non_empty_note_lines = List.filter (fun s -> s <> "") note_lines in
      if List.length non_empty_note_lines > 0 then
        "  Notes Changes:\n" ^ (String.concat "\n" non_empty_note_lines)
      else ""
    in

    let all_lines = [header; name_line; start_time_line; end_time_line; signature_line; loop_section; notes_section] in
    let non_empty_lines = List.filter (fun s -> s <> "") all_lines in
    String.concat "\n" non_empty_lines
