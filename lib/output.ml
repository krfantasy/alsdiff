open Live
open Diff

(** The interface for modules that can render automation patches into a specific format. *)
module type Output = sig
  (** The type of the rendered output (e.g., string for plain-text, or a more
      structured type for HTML). *)
  type t

  (** [render_event_change change] renders a single change to an envelope event. *)
  val render_event_change : EnvelopeEvent.t flat_change -> t

  (** [render_envelope_patch patch] renders the details of a patched envelope,
      including its list of event changes. *)
  val render_envelope_patch : AutomationEnvelopePatch.t -> t

  (** [render_envelope_op op] renders a single operation on the list of envelopes
      (Added, Removed, or Patched). *)
  val render_envelope_op : AutomationPatch.envelope_list_op -> t

  (** [render_automation_patch patch] is the main entry point. It renders the
      entire automation patch, including its list of envelope operations. *)
  val render_automation_patch : AutomationPatch.t -> t

  (** [render_mixer patch] renders the details of a patched mixer,
      including its volume, pan, mute, solo, and send changes. *)
  val render_mixer : MixerPatch.t -> t
end

(** A concrete implementation of the Output interface for plain-text rendering. *)
module TextOutput : Output with type t = string = struct
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

  let render_envelope_patch (patch : AutomationEnvelopePatch.t) =
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

  let render_automation_patch (patch : AutomationPatch.t) =
    let header = "Automation Patch:\n" in
    let change_lines = List.map render_envelope_op patch.AutomationPatch.envelope_changes in
    let filtered_lines = List.filter (fun s -> s <> "") change_lines in
    header ^ (String.concat "\n" filtered_lines)

  let render_mixer (patch : MixerPatch.t) =
    let header = "Mixer Patch:" in

    (* Render volume change *)
    let volume_line =
      match patch.MixerPatch.volume with
      | `Unchanged -> ""
      | `Added v -> Printf.sprintf "  + Volume: %.4f" v
      | `Removed v -> Printf.sprintf "  - Volume: %.4f" v
      | `Modified m -> Printf.sprintf "  ~ Volume changed from %.4f to %.4f" m.old m.new_
    in

    (* Render pan change *)
    let pan_line =
      match patch.MixerPatch.pan with
      | `Unchanged -> ""
      | `Added p -> Printf.sprintf "  + Pan: %.4f" p
      | `Removed p -> Printf.sprintf "  - Pan: %.4f" p
      | `Modified m -> Printf.sprintf "  ~ Pan changed from %.4f to %.4f" m.old m.new_
    in

    (* Render mute change *)
    let mute_line =
      match patch.MixerPatch.mute with
      | `Unchanged -> ""
      | `Added b -> Printf.sprintf "  + Mute: %b" b
      | `Removed b -> Printf.sprintf "  - Mute: %b" b
      | `Modified m -> Printf.sprintf "  ~ Mute changed from %b to %b" m.old m.new_
    in

    (* Render solo change *)
    let solo_line =
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
            match patch.SendPatch.target with
            | `Unchanged -> None
            | `Modified m -> Some (Printf.sprintf "target: %d->%d" m.old m.new_)
            | `Added t -> Some (Printf.sprintf "target: ->%d" t)
            | `Removed t -> Some (Printf.sprintf "target: %d->" t)
          in
          let amount_change_part = 
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

    let send_lines = List.map render_send_change patch.MixerPatch.sends in
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
end
