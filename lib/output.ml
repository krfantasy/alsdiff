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
  val render_envelope_patch : automation_envelope_patch -> t

  (** [render_envelope_op op] renders a single operation on the list of envelopes
      (Added, Removed, or Patched). *)
  val render_envelope_op : envelope_list_op -> t

  (** [render_automation_patch patch] is the main entry point. It renders the
      entire automation patch, including its list of envelope operations. *)
  val render_automation_patch : automation_patch -> t
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

  let render_envelope_patch patch =
    let header =
      Printf.sprintf "  ~ Patched Envelope (Id: %d, Target: %d):"
        patch.id patch.target
    in
    let event_lines = List.map render_event_change patch.events in
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

  let render_automation_patch patch =
    let header = "Automation Patch:\n" in
    let change_lines = List.map render_envelope_op patch.envelope_changes in
    let filtered_lines = List.filter (fun s -> s <> "") change_lines in
    header ^ (String.concat "\n" filtered_lines)
end
