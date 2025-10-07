open Alsdiff_lib_live.Automation
open Alsdiff_lib_diff.Diff

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
