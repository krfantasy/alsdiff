open Alsdiff_diff


(** The interface for modules that can render automation patches into a specific format. *)
module type Output = sig
  (** The type of the rendered output (e.g., string for plain-text, or a more
      structured type for HTML). *)
  type t

  (** [render_automation_patch patch] is the main entry point. It renders the
      entire automation patch, including its list of envelope operations. *)
  val render_automation_patch : Automation_patch.t -> t

  (** [render_audio_clip patch] renders the details of a patched audio clip,
      including name, timing, loop, signature, and sample reference changes. *)
  val render_audio_clip : Clip_patch.AudioClipPatch.t -> t

  (** [render_midi_clip patch] renders the details of a patched midi clip,
      including name, timing, loop, signature, and notes changes. *)
  val render_midi_clip : Clip_patch.MidiClipPatch.t -> t
end
