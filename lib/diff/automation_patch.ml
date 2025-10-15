open Alsdiff_lib_live
open Diff

module AutomationEnvelopePatch = struct
  (** A patch for a single automation envelope. *)
  type t = {
    id : int;
    target : int;
    events : Automation.EnvelopeEvent.t flat_change list;
  }

  let diff (old_envelope : Automation.AutomationEnvelope.t) (new_envelope : Automation.AutomationEnvelope.t) : t option =
    let event_changes = diff_list (module Automation.EnvelopeEvent) old_envelope.events new_envelope.events in

    match event_changes with
    | [] -> None (* No changes in events *)
    | changes ->
        let patch = {
          id = new_envelope.id;
          target = new_envelope.target;
          events = changes;
        } in
        Some patch
end


(** An operation describing one change within a list of automation envelopes. *)
type envelope_list_op =
  (Automation.AutomationEnvelope.t, AutomationEnvelopePatch.t) structured_change

type t = {
  envelope_changes : envelope_list_op list;
}

let diff (old_automation : Automation.t) (new_automation : Automation.t) : t =
  (* The unique key for an envelope is the tuple (id, target). *)
  let module EnvelopeKey = struct
    type t = int * int
    let compare = compare
  end in
  let module EnvelopeMap = Map.Make(EnvelopeKey) in

  (* Convert lists of envelopes to maps keyed by their (id, target). *)
  let to_map = List.fold_left (fun map env ->
      let key = (env.Automation.AutomationEnvelope.id, env.Automation.AutomationEnvelope.target) in
      EnvelopeMap.add key env map
    ) EnvelopeMap.empty in

  let old_map = to_map old_automation.automation_envelopes in
  let new_map = to_map new_automation.automation_envelopes in

  (* Merge the two maps to find additions, removals, and modifications. *)
  let changes = EnvelopeMap.merge (fun _ old_opt new_opt ->
      match old_opt, new_opt with
      | Some old_env, None -> Some (`Removed old_env)
      | None, Some new_env -> Some (`Added new_env)
      | Some old_env, Some new_env ->
        (match AutomationEnvelopePatch.diff old_env new_env with
         | Some patch -> Some (`Patched patch)
         | None -> None
        )
      | None, None -> None
    ) old_map new_map |> EnvelopeMap.to_seq |> Seq.map snd |> List.of_seq in

  { envelope_changes = changes }
