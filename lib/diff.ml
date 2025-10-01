open Live
open Patch
open Equality

(** A generic helper to find added and removed items between two lists.
    This is a simple set-based diff, not a sequence-based one. *)
let diff_list (type a) (module Eq : EQUATABLE with type t = a) (old_list : a list) (new_list : a list) =
  let old_seq = List.to_seq old_list in
  let new_seq = List.to_seq new_list in

  let removed = Seq.filter (fun old_item ->
    not (Seq.exists (Eq.equal old_item) new_seq)
  ) old_seq |> Seq.map (fun item -> `Removed item) in

  let added = Seq.filter (fun new_item ->
    not (Seq.exists (Eq.equal new_item) old_seq)
  ) new_seq |> Seq.map (fun item -> `Added item) in

  List.of_seq (Seq.append removed added)

(** Diffs the events within a single envelope. Returns Some(diff) or None if no changes. *)
let diff_envelope_events old_events new_events =
  let changes = diff_list (module EnvelopeEvent) old_events new_events in
  if changes = [] then None else Some changes

(** Diffs two envelopes that are known to have the same ID and target. Returns Some(patch) or None. *)
let diff_envelope (old_envelope : AutomationEnvelope.t) (new_envelope : AutomationEnvelope.t) : automation_envelope_patch option =
  let event_changes = diff_envelope_events old_envelope.events new_envelope.events in

  match event_changes with
  | None -> None (* No changes in events *)
  | Some events ->
      let patch = {
        id = new_envelope.id;
        target = new_envelope.target;
        events = events;
      } in
      Some patch

(** Compares two Automation objects and produces a patch describing the difference. *)
let diff_automation (old_automation : Automation.t) (new_automation : Automation.t) : automation_patch =
  (* The unique key for an envelope is the tuple (id, target). *)
  let module EnvelopeKey = struct
    type t = int * int
    let compare = compare
  end in
  let module EnvelopeMap = Map.Make(EnvelopeKey) in

  (* Convert lists of envelopes to maps keyed by their (id, target). *)
  let to_map = List.fold_left (fun map env ->
    let key = (env.AutomationEnvelope.id, env.AutomationEnvelope.target) in
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
        (match diff_envelope old_env new_env with
         | Some patch -> Some (`Patched patch) (* Envelopes differ *)
         | None -> None (* Envelopes are the same, so no change operation *)
        )
    | None, None -> None (* Should not happen in this logic *)
  ) old_map new_map |> EnvelopeMap.to_seq |> Seq.map snd |> List.of_seq in

  { envelope_changes = changes }
