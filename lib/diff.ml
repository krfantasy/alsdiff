open Live
open Equality

(** The payload for a `Modified` change, containing the old and new values. *)
type 'a modified = { old : 'a; new_ : 'a }

(** A patch for a simple value, representing the change from an old to a new value. *)
type 'a flat_change = [
  | `Unchanged
  | `Added of 'a
  | `Removed of 'a
  | `Modified of 'a modified
]

(** A patch for a complex or list-like value.
    The `Patched` constructor holds a dedicated patch type `'p` which describes
    the internal changes to the value `'a`.
*)
type ('a, 'p) structured_change = [
  | `Unchanged
  | `Added of 'a
  | `Removed of 'a
  | `Patched of 'p
]

(** A patch for a single automation envelope. *)
type automation_envelope_patch = {
  id : int;
  target : int;
  events : EnvelopeEvent.t flat_change list;
}

(** An operation describing one change within a list of automation envelopes. *)
type envelope_list_op =
  (AutomationEnvelope.t, automation_envelope_patch) structured_change

(** A patch for the entire automation section. *)
type automation_patch = {
  envelope_changes : envelope_list_op list;
}


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


module AutomationEnvelopePatch = struct
  type t = automation_envelope_patch

  let diff (old_envelope : AutomationEnvelope.t) (new_envelope : AutomationEnvelope.t) : t option =
    let event_changes = diff_list (module EnvelopeEvent) old_envelope.events new_envelope.events in

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

module AutomationPatch = struct
  type t = automation_patch

  let diff (old_automation : Automation.t) (new_automation : Automation.t) : t =
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
          (match AutomationEnvelopePatch.diff old_env new_env with
           | Some patch -> Some (`Patched patch)
           | None -> None
          )
      | None, None -> None
    ) old_map new_map |> EnvelopeMap.to_seq |> Seq.map snd |> List.of_seq in

    { envelope_changes = changes }
end
