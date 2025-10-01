open Live

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

(** A patch for a single automation envelope.
    The `id` and `target` are stable identifiers.
    The `events` list contains a series of changes to the individual envelope events.
*)
type automation_envelope_patch = {
  id : int;
  target : int;
  events : EnvelopeEvent.t flat_change list;
}

(** An operation describing one change within a list of automation envelopes.
    It can be an envelope that was added, removed, or patched (modified).
*)
type envelope_list_op =
  (AutomationEnvelope.t, automation_envelope_patch) structured_change

(** A patch for the entire automation section, consisting of a list of
    changes to the envelopes.
*)
type automation_patch = {
  envelope_changes : envelope_list_op list;
}
