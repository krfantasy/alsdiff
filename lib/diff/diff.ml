open Alsdiff_lib_live
open Alsdiff_lib_live.Track
open Alsdiff_lib_base.Equality

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


(** A generic helper to find added and removed items between two lists.
    This is a simple set-based diff, not a sequence-based one. *)
let diff_list (type a) (module Eq : EQUALABLE with type t = a) (old_list : a list) (new_list : a list) =
  let old_seq = List.to_seq old_list in
  let new_seq = List.to_seq new_list in

  let removed = Seq.filter (fun old_item ->
    not (Seq.exists (Eq.equal old_item) new_seq)
  ) old_seq |> Seq.map (fun item -> `Removed item) in

  let added = Seq.filter (fun new_item ->
    not (Seq.exists (Eq.equal new_item) old_seq)
  ) new_seq |> Seq.map (fun item -> `Added item) in

  List.of_seq (Seq.append removed added)

(** A sequence-based diff algorithm that preserves order and can detect moves.
    Returns a list of changes with their positions. *)
let diff_list_ord (type a) (module Eq : EQUALABLE with type t = a) (old_list : a list) (new_list : a list) : a flat_change list =
  let old_len = List.length old_list in
  let new_len = List.length new_list in

  (* Handle edge cases *)
  if old_len = 0 then
    List.map (fun item -> `Added item) new_list
  else if new_len = 0 then
    List.map (fun item -> `Removed item) old_list
  else
    (* Convert lists to arrays for efficient indexing *)
    let old_arr = Array.of_list old_list in
    let new_arr = Array.of_list new_list in

    (* Create a matrix to store the longest common subsequence lengths *)
    let dp = Array.make_matrix (old_len + 1) (new_len + 1) 0 in

    (* Fill the DP matrix *)
    for i = 1 to old_len do
      for j = 1 to new_len do
        if Eq.equal old_arr.(i - 1) new_arr.(j - 1) then
          dp.(i).(j) <- dp.(i - 1).(j - 1) + 1
        else
          dp.(i).(j) <- max dp.(i - 1).(j) dp.(i).(j - 1)
      done
    done;
    (* Note the semicolon above - we're sequencing the for loops with the backtrack *)

    (* Backtrack to find the diff operations *)
    let rec backtrack i j acc =
      if i = 0 && j = 0 then
        acc
      else if i = 0 then
        backtrack i (j - 1) (`Added new_arr.(j - 1) :: acc)
      else if j = 0 then
        backtrack (i - 1) j (`Removed old_arr.(i - 1) :: acc)
      else if Eq.equal old_arr.(i - 1) new_arr.(j - 1) then
        backtrack (i - 1) (j - 1) (`Unchanged :: acc)
      else if dp.(i).(j - 1) >= dp.(i - 1).(j) then
        backtrack i (j - 1) (`Added new_arr.(j - 1) :: acc)
      else
        backtrack (i - 1) j (`Removed old_arr.(i - 1) :: acc)
    in

    backtrack old_len new_len []

(** Myers' O(ND) diff algorithm - based on Eugene W. Myers' 1986 paper.
    Returns a list of changes representing the shortest edit script.
    Time complexity: O((N+M)D) where D is the size of the edit script.
    Space complexity: O((N+M)D) for trace storage.
*)
let diff_list_myers (type a) (module Eq : EQUALABLE with type t = a) (old_list : a list) (new_list : a list) : a flat_change list =
  let old_arr = Array.of_list old_list in
  let new_arr = Array.of_list new_list in
  let n = Array.length old_arr in
  let m = Array.length new_arr in

  (* Handle edge cases *)
  if n = 0 then List.map (fun x -> `Added x) new_list
  else if m = 0 then List.map (fun x -> `Removed x) old_list
  else
    (* Myers O(ND) algorithm implementation *)
    let max_d = n + m in
    let offset = max_d in (* Offset to handle negative k indices *)

    (* V array stores the furthest x position for each k-line *)
    let v = Array.make (2 * max_d + 1) 0 in
    (* Trace array stores V states for backtracking *)
    let traces = Array.init (max_d + 1) (fun _ -> Array.make (2 * max_d + 1) 0) in

    (* Follow diagonal (matching elements) as far as possible *)
    let rec follow_snake x y =
      if x < n && y < m && Eq.equal old_arr.(x) new_arr.(y) then
        follow_snake (x + 1) (y + 1)
      else (x, y)
    in

    (* Forward search to find the shortest edit distance *)
    let rec search d =
      if d > max_d then failwith "Myers algorithm: exceeded maximum edit distance";

      (* Store current V state for backtracking *)
      traces.(d) <- Array.copy v;

      (* Search k-lines from -d to +d *)
      let rec search_k k =
        if k > d then false (* Not found at this edit distance *)
        else
          (* Calculate x position based on previous k-lines *)
          let x =
            if k = -d || (k <> d && v.(k - 1 + offset) < v.(k + 1 + offset)) then
              v.(k + 1 + offset) (* Move down (insertion) *)
            else
              v.(k - 1 + offset) + 1 (* Move right (deletion) *)
          in
          let y = x - k in

          (* Follow diagonal (snake) *)
          let x_end, y_end = follow_snake x y in
          v.(k + offset) <- x_end;

          (* Check if we've reached the end *)
          if x_end >= n && y_end >= m then true
          else search_k (k + 2)
      in

      if search_k (-d) then d (* Found solution at edit distance d *)
      else search (d + 1) (* Try next edit distance *)
    in

    (* Find the edit distance *)
    let edit_distance = search 0 in

    (* Backtrack to reconstruct the edit script *)
    let result = ref [] in

    let rec backtrack d x y =
      if d = 0 then
        (* At edit distance 0, everything is unchanged *)
        let rec add_unchanged i =
          if i >= 0 then (
            result := `Unchanged :: !result;
            add_unchanged (i - 1)
          )
        in
        add_unchanged (x - 1)
      else
        let prev_v = traces.(d) in
        let k = x - y in

        (* Determine which previous k-line we came from *)
        let prev_k =
          if k = -d || (k <> d && prev_v.(k - 1 + offset) < prev_v.(k + 1 + offset)) then
            k + 1 (* Came from insertion *)
          else
            k - 1 (* Came from deletion *)
        in

        let prev_x = prev_v.(prev_k + offset) in
        let prev_y = prev_x - prev_k in

        (* Calculate where the snake started *)
        let snake_start_x, snake_start_y =
          if prev_k = k - 1 then (prev_x + 1, prev_y) (* After deletion *)
          else (prev_x, prev_y + 1) (* After insertion *)
        in

        (* Add unchanged elements from the snake *)
        let rec add_snake curr_x curr_y =
          if curr_x > snake_start_x && curr_y > snake_start_y then (
            result := `Unchanged :: !result;
            add_snake (curr_x - 1) (curr_y - 1)
          )
        in
        add_snake x y;

        (* Add the edit operation *)
        if prev_k = k - 1 then
          (* Deletion *)
          result := `Removed old_arr.(snake_start_x - 1) :: !result
        else
          (* Insertion *)
          result := `Added new_arr.(snake_start_y - 1) :: !result;

        (* Continue backtracking *)
        backtrack (d - 1) prev_x prev_y
    in

    backtrack edit_distance n m;
    List.rev !result

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

module AutomationPatch = struct
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
end

module SendPatch = struct
  type t = {
    target : int flat_change;
    amount : float flat_change;
  }

  let diff (old_send : Send.t) (new_send : Send.t) : t =
    let target_change =
      if old_send.target = new_send.target then
        `Unchanged
      else
        `Modified { old = old_send.target; new_ = new_send.target }
    in
    let amount_change =
      if old_send.amount = new_send.amount then
        `Unchanged
      else
        `Modified { old = old_send.amount; new_ = new_send.amount }
    in
    { target = target_change; amount = amount_change }

end

module MixerPatch = struct
  type send_changes = (Send.t, SendPatch.t) structured_change list

  type t = {
    volume : float flat_change;
    pan : float flat_change;
    mute : bool flat_change;
    solo : bool flat_change;
    sends : send_changes;
  }

  let diff (old_mixer : Mixer.t) (new_mixer : Mixer.t) : t =
    let volume_change =
      if old_mixer.volume = new_mixer.volume then
        `Unchanged
      else
        `Modified { old = old_mixer.volume; new_ = new_mixer.volume }
    in
    let pan_change =
      if old_mixer.pan = new_mixer.pan then
        `Unchanged
      else
        `Modified { old = old_mixer.pan; new_ = new_mixer.pan }
    in
    let mute_change =
      if old_mixer.mute = new_mixer.mute then
        `Unchanged
      else
        `Modified { old = old_mixer.mute; new_ = new_mixer.mute }
    in
    let solo_change =
      if old_mixer.solo = new_mixer.solo then
        `Unchanged
      else
        `Modified { old = old_mixer.solo; new_ = new_mixer.solo }
    in

    (* To properly handle send changes, we need to match sends that represent the same logical send
       Since the target is always 0, we'll use a combination of target and amount to match sends
       For the purpose of this diff, sends with the same target and amount are considered the same *)
    let module SendKey = struct
      type t = int * float  (* target * amount - though target is always 0 *)
      let compare = compare
    end in
    let module SendMap = Map.Make(SendKey) in

    (* Create maps of sends keyed by target and amount *)
    let send_to_key send = (send.Send.target, send.Send.amount) in
    let to_map sends =
      List.fold_left (fun map send ->
        let key = send_to_key send in
        SendMap.add key send map
      ) SendMap.empty sends
    in

    let old_map = to_map old_mixer.sends in
    let new_map = to_map new_mixer.sends in

    (* Merge the maps to find changes *)
    let send_changes = SendMap.merge (fun _ old_opt new_opt ->
      match old_opt, new_opt with
      | Some old_send, None -> Some (`Removed old_send)
      | None, Some new_send -> Some (`Added new_send)
      | Some old_send, Some new_send ->
          if Send.equal old_send new_send then
            None  (* Unchanged, don't include in changes list *)
          else
            (* Send was modified, create a patch *)
            let patch = SendPatch.diff old_send new_send in
            Some (`Patched patch)
      | None, None -> None
    ) old_map new_map |> SendMap.to_seq |> Seq.map snd |> List.of_seq in

    { volume = volume_change;
      pan = pan_change;
      mute = mute_change;
      solo = solo_change;
      sends = send_changes;
    }

end
