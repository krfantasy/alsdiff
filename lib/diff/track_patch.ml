open Alsdiff_live.Track
open Diff

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
