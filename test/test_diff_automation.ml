open Alcotest
open Live_git_lib
open Live_git_lib.Live
open Live_git_lib.Patch
open Live_git_lib.Diff

(** Helper to load an Automation.t from a file path. *)
let load_automation_from_file (path : string) : Automation.t =
  let (_, xml) = Xml.read_file path in
  let envelopes_element =
    match xml with
    | Xml.Element { name = "AutomationEnvelopes"; _ } as envelopes -> envelopes
    | _ -> failwith ("Root element in " ^ path ^ " is not AutomationEnvelopes")
  in
  create_automation envelopes_element

(** The main test function for the automation diffing logic. *)
let test_diff_logic () =
  (* 1. Load the old and new states from the XML files. *)
  let old_automation = load_automation_from_file "automation_old.xml" in
  let new_automation = load_automation_from_file "automation.xml" in

  (* 2. Compute the diff between the old and new states. *)
  let patch = diff_automation old_automation new_automation in
  let changes = patch.envelope_changes in

  (* 3. Assert the expected outcomes. *)
  check int "total changes" 5 (List.length changes);

  (* Partition the changes by type for easier and more robust assertions. *)
  let added_envelopes = List.filter_map (function `Added e -> Some e | _ -> None) changes in
  let removed_envelopes = List.filter_map (function `Removed e -> Some e | _ -> None) changes in
  let patched_envelopes = List.filter_map (function `Patched p -> Some p | _ -> None) changes in

  check int "added envelopes count" 2 (List.length added_envelopes);
  check int "removed envelopes count" 2 (List.length removed_envelopes);
  check int "patched envelopes count" 1 (List.length patched_envelopes);

  (* --- Assertions for Added Envelopes --- *)
  let added_6 = List.find_opt (fun (e:AutomationEnvelope.t) -> e.id = 6) added_envelopes in
  (match added_6 with
  | Some e -> check int "added envelope id 6 target" 27894 e.target
  | None -> fail "Expected to find added envelope with id 6");

  let added_3 = List.find_opt (fun (e:AutomationEnvelope.t) -> e.id = 3) added_envelopes in
  (match added_3 with
  | Some e -> check int "added envelope id 3 target" 27888 e.target
  | None -> fail "Expected to find added envelope with id 3");

  (* --- Assertions for Removed Envelopes --- *)
  let removed_7 = List.find_opt (fun (e:AutomationEnvelope.t) -> e.id = 7) removed_envelopes in
  (match removed_7 with
  | Some e -> check int "removed envelope id 7 target" 27902 e.target
  | None -> fail "Expected to find removed envelope with id 7");

  let removed_3 = List.find_opt (fun (e:AutomationEnvelope.t) -> e.id = 3) removed_envelopes in
  (match removed_3 with
  | Some e -> check int "removed envelope id 3 target" 99999 e.target
  | None -> fail "Expected to find removed envelope with id 3");

  (* --- Assertions for Patched Envelope --- *)
  let patched_0 = List.hd patched_envelopes in
  check int "patched envelope id" 0 patched_0.id;

  let event_changes = patched_0.events in
  check int "event changes count" 2 (List.length event_changes);

  let event_added = List.find_map (function `Added e -> Some e | _ -> None) event_changes in
  (match event_added with
  | Some e -> check (float 0.001) "added event time" 136.0 e.EnvelopeEvent.time
  | None -> fail "Expected to find an added event in patch for id 0");

  let event_removed = List.find_map (function `Removed e -> Some e | _ -> None) event_changes in
  (match event_removed with
  | Some e -> check (float 0.001) "removed event time" 140.0 e.EnvelopeEvent.time
  | None -> fail "Expected to find a removed event in patch for id 0")

(** Alcotest test suite setup. *)
let () =
  run "Diff Automation" [
    "diff-logic", [ test_case "Test automation diffing logic" `Quick test_diff_logic ];
  ]
