open Live_git_lib.Xml
open Live_git_lib.Live

let test_create_automation () =
  (* Read the automation.xml file *)
  let (_, xml) = read_file "automation.xml" in

  (* Extract the AutomationEnvelopes element *)
  let automation_envelopes_element = match xml with
    | Element {name = "AutomationEnvelopes"; _} as envelopes -> envelopes
    | _ -> failwith "Root element is not AutomationEnvelopes"
  in

  (* Create automation from the XML *)
  let automation = Automation.create automation_envelopes_element in

  (* Check that we have the expected number of envelopes *)
  let expected_envelope_count = 6 in
  let actual_envelope_count = List.length automation.automation_envelopes in
  Alcotest.(check int) "envelope count" expected_envelope_count actual_envelope_count;

  (* Check the first envelope *)
  match automation.automation_envelopes with
  | first_envelope :: _ ->
      Alcotest.(check int) "first envelope id" 0 first_envelope.id;
      Alcotest.(check int) "first envelope target" 27743 first_envelope.target;
      let expected_event_count = 6 in
      let actual_event_count = List.length first_envelope.events in
      Alcotest.(check int) "first envelope event count" expected_event_count actual_event_count;

      (* Check the first event of the first envelope *)
      (match first_envelope.events with
       | first_event :: _ ->
           Alcotest.(check (float 0.0001)) "first event time" (-63072000.0) first_event.time;
           Alcotest.(check (float 0.0001)) "first event value" 0.1412537396 first_event.value;
       | [] -> Alcotest.fail "First envelope should have events")
  | [] -> Alcotest.fail "Should have envelopes"

let () =
  Alcotest.run "Live" [
    "create_automation",
    [
      Alcotest.test_case "parse automation XML" `Quick test_create_automation
    ]
  ]