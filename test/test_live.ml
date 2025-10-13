open Alsdiff_lib_base.Xml
open Alsdiff_lib_live
open Alsdiff_lib_live.Track

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

let test_create_mixer () =
  (* Read the mixer.xml file *)
  let (_, xml) = read_file "mixer.xml" in

  (* Create mixer from the XML *)
  let mixer = Mixer.create xml in

  (* Check mixer values *)
  Alcotest.(check (float 0.0001)) "volume" 0.5011872053 mixer.volume;
  Alcotest.(check (float 0.0001)) "pan" 0.4699999988 mixer.pan;
  Alcotest.(check bool) "mute" true mixer.mute;
  Alcotest.(check bool) "solo" false mixer.solo;

  (* Check sends *)
  let expected_sends_count = 1 in
  let actual_sends_count = List.length mixer.sends in
  Alcotest.(check int) "sends count" expected_sends_count actual_sends_count;

  (* Check the first send *)
  (match mixer.sends with
   | first_send :: _ ->
       Alcotest.(check int) "first send target" 0 first_send.target;
       Alcotest.(check (float 0.0001)) "first send amount" 0.0003162277571 first_send.amount;
   | [] -> Alcotest.fail "Should have sends")

let () =
  Alcotest.run "Live" [
    "create_automation",
    [
      Alcotest.test_case "parse automation XML" `Quick test_create_automation
    ];
    "create_mixer",
    [
      Alcotest.test_case "parse mixer XML" `Quick test_create_mixer
    ]
  ]
