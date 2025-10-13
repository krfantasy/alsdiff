open Alsdiff_lib_base.Xml
open Alsdiff_lib_live.Clip.MidiClip
open Alsdiff_lib_live.Clip.TimeSignature
open Alsdiff_lib_live.Clip.LoopSection

let test_midi_clip_parsing () =
  (* Read the midi_clip.xml file *)
  let (_, xml) = read_file "midi_clip.xml" in

  (* Create midi clip from the XML *)
  let midi_clip = create xml in

  (* Expected values based on the XML file *)
  let expected_id = 2 in
  let expected_start_time = 80.0 in
  let expected_end_time = 100.0 in
  let expected_signature = { numer = 4; denom = 4 } in
  let expected_loop = Some {
    start_time = 92.0;
    end_time = 112.0;
    on = false;
  } in

  (* Test basic fields *)
  Alcotest.(check int) "id" expected_id midi_clip.id;
  Alcotest.(check (float 0.001)) "start_time" expected_start_time midi_clip.start_time;
  Alcotest.(check (float 0.001)) "end_time" expected_end_time midi_clip.end_time;
  Alcotest.(check (pair int int)) "signature" (expected_signature.numer, expected_signature.denom) (midi_clip.signature.numer, midi_clip.signature.denom);

  (* Test loop section *)
  match midi_clip.loop, expected_loop with
  | Some actual_loop, Some expected_loop ->
      Alcotest.(check (float 0.001)) "loop.start_time" expected_loop.start_time actual_loop.start_time;
      Alcotest.(check (float 0.001)) "loop.end_time" expected_loop.end_time actual_loop.end_time;
      Alcotest.(check bool) "loop.on" expected_loop.on actual_loop.on
  | None, _ -> Alcotest.fail "Expected midi clip loop to be Some"
  | _, None -> Alcotest.fail "Expected expected_loop to be Some"

  (* TODO: Fix notes field access issue *)
  (* let notes_list = midi_clip.notes in *)
  (* let actual_note_count = List.length notes_list in *)
  (* Alcotest.(check int) "note count" expected_note_count actual_note_count; *)

  (* Check some note details *)
  (* if actual_note_count > 0 then ( *)
  (*   let first_note = List.hd notes_list in *)
  (*   Alcotest.(check int) "first note time" 3 first_note.time; *)
  (*   Alcotest.(check int) "first note duration" 0 first_note.duration; *)
  (*   Alcotest.(check int) "first note velocity" 100 first_note.velocity; *)
  (*   Alcotest.(check int) "first note note" 1 first_note.note *)
  (* ) else *)
  (*   Alcotest.fail "Expected at least one MIDI note" *)

let () =
  Alcotest.run "MidiClip" [
    "create_midi_clip",
    [
      Alcotest.test_case "parse MidiClip XML" `Quick test_midi_clip_parsing
    ]
  ]
