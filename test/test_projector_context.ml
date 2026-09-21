open Alcotest
open Alsdiff_live
open Alsdiff_output.View_model
open Track_helpers

(* All fixtures (make_note/make_clip/make_midi_track/make_return_track/
   make_main_track/make_automation/make_event/make_test_liveset) come from
   Track_helpers. *)

(* [Ctx.empty] is the no-reference context: every lookup is [None], including
   the Main sentinel (id 0). *)
let test_empty_ctx_all_lookups_none () =
  let ctx = Ctx.empty in
  check bool "no track" true (Ctx.track ctx ~id:1 = None);
  check bool "no track for Main sentinel id 0" true (Ctx.track ctx ~id:0 = None);
  check bool "no main track" true (Ctx.main_track ctx = None);
  check bool "no midi clip" true (Ctx.midi_clip ctx ~track_id:1 ~id:9 = None);
  check bool "no audio clip" true (Ctx.audio_clip ctx ~track_id:1 ~id:9 = None);
  check bool "no automation" true (Ctx.automation ctx ~track_id:1 ~id:2 = None);
  check bool "no main automation" true (Ctx.automation ctx ~track_id:0 ~id:2 = None);
  check bool "no note" true (Ctx.midi_note ctx ~track_id:1 ~clip_id:9 ~id:7 = None);
  check bool "no event" true
    (Ctx.envelope_event ctx ~track_id:1 ~automation_id:2 ~id:5 = None)

let test_resolves_tracks_clips_notes () =
  let note = make_note 7 52 in
  let clip = make_clip 9 [note] in
  let ls = Track_helpers.make_test_liveset
      ~main:(make_main_track ())
      ~tracks:[Track.Midi (make_midi_track ~clips:[clip] 1 "Bass")]
  in
  let ctx = Ctx.of_liveset ls in
  (match Ctx.track ctx ~id:1 with
   | Some (Track.Midi t) -> check string "track name" "Bass" t.Track.MidiTrack.name
   | _ -> fail "expected Midi track 1");
  check bool "Main sentinel never resolves as a track" true
    (Ctx.track ctx ~id:0 = None);
  check bool "missing track" true (Ctx.track ctx ~id:99 = None);
  (match Ctx.midi_clip ctx ~track_id:1 ~id:9 with
   | Some cl -> check int "clip id" 9 cl.Clip.MidiClip.id
   | None -> fail "expected clip 9 in track 1");
  check bool "missing clip id" true (Ctx.midi_clip ctx ~track_id:1 ~id:10 = None);
  check bool "clip of missing track" true (Ctx.midi_clip ctx ~track_id:99 ~id:9 = None);
  (match Ctx.midi_note ctx ~track_id:1 ~clip_id:9 ~id:7 with
   | Some n -> check int "note pitch" 52 n.Clip.MidiNote.note
   | None -> fail "expected note 7 in clip 9");
  check bool "missing note id" true
    (Ctx.midi_note ctx ~track_id:1 ~clip_id:9 ~id:8 = None);
  (* Negative memoization: a note inside an absent clip resolves to [None]
     (and must not blow up on repeated lookups). *)
  check bool "note in missing clip" true
    (Ctx.midi_note ctx ~track_id:1 ~clip_id:99 ~id:7 = None);
  (* Repeated lookups hit the memo tables and stay stable. *)
  check bool "clip lookup stable" true (Ctx.midi_clip ctx ~track_id:1 ~id:9 <> None);
  check bool "note lookup stable" true
    (Ctx.midi_note ctx ~track_id:1 ~clip_id:9 ~id:7 <> None)

let test_return_track_and_main_context () =
  let ls = Track_helpers.make_test_liveset
      ~main:(make_main_track ())
      ~tracks:[Track.Return (make_return_track 3 "Return A")]
  in
  let ctx = Ctx.of_liveset ls in
  (match Ctx.main_track ctx with
   | Some m ->
     check string "main tempo param" "Tempo"
       m.Track.MainTrack.mixer.tempo.Device.GenericParam.name
   | None -> fail "expected main track");
  (match Ctx.track ctx ~id:3 with
   | Some (Track.Return t) ->
     check string "return name" "Return A" t.Track.AudioTrack.name
   | _ -> fail "expected Return track 3");
  check bool "audio clips of a return track" true
    (Ctx.audio_clip ctx ~track_id:3 ~id:9 = None)

(* Main-track automations resolve through the id-0 sentinel scope. *)
let test_main_automation_and_event_scope () =
  let auto = make_automation 2 8 [make_event 5 1.5] in
  let ls = Track_helpers.make_test_liveset
      ~main:(make_main_track ~automations:[auto] ())
      ~tracks:[]
  in
  let ctx = Ctx.of_liveset ls in
  (match Ctx.automation ctx ~track_id:0 ~id:2 with
   | Some a -> check int "automation target" 8 a.Automation.target
   | None -> fail "expected main automation 2 via sentinel scope");
  (match Ctx.envelope_event ctx ~track_id:0 ~automation_id:2 ~id:5 with
   | Some e -> check bool "event time" true (e.Automation.EnvelopeEvent.time = 1.5)
   | None -> fail "expected event 5");
  check bool "same id absent in regular scope" true
    (Ctx.automation ctx ~track_id:1 ~id:2 = None)

(* [of_track_list] is [of_liveset] restricted to an explicit list. *)
let test_of_track_list_matches_of_liveset () =
  let track = make_midi_track 1 "T" in
  let via_list = Ctx.of_track_list ~main:None [Track.Midi track] in
  let via_liveset = Ctx.of_liveset
      (Track_helpers.make_test_liveset ~main:(make_main_track ())
         ~tracks:[Track.Midi track])
  in
  check bool "track resolves in both" true
    ((Ctx.track via_list ~id:1 <> None) && (Ctx.track via_liveset ~id:1 <> None));
  check bool "no main via list" true (Ctx.main_track via_list = None);
  check bool "main via liveset" true (Ctx.main_track via_liveset <> None)

(* First-occurrence-wins tie-break: when a regular track and a return share
   an id (impossible in Live-authored data — track ids are globally unique),
   the first indexed track wins; the return must not silently shadow it
   ([Hashtbl.add] alone would resolve last-wins). *)
let test_shared_id_first_occurrence_wins () =
  let ctx = Ctx.of_track_list ~main:None
      [Track.Midi (make_midi_track 5 "Bass");
       Track.Return (make_return_track 5 "Return A");
       Track.Return (make_return_track 6 "Return B")]
  in
  (match Ctx.track ctx ~id:5 with
   | Some (Track.Midi t) -> check string "shared id: first track wins" "Bass" t.Track.MidiTrack.name
   | _ -> fail "expected first-occurring Midi track 5, not the shadowing Return");
  (match Ctx.track ctx ~id:6 with
   | Some (Track.Return _) -> ()
   | _ -> fail "expected Return 6")

let () =
  run "ProjectorContext" [
    "Ctx", [
      test_case "empty ctx: all lookups none" `Quick test_empty_ctx_all_lookups_none;
      test_case "resolves tracks, clips, notes with memoization" `Quick
        test_resolves_tracks_clips_notes;
      test_case "return track and main context" `Quick
        test_return_track_and_main_context;
      test_case "main automation via id-0 sentinel scope" `Quick
        test_main_automation_and_event_scope;
      test_case "of_track_list matches of_liveset" `Quick
        test_of_track_list_matches_of_liveset;
      test_case "shared id: first occurrence wins" `Quick
        test_shared_id_first_occurrence_wins;
    ];
  ]
