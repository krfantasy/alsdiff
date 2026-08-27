open Alcotest
open Alsdiff_base.Xml
open Alsdiff_base.Diff
open Alsdiff_live
open Alsdiff_live.Clip
open Alsdiff_output.View_model

(* ========== Helper Functions ========== *)

(* Helper: Check if a view is a Field and return it *)
let get_field = function
  | Field f -> f
  | Item _ -> failwith "Expected Field view, got Item"
  | Collection _ -> failwith "Expected Field view, got Collection"

(* Helper: Check if a view is an Item and return it *)
let get_item = function
  | Item i -> i
  | Field _ -> failwith "Expected Item view, got Field"
  | Collection _ -> failwith "Expected Item view, got Collection"

(* Helper: Check if a view is a Collection and return it *)
let get_collection = function
  | Collection c -> c
  | Field _ -> failwith "Expected Collection view, got Field"
  | Item _ -> failwith "Expected Collection view, got Item"

(* Helper: Find a sub-view by name *)
let find_view_by_name name views =
  try
    List.find (fun v ->
        match v with
        | Field f -> f.name = name
        | Item i -> i.name = name
        | Collection c -> c.name = name
      ) views
  with Not_found -> failwith ("View with name '" ^ name ^ "' not found")

(* Helper: Find item in collection items by checking name prefix *)
let find_item_in_collection name (col : collection) =
  try
    List.find (fun v ->
        match v with
        | Item i -> String.length i.name >= String.length name &&
                    String.sub i.name 0 (String.length name) = name
        | _ -> false
      ) col.items |> get_item
  with Not_found -> failwith ("Item starting with '" ^ name ^ "' not found in collection")


(* ========== ViewBuilder Module Tests ========== *)

let test_change_type_of () =
  (* Test Added *)
  let added_change = `Added 42 in
  check (Alcotest.of_pp (fun fmt ct ->
      Fmt.pf fmt "%s" (match ct with Added -> "Added" | Removed -> "Removed" | Modified -> "Modified" | Unchanged -> "Unchanged")
    )) "Added change" Added (ViewBuilder.change_type_of added_change);

  (* Test Removed *)
  let removed_change = `Removed "hello" in
  check (Alcotest.of_pp (fun fmt ct ->
      Fmt.pf fmt "%s" (match ct with Added -> "Added" | Removed -> "Removed" | Modified -> "Modified" | Unchanged -> "Unchanged")
    )) "Removed change" Removed (ViewBuilder.change_type_of removed_change);

  (* Test Modified *)
  let modified_change = `Modified { oldval = 1; newval = 2 } in
  check (Alcotest.of_pp (fun fmt ct ->
      Fmt.pf fmt "%s" (match ct with Added -> "Added" | Removed -> "Removed" | Modified -> "Modified" | Unchanged -> "Unchanged")
    )) "Modified change" Modified (ViewBuilder.change_type_of modified_change);

  (* Test Unchanged *)
  let unchanged_change = `Unchanged in
  check (Alcotest.of_pp (fun fmt ct ->
      Fmt.pf fmt "%s" (match ct with Added -> "Added" | Removed -> "Removed" | Modified -> "Modified" | Unchanged -> "Unchanged")
    )) "Unchanged change" Unchanged (ViewBuilder.change_type_of unchanged_change)




(* ========== Create Function Tests ========== *)

let test_create_note_item_added () =
  let note = { MidiNote.id = 1; note = 60; time = 0.0; duration = 1.0; velocity = 100.0; off_velocity = 64.0 } in
  let change = `Added note in

  let item = create_note_item change in

  check bool "Name starts with 'Note'" true (String.starts_with ~prefix:"Note" item.name);
  check bool "Item is Added" true (item.change = Added);
  check bool "Has children" true (List.length item.children > 0);

  (* Check Time field exists *)
  let time_field = get_field (find_view_by_name "Time" item.children) in
  check bool "Time field is Added" true (time_field.change = Added);
  (match time_field.newval with
   | Some (Ffloat t) -> check (float 0.001) "Time value" 0.0 t
   | _ -> fail "Expected Ffloat for Time")


let test_create_note_item_modified () =
  let old_note = { MidiNote.id = 1; note = 60; time = 0.0; duration = 1.0; velocity = 100.0; off_velocity = 64.0 } in
  let new_note = { MidiNote.id = 1; note = 60; time = 0.5; duration = 1.5; velocity = 100.0; off_velocity = 64.0 } in

  let patch = MidiNote.diff old_note new_note in
  let change = `Modified patch in

  let item = create_note_item change in

  check bool "Item is Modified" true (item.change = Modified);

  (* Check Time field is modified *)
  let time_field = get_field (find_view_by_name "Time" item.children) in
  check bool "Time field is Modified" true (time_field.change = Modified);
  (match time_field.oldval, time_field.newval with
   | Some (Ffloat o), Some (Ffloat n) ->
     check (float 0.001) "Old time" 0.0 o;
     check (float 0.001) "New time" 0.5 n
   | _ -> fail "Expected Ffloat old and new for Time")

(* Modified notes must carry their unchanged leaf fields as Unchanged context
   (from the reference note) so consumers render the real pitch/duration/
   velocity instead of defaults; the item name carries the pitch like
   Added/Removed notes. *)
let test_modified_note_reference_context () =
  let old_note = { MidiNote.id = 7; note = 52; time = 4.0; duration = 11.0; velocity = 102.0; off_velocity = 64.0 } in
  (* only velocity changes *)
  let new_note = { old_note with velocity = 110.0 } in
  let patch = MidiNote.diff old_note new_note in
  let item = create_note_item ~reference_note:old_note (`Modified patch) in
  check string "note name includes pitch" "Note E3 (52)" item.name;
  let note_field = get_field (find_view_by_name "Note" item.children) in
  check bool "Note field is Unchanged context" true (note_field.change = Unchanged);
  (match note_field.newval with
   | Some (Fint 52) -> () | _ -> fail "Note newval expected Fint 52");
  let dur_field = get_field (find_view_by_name "Duration" item.children) in
  (match dur_field.newval with
   | Some (Ffloat 11.0) -> () | _ -> fail "Duration newval expected Ffloat 11.0");
  let vel_field = get_field (find_view_by_name "Velocity" item.children) in
  check bool "changed Velocity stays Modified" true (vel_field.change = Modified);
  (match vel_field.oldval, vel_field.newval with
   | Some (Ffloat 102.0), Some (Ffloat 110.0) -> ()
   | _ -> fail "Velocity old/new expected 102/110");
  (* Context fields come first, matching the Added/Removed field order. *)
  (match item.children with
   | Field { name = "Time"; _ } :: Field { name = "Duration"; _ } :: _ -> ()
   | _ -> fail "Time/Duration should lead the children")

(* Without a reference the Modified item keeps the old behavior (patch fields
   only) but is at least identifiable by note id. *)
let test_modified_note_no_reference_falls_back_to_id_name () =
  let old_note = { MidiNote.id = 7; note = 52; time = 4.0; duration = 11.0; velocity = 102.0; off_velocity = 64.0 } in
  let new_note = { old_note with velocity = 110.0 } in
  let patch = MidiNote.diff old_note new_note in
  let item = create_note_item (`Modified patch) in
  check string "note name falls back to id" "Note (#7)" item.name;
  check bool "no Note context field without reference" true
    (not (List.exists (function Field { name = "Note"; _ } -> true | _ -> false) item.children))


let test_create_note_item_sharp_style () =
  (* Note 54 = F#3 in sharp notation *)
  let note = { MidiNote.id = 1; note = 54; time = 0.0; duration = 1.0; velocity = 100.0; off_velocity = 64.0 } in
  let change = `Added note in

  (* Test with explicit Sharp style *)
  let item = create_note_item ~note_name_style:Sharp change in

  check string "Item name contains F#3" "Note F#3 (54)" item.name;
  check bool "Item is Added" true (item.change = Added)


let test_create_note_item_flat_style () =
  (* Note 54 = Gb3 in flat notation *)
  let note = { MidiNote.id = 1; note = 54; time = 0.0; duration = 1.0; velocity = 100.0; off_velocity = 64.0 } in
  let change = `Added note in

  let item = create_note_item ~note_name_style:Flat change in

  check string "Item name contains Gb3" "Note Gb3 (54)" item.name;
  check bool "Item is Added" true (item.change = Added)


let test_create_note_item_flat_style_ab_note () =
  (* Note 56 = Ab3 in flat notation *)
  let note = { MidiNote.id = 1; note = 56; time = 0.0; duration = 1.0; velocity = 100.0; off_velocity = 64.0 } in
  let change = `Added note in

  let item = create_note_item ~note_name_style:Flat change in

  check string "Item name contains Ab3" "Note Ab3 (56)" item.name;
  check bool "Item is Added" true (item.change = Added)


let test_create_note_item_default_is_sharp () =
  (* Verify that without specifying style, default is Sharp *)
  let note = { MidiNote.id = 1; note = 54; time = 0.0; duration = 1.0; velocity = 100.0; off_velocity = 64.0 } in
  let change = `Added note in

  let item = create_note_item change in

  check string "Default style is Sharp (F#3)" "Note F#3 (54)" item.name


let test_create_midi_clip_item () =
  (* Setup data *)
  let old_midi_note = { MidiNote.id = 1; note = 60; time = 0.0; duration = 1.0; velocity = 100.0; off_velocity = 64.0 } in
  let new_midi_note = { MidiNote.id = 1; note = 60; time = 0.5; duration = 1.0; velocity = 100.0; off_velocity = 64.0 } in

  let note_change = MidiNote.diff old_midi_note new_midi_note in
  let notes_changes = [`Modified note_change] in

  let old_numer = 4 in
  let new_numer = 3 in
  let signature_patch = { TimeSignature.Patch.numer = `Modified { oldval = old_numer; newval = new_numer }; denom = `Unchanged } in

  let loop_patch = { Loop.Patch.start_time = `Unchanged; end_time = `Unchanged; on = `Modified { oldval = false; newval = true } } in

  let clip_patch = {
    MidiClip.Patch.
    id = 1;
    name = `Modified { oldval = "Clip A"; newval = "Clip B" };
    start_time = `Unchanged;
    end_time = `Unchanged;
    loop = `Modified loop_patch;
    signature = `Modified signature_patch;
    notes = notes_changes;
  } in

  let change = `Modified clip_patch in

  (* Execute *)
  let item = create_midi_clip_item change in

  (* Verify - item name contains MidiClip *)
  check bool "Item name contains MidiClip" true (String.starts_with ~prefix:"MidiClip" item.name);

  (* Check Name field *)
  let name_view = get_field (find_view_by_name "Name" item.children) in
  check string "Name field name" "Name" name_view.name;
  (match name_view.change with
   | Modified ->
     (match name_view.oldval, name_view.newval with
      | Some (Fstring o), Some (Fstring n) ->
        check string "Old name" "Clip A" o;
        check string "New name" "Clip B" n
      | _ -> fail "Invalid values for Name field")
   | _ -> fail "Expected Name field to be Modified");

  (* Check TimeSignature item *)
  let sig_item = get_item (find_view_by_name "TimeSignature" item.children) in
  let numer_view = get_field (find_view_by_name "Numer" sig_item.children) in
  (match numer_view.change with
   | Modified ->
     (match numer_view.oldval, numer_view.newval with
      | Some (Fint o), Some (Fint n) ->
        check int "Old numer" 4 o;
        check int "New numer" 3 n
      | _ -> fail "Invalid values for Numer field")
   | _ -> fail "Expected Numer to be Modified");

  (* Check Loop item *)
  let loop_item = get_item (find_view_by_name "Loop" item.children) in
  let on_view = get_field (find_view_by_name "On" loop_item.children) in
  (match on_view.change with
   | Modified ->
     (match on_view.oldval, on_view.newval with
      | Some (Fbool o), Some (Fbool n) ->
        check bool "Old on" false o;
        check bool "New on" true n
      | _ -> fail "Invalid values for On field")
   | _ -> fail "Expected On field to be Modified");

  (* Check Notes collection *)
  let notes_collection = get_collection (find_view_by_name "Notes" item.children) in
  check int "Number of notes" 1 (List.length notes_collection.items);

  let note_item = find_item_in_collection "Note" notes_collection in
  let time_view = get_field (find_view_by_name "Time" note_item.children) in
  (match time_view.change with
   | Modified ->
     (match time_view.oldval, time_view.newval with
      | Some (Ffloat o), Some (Ffloat n) ->
        check (float 0.001) "Old note time" 0.0 o;
        check (float 0.001) "New note time" 0.5 n
      | _ -> fail "Invalid values for Note Time field")
   | _ -> fail "Expected Note Time to be Modified")


(* Inline fields must carry their parent Item's domain_type, not the PPX's
   DTOther placeholder — otherwise type_overrides match the item header but
   filter out its own inline fields. *)
let test_inline_field_inherits_parent_domain () =
  let clip_patch = {
    MidiClip.Patch.
    id = 1;
    name = `Modified { oldval = "A"; newval = "B" };
    start_time = `Unchanged;
    end_time = `Unchanged;
    loop = `Unchanged;
    signature = `Unchanged;
    notes = [];
  } in
  let item = create_midi_clip_item (`Modified clip_patch) in
  check bool "parent item keeps DTClip domain" true (item.domain_type = DTClip);
  let name_field = get_field (find_view_by_name "Name" item.children) in
  check bool "inline Name field inherits parent DTClip domain" true
    (name_field.domain_type = DTClip)


let test_create_audio_clip_item_added () =
  let sample_ref = { SampleRef.file_path = "/path/to/sample.wav"; crc = "abc123"; last_modified_date = 12345 } in
  let loop = { Loop.start_time = 0.0; end_time = 4.0; on = true } in
  let signature = { TimeSignature.numer = 4; denom = 4 } in
  let clip = {
    AudioClip.id = 1;
    name = "Audio Clip 1";
    start_time = 0.0;
    end_time = 8.0;
    loop;
    signature;
    sample_ref;
    fade = None;
  } in

  let change = `Added clip in
  let item = create_audio_clip_item change in

  check bool "Item name contains AudioClip" true (String.starts_with ~prefix:"AudioClip" item.name);
  check bool "Item is Added" true (item.change = Added);

  (* Check Name field *)
  let name_field = get_field (find_view_by_name "Name" item.children) in
  check bool "Name field is Added" true (name_field.change = Added);
  (match name_field.newval with
   | Some (Fstring s) -> check string "Clip name" "Audio Clip 1" s
   | _ -> fail "Expected Fstring for Name");

  (* Check Loop item exists *)
  let loop_item = get_item (find_view_by_name "Loop" item.children) in
  check bool "Loop item is Added" true (loop_item.change = Added);

  (* Check SampleRef item exists *)
  let sample_item = get_item (find_view_by_name "SampleRef" item.children) in
  check bool "SampleRef item is Added" true (sample_item.change = Added);

  let file_path_field = get_field (find_view_by_name "File Path" sample_item.children) in
  (match file_path_field.newval with
   | Some (Fstring s) -> check string "Sample file path" "/path/to/sample.wav" s
   | _ -> fail "Expected Fstring for File Path")

let build_automation_item_from_event_patch event_patch =
  let automation_patch = {
    Automation.Patch.id = 1;
    target = 2;
    events = [`Modified event_patch];
  } in
  create_automation_item ~get_pointee_name:(fun _ -> "Target") (`Modified automation_patch)

let get_single_event_item item =
  check int "single events collection" 1 (List.length item.children);
  let events_col = get_collection (List.hd item.children) in
  check int "single event in collection" 1 (List.length events_col.items);
  get_item (List.hd events_col.items)

let test_create_automation_item_curve_added_summary () =
  let event_patch = {
    Automation.EnvelopeEvent.Patch.id = 42;
    time = `Modified { oldval = 1.0; newval = 2.0 };
    value = `Modified { oldval = Automation.FloatEvent 10.0; newval = Automation.FloatEvent 11.0 };
    curve = `Added {
        Automation.CurveControls.curve1_x = 0.1;
        curve1_y = 0.2;
        curve2_x = 0.3;
        curve2_y = 0.4;
      };
  } in
  let item = build_automation_item_from_event_patch event_patch in
  let event_item = get_single_event_item item in
  check string "event name" "Event[42]" event_item.name;
  check bool "event change" true (event_item.change = Modified);
  (* Check Time field *)
  let time_field = get_field (find_view_by_name "Time" event_item.children) in
  check bool "time field change" true (time_field.change = Modified);
  (match time_field.oldval, time_field.newval with
   | Some (Ffloat o), Some (Ffloat n) ->
     check (float 0.001) "time old" 1.0 o;
     check (float 0.001) "time new" 2.0 n
   | _ -> fail "Expected Ffloat for Time");
  (* Check Value field *)
  let value_field = get_field (find_view_by_name "Value" event_item.children) in
  check bool "value field change" true (value_field.change = Modified);
  (match value_field.oldval, value_field.newval with
   | Some (Ffloat o), Some (Ffloat n) ->
     check (float 0.001) "value old" 10.0 o;
     check (float 0.001) "value new" 11.0 n
   | _ -> fail "Expected Ffloat for Value");
  (* Check Curve child item with Added curve *)
  let curve_item = get_item (find_view_by_name "Curve" event_item.children) in
  check bool "curve item change" true (curve_item.change = Modified)

let test_create_automation_item_curve_removed_summary () =
  let event_patch = {
    Automation.EnvelopeEvent.Patch.id = 42;
    time = `Modified { oldval = 1.0; newval = 2.0 };
    value = `Unchanged;
    curve = `Removed {
        Automation.CurveControls.curve1_x = 0.5;
        curve1_y = 0.6;
        curve2_x = 0.7;
        curve2_y = 0.8;
      };
  } in
  let item = build_automation_item_from_event_patch event_patch in
  let event_item = get_single_event_item item in
  check string "event name" "Event[42]" event_item.name;
  check bool "event change" true (event_item.change = Modified);
  (* Check Time field *)
  let time_field = get_field (find_view_by_name "Time" event_item.children) in
  (match time_field.oldval, time_field.newval with
   | Some (Ffloat o), Some (Ffloat n) ->
     check (float 0.001) "time old" 1.0 o;
     check (float 0.001) "time new" 2.0 n
   | _ -> fail "Expected Ffloat for Time");
  (* Check Curve child item with Removed curve *)
  let curve_item = get_item (find_view_by_name "Curve" event_item.children) in
  check bool "curve item change" true (curve_item.change = Modified)

let test_create_automation_item_curve_modified_summary () =
  let event_patch = {
    Automation.EnvelopeEvent.Patch.id = 42;
    time = `Modified { oldval = 1.0; newval = 2.0 };
    value = `Modified { oldval = Automation.FloatEvent 10.0; newval = Automation.FloatEvent 11.0 };
    curve = `Modified {
        Automation.CurveControls.Patch.curve1_x = `Modified { oldval = 0.1; newval = 0.2 };
        curve1_y = `Modified { oldval = 0.2; newval = 0.3 };
        curve2_x = `Modified { oldval = 0.3; newval = 0.4 };
        curve2_y = `Modified { oldval = 0.4; newval = 0.5 };
      };
  } in
  let item = build_automation_item_from_event_patch event_patch in
  let event_item = get_single_event_item item in
  check string "event name" "Event[42]" event_item.name;
  check bool "event change" true (event_item.change = Modified);
  (* Check Time field *)
  let time_field = get_field (find_view_by_name "Time" event_item.children) in
  (match time_field.oldval, time_field.newval with
   | Some (Ffloat o), Some (Ffloat n) ->
     check (float 0.001) "time old" 1.0 o;
     check (float 0.001) "time new" 2.0 n
   | _ -> fail "Expected Ffloat for Time");
  (* Check Value field *)
  let value_field = get_field (find_view_by_name "Value" event_item.children) in
  (match value_field.oldval, value_field.newval with
   | Some (Ffloat o), Some (Ffloat n) ->
     check (float 0.001) "value old" 10.0 o;
     check (float 0.001) "value new" 11.0 n
   | _ -> fail "Expected Ffloat for Value");
  (* Check Curve child item with Modified curve sub-fields *)
  let curve_item = get_item (find_view_by_name "Curve" event_item.children) in
  check bool "curve item change" true (curve_item.change = Modified);
  let c1x = get_field (find_view_by_name "Curve1 X" curve_item.children) in
  (match c1x.oldval, c1x.newval with
   | Some (Ffloat o), Some (Ffloat n) ->
     check (float 0.001) "c1x old" 0.1 o;
     check (float 0.001) "c1x new" 0.2 n
   | _ -> fail "Expected Ffloat for Curve1 X")

(* A curve-only Modified event must still carry Time/Value as Unchanged
   context (from the reference event) so consumers can place it — the web's
   structured parser needs a Time or Value field to render the event at all
   (review I5). *)
let test_curve_only_event_carries_time_value_context () =
  let curve = {
    Automation.CurveControls.curve1_x = 0.1;
    curve1_y = 0.2;
    curve2_x = 0.8;
    curve2_y = 0.9;
  } in
  let old_curve = { curve with Automation.CurveControls.curve1_x = 0.0 } in
  let old_event = {
    Automation.EnvelopeEvent.id = 42;
    time = 16.0;
    value = Automation.FloatEvent 0.5;
    curve = Some old_curve;
  } in
  let new_event = { old_event with curve = Some curve } in
  let ev_patch = Automation.EnvelopeEvent.diff old_event new_event in
  (* the patch's time/value are Unchanged; only curve moved *)
  let item = create_events_item ~reference_event:old_event (`Modified ev_patch) in
  let time_field = get_field (find_view_by_name "Time" item.children) in
  check bool "Time context is Unchanged" true (time_field.change = Unchanged);
  (match time_field.newval with
   | Some (Ffloat 16.0) -> () | _ -> fail "Time context expected 16.0");
  let value_field = get_field (find_view_by_name "Value" item.children) in
  check bool "Value context is Unchanged" true (value_field.change = Unchanged);
  (match value_field.newval with
   | Some (Ffloat 0.5) -> () | _ -> fail "Value context expected 0.5");
  (* Curve child still present and Modified *)
  check bool "Curve child present" true
    (List.exists (function Item { name = "Curve"; _ } -> true | _ -> false) item.children)

(* Added/Removed automations must render their events, wrapped in an Events
   Collection (review_0614.org [#B], change_projector.ml:837). *)
let build_automation_item_added () =
  let automation = {
    Automation.id = 1;
    target = 2;
    events = [{
        Automation.EnvelopeEvent.id = 42;
        time = 1.0;
        value = Automation.FloatEvent 10.0;
        curve = None;
      }];
  } in
  create_automation_item ~get_pointee_name:(fun _ -> "Target") (`Added automation)

let build_automation_item_removed () =
  let automation = {
    Automation.id = 1;
    target = 2;
    events = [{
        Automation.EnvelopeEvent.id = 42;
        time = 1.0;
        value = Automation.FloatEvent 10.0;
        curve = None;
      }];
  } in
  create_automation_item ~get_pointee_name:(fun _ -> "Target") (`Removed automation)

let test_create_automation_item_added_event_summary () =
  let item = build_automation_item_added () in
  check string "automation name" "Automation (id=1, target=Target)" item.name;
  check bool "automation change is Added" true (item.change = Added);
  let event_item = get_single_event_item item in
  check string "event uses real id" "Event[42]" event_item.name;
  check bool "event change is Added" true (event_item.change = Added);
  let time_field = get_field (find_view_by_name "Time" event_item.children) in
  check bool "time field change is Added" true (time_field.change = Added);
  check bool "time field oldval is None" true (time_field.oldval = None);
  (match time_field.newval with
   | Some (Ffloat n) -> check (float 0.001) "time new" 1.0 n
   | _ -> fail "Expected Ffloat for Time");
  let value_field = get_field (find_view_by_name "Value" event_item.children) in
  check bool "value field change is Added" true (value_field.change = Added);
  (match value_field.newval with
   | Some (Ffloat n) -> check (float 0.001) "value new" 10.0 n
   | _ -> fail "Expected Ffloat for Value")

let test_create_automation_item_removed_event_summary () =
  let item = build_automation_item_removed () in
  check string "automation name" "Automation (id=1, target=Target)" item.name;
  check bool "automation change is Removed" true (item.change = Removed);
  let event_item = get_single_event_item item in
  check string "event uses real id" "Event[42]" event_item.name;
  check bool "event change is Removed" true (event_item.change = Removed);
  let time_field = get_field (find_view_by_name "Time" event_item.children) in
  check bool "time field change is Removed" true (time_field.change = Removed);
  check bool "time field newval is None" true (time_field.newval = None);
  (match time_field.oldval with
   | Some (Ffloat o) -> check (float 0.001) "time old" 1.0 o
   | _ -> fail "Expected Ffloat for Time")

let test_create_liveset_item_with_main_only_change () =
  let path = Utils.resolve_test_data_path "t4.xml" in
  let xml = read_file path in
  let liveset1 = Liveset.create xml path in
  let liveset2 = Liveset.create xml path in
  let updated_main =
    match liveset2.Liveset.main with
    | Track.Main main_track ->
      let updated_tempo =
        { main_track.Track.MainTrack.mixer.tempo with value = Device.Float 128.0 }
      in
      Track.Main {
        main_track with
        mixer = { main_track.Track.MainTrack.mixer with tempo = updated_tempo };
      }
    | _ -> fail "Expected Track.Main type for main track"
  in
  let patch = Liveset.diff liveset1 { liveset2 with main = updated_main } in
  let item = create_liveset_item (`Modified patch) in
  (* After the Main Track outer wrapper was removed, the inner item appears
     directly as a child — no need to descend a section. *)
  let main_track_item = get_item (find_view_by_name "MainTrack: Main" item.children) in

  check bool "main track rendered flat under liveset" true
    (String.starts_with ~prefix:"MainTrack: Main" main_track_item.name)

(* Unchanged master-mixer params must be populated from the reference liveset
   so consumers can read the current tempo/time signature even when only part
   of the master mixer changed (mirrors the track-mixer population). *)
let test_reference_populates_unchanged_main_mixer_params () =
  let path = Utils.resolve_test_data_path "t4.xml" in
  let xml = read_file path in
  let liveset1 = Liveset.create xml path in
  let liveset2 = Liveset.create xml path in
  let updated_main =
    match liveset2.Liveset.main with
    | Track.Main main_track ->
      let tempo = { main_track.Track.MainTrack.mixer.tempo with value = Device.Float 138.0 } in
      Track.Main { main_track with mixer = { main_track.Track.MainTrack.mixer with tempo } }
    | _ -> fail "Expected Track.Main type for main track"
  in
  let patch = Liveset.diff liveset1 { liveset2 with main = updated_main } in
  let item = create_liveset_item ~reference_liveset:liveset1 (`Modified patch) in
  let main_item = get_item (find_view_by_name "MainTrack: Main" item.children) in
  let mixer = get_item (find_view_by_name "Mixer" main_item.children) in
  (* Tempo changed: Modified Value field with old/new. *)
  let tempo = get_item (find_view_by_name "Tempo" mixer.children) in
  let tempo_value = get_field (find_view_by_name "Value" tempo.children) in
  check bool "Tempo Value is Modified" true (tempo_value.change = Modified);
  (match tempo_value.oldval, tempo_value.newval with
   | Some (Ffloat 120.0), Some (Ffloat 138.0) -> ()
   | _ -> check bool "Tempo old/new are 120/138" true false);
  (* Time signature unchanged: placeholder populated with the code (t4: 201 = 4/4). *)
  let ts = get_item (find_view_by_name "Time Signature" mixer.children) in
  (match List.find_opt (function Field { name = "Value"; _ } -> true | _ -> false) ts.children with
      | Some (Field { change = Unchanged; newval = Some (Fint 201); _ }) -> ()
      | _ -> check bool "Time Signature Value populated as Unchanged 201" true false)

(* The master item must be emitted (with populated mixer context) even when
   the main track itself is unchanged, so consumers can read the project's
   tempo/time signature when only regular tracks changed. *)
let test_unchanged_main_track_emitted_with_reference () =
  let path = Utils.resolve_test_data_path "t4.xml" in
  let xml = read_file path in
  let ls1 = Liveset.create xml path in
  (* Change only the first regular track's mixer volume; main stays untouched. *)
  let ls2 = match ls1.Liveset.tracks with
    | Track.Midi t :: rest ->
      let mixer = t.Track.MidiTrack.mixer in
      let vol = { mixer.Track.Mixer.volume with value = Device.Float 0.9 } in
      { ls1 with Liveset.tracks =
                   Track.Midi { t with Track.MidiTrack.mixer = { mixer with Track.Mixer.volume = vol } } :: rest }
    | _ -> fail "expected MidiTrack first in t4.xml"
  in
  let patch = Liveset.diff ls1 ls2 in
  let item = create_liveset_item ~reference_liveset:ls1 (`Modified patch) in
  let main_item =
    List.find_opt (function
        | Item i -> String.starts_with ~prefix:"MainTrack" i.name
        | _ -> false) item.children
    |> Option.map (function Item i -> i | _ -> assert false)
  in
  check bool "unchanged main track emitted with reference" true (main_item <> None);
  (match main_item with
   | None -> ()
   | Some mi ->
     check bool "main item is Unchanged" true (mi.change = Unchanged);
     let mixer = get_item (find_view_by_name "Mixer" mi.children) in
     (match List.find_opt (function
          | Item { name = "Tempo"; _ } -> true | _ -> false) mixer.children with
      | Some (Item { children; _ }) ->
        (match List.find_opt (function Field { name = "Value"; _ } -> true | _ -> false) children with
            | Some (Field { change; newval; _ }) ->
              check bool "Tempo context is Unchanged" true (change = Unchanged);
              (match newval with Some (Ffloat 120.0) -> () | _ -> fail "Tempo context expected 120.0")
            | _ -> fail "Tempo Value context missing")
      | _ -> fail "Tempo placeholder missing");
     (* The Unchanged master carries MIXER context only: restamping the whole
        reference subtree would materialize its Automations/Devices (t4's
        master Limiter with all params) as pseudo-context. *)
     let has_child name =
       List.exists (function
           | Item { name = n; _ } | Collection { name = n; _ } -> n = name
           | _ -> false) mi.children
     in
     check bool "no Automations context on unchanged master" true (not (has_child "Automations"));
     check bool "no Devices context on unchanged master" true (not (has_child "Devices")))

(* When only part of the liveset changes, an unchanged nested section (here
   Version) emits a placeholder {change=Unchanged} item from the projector so
   JSON/web consumers see the node (restoring Mixer/Routing/Send data lost by
   the e9d4b96 "drop leaked placeholder" change). Under verbose, the TEXT
   renderer shows the unchanged header ("= Version") — consistent with the
   verbose contract of showing everything. *)
let test_unchanged_section_placeholder_handling () =
  let path = Utils.resolve_test_data_path "t4.xml" in
  let xml = read_file path in
  let liveset1 = Liveset.create xml path in
  let liveset2 = Liveset.create xml path in
  let updated_main =
    match liveset2.Liveset.main with
    | Track.Main main_track ->
      let updated_tempo =
        { main_track.Track.MainTrack.mixer.tempo with value = Device.Float 128.0 }
      in
      Track.Main {
        main_track with
        mixer = { main_track.Track.MainTrack.mixer with tempo = updated_tempo };
      }
    | _ -> fail "Expected Track.Main type for main track"
  in
  (* Only the main track's tempo changes, so version is `Unchanged in the patch. *)
  let patch = Liveset.diff liveset1 { liveset2 with main = updated_main } in
  let item = create_liveset_item (`Modified patch) in
  (* 1. Projector DOES emit the placeholder (so JSON/web can show the node). *)
  let has_unchanged_version =
    List.exists (fun v ->
        match v with
        | Item i -> i.name = "Version" && i.change = Unchanged
        | _ -> false) item.children
  in
  check bool "projector emits unchanged Version placeholder" true
    has_unchanged_version;
  (* 2. TEXT renderer emits the unchanged Version header under verbose —
     "verbose" means show everything including unchanged (matches
     test_unchanged_full_visible: a bare Unchanged item renders as "= Name"). *)
  let text = Alsdiff_output.Text_renderer.render Alsdiff_output.Config.verbose [ Item item ] in
  let lines = String.split_on_char '\n' text in
  let shows_version =
    List.exists (fun line -> String.trim line = "= Version") lines
  in
  check bool "text renderer shows unchanged Version header under verbose" true shows_version;
  (* 3. JSON renderer emits the unchanged Version node. *)
  let json = Alsdiff_output.Json_renderer.render Alsdiff_output.Config.verbose [ Item item ] in
  let has_version_node =
    let needle = "\"name\": \"Version\"" in
    let rec search i =
      if i + String.length needle > String.length json then false
      else if String.sub json i (String.length needle) = needle then true
      else search (i + 1)
    in
    search 0
  in
  check bool "json renderer includes unchanged Version node" true has_version_node

(* The LiveSet item must carry the project's current tempo/time signature as
   Unchanged context fields (mirroring the TrackId/GroupId identity fields) so
   the web ruler reads them at EVERY detail level — including Summary/Compact
   presets where the whole MainTrack item is level-dropped. With a reference
   liveset, the values come from the reference main track. *)
let test_liveset_carries_tempo_context_with_reference () =
  let path = Utils.resolve_test_data_path "t4.xml" in
  let xml = read_file path in
  let ls1 = Liveset.create xml path in
  (* Change only the first regular track's mixer volume; main stays untouched. *)
  let ls2 = match ls1.Liveset.tracks with
    | Track.Midi t :: rest ->
      let mixer = t.Track.MidiTrack.mixer in
      let vol = { mixer.Track.Mixer.volume with value = Device.Float 0.9 } in
      { ls1 with Liveset.tracks =
                   Track.Midi { t with Track.MidiTrack.mixer = { mixer with Track.Mixer.volume = vol } } :: rest }
    | _ -> fail "expected MidiTrack first in t4.xml"
  in
  let patch = Liveset.diff ls1 ls2 in
  let item = create_liveset_item ~reference_liveset:ls1 (`Modified patch) in
  let tempo = get_field (find_view_by_name "Tempo" item.children) in
  check bool "liveset Tempo context is Unchanged" true (tempo.change = Unchanged);
  (match tempo.newval with
   | Some (Ffloat 120.0) -> () | _ -> fail "liveset Tempo context expected 120.0");
  let ts = get_field (find_view_by_name "Time Signature" item.children) in
  check bool "liveset Time Signature context is Unchanged" true (ts.change = Unchanged);
  (match ts.newval with
   | Some (Fint 201) -> () | _ -> fail "liveset Time Signature context expected 201")

(* When the master tempo changes, the LiveSet context field carries the NEW
   value from the patch — no reference needed. *)
let test_liveset_tempo_context_from_patch () =
  let path = Utils.resolve_test_data_path "t4.xml" in
  let xml = read_file path in
  let liveset1 = Liveset.create xml path in
  let liveset2 = Liveset.create xml path in
  let updated_main =
    match liveset2.Liveset.main with
    | Track.Main main_track ->
      let tempo = { main_track.Track.MainTrack.mixer.tempo with value = Device.Float 138.0 } in
      Track.Main { main_track with mixer = { main_track.Track.MainTrack.mixer with tempo } }
    | _ -> fail "Expected Track.Main type for main track"
  in
  let patch = Liveset.diff liveset1 { liveset2 with main = updated_main } in
  let item = create_liveset_item (`Modified patch) in
  let tempo = get_field (find_view_by_name "Tempo" item.children) in
  (match tempo.newval with
   | Some (Ffloat 138.0) -> ()
   | _ -> fail "liveset Tempo context expected 138.0 from patch");
  (* Time signature did not change and no reference is available -> absent. *)
  check bool "no liveset Time Signature context without reference" true
    (not (List.exists (function
         | Field { name = "Time Signature"; _ } -> true | _ -> false) item.children))

(* Without a reference and an untouched master there is nothing to read: no
   context fields are emitted. *)
let test_liveset_no_tempo_context_without_reference () =
  let path = Utils.resolve_test_data_path "t4.xml" in
  let xml = read_file path in
  let ls1 = Liveset.create xml path in
  let ls2 = match ls1.Liveset.tracks with
    | Track.Midi t :: rest ->
      let mixer = t.Track.MidiTrack.mixer in
      let vol = { mixer.Track.Mixer.volume with value = Device.Float 0.9 } in
      { ls1 with Liveset.tracks =
                   Track.Midi { t with Track.MidiTrack.mixer = { mixer with Track.Mixer.volume = vol } } :: rest }
    | _ -> fail "expected MidiTrack first in t4.xml"
  in
  let patch = Liveset.diff ls1 ls2 in
  let item = create_liveset_item (`Modified patch) in
  check bool "no liveset Tempo context without reference" true
    (not (List.exists (function
         | Field { name = "Tempo"; _ } -> true | _ -> false) item.children));
  check bool "no liveset Time Signature context without reference" true
    (not (List.exists (function
         | Field { name = "Time Signature"; _ } -> true | _ -> false) item.children))

(* A self-diff (`` `Unchanged `` liveset) carries no tempo context: the web
   shows the no-differences result and never reads the ruler. *)
let test_liveset_no_tempo_context_when_unchanged () =
  let path = Utils.resolve_test_data_path "t4.xml" in
  let xml = read_file path in
  let ls1 = Liveset.create xml path in
  let item = create_liveset_item ~reference_liveset:ls1 `Unchanged in
  check bool "no liveset Tempo context on Unchanged" true
    (not (List.exists (function
         | Field { name = "Tempo"; _ } -> true | _ -> false) item.children));
  check bool "no liveset Time Signature context on Unchanged" true
    (not (List.exists (function
         | Field { name = "Time Signature"; _ } -> true | _ -> false) item.children))

(* When a track is Modified but its mixer is Unchanged, the projector emits an
   empty {change=Unchanged; children=[]} Mixer placeholder. With a reference
   track threaded in (the old value), the placeholder is populated from the
   reference track's mixer value (restamped Unchanged) so the web app can
   render a full mixer strip. Restores the lost 044a9a7 feature. *)
let test_reference_populates_unchanged_mixer () =
  let mixer = Track_helpers.make_mixer 0.70 (-0.30) in
  let mk_track name =
    {
      Track.MidiTrack.id = 1; name; current_name = name; group_id = -1;
      clips = []; automations = []; devices = [];
      mixer; routings = Track_helpers.make_empty_routing_set ();
    }
  in
  (* Two tracks, same id, different name -> Modified track, Unchanged mixer. *)
  let t1 = mk_track "Old" in
  let t2 = mk_track "New" in
  let patch = Track.MidiTrack.diff t1 t2 in
  let find_mixer it =
    List.find_opt (fun ch -> match ch with Item mi when mi.name = "Mixer" -> true | _ -> false) it.children
  in
  let get_pointee_name _ = "?" in
  (* WITHOUT reference: Mixer is an empty placeholder. *)
  let item_no_ref = create_midi_track_item ~get_pointee_name (`Modified patch) in
  (match find_mixer item_no_ref with
   | Some (Item mi) ->
     check bool "without reference: mixer is empty placeholder" true (mi.children = [])
   | _ -> check bool "without reference: mixer placeholder present" true false);
  (* WITH reference: Mixer is populated with Volume/Pan/Mute/Solo. *)
  let item_with_ref = create_midi_track_item ~get_pointee_name ~reference_track:t1 (`Modified patch) in
  (match find_mixer item_with_ref with
   | Some (Item mi) ->
     let names = List.filter_map (fun v ->
         match v with Item ci -> Some ci.name | _ -> None) mi.children in
     check bool "with reference: mixer has 4 children" true (List.length mi.children = 4);
     check bool "with reference: mixer has Volume child" true (List.mem "Volume" names);
     let vol = List.find_opt (fun v -> match v with Item ci when ci.name = "Volume" -> true | _ -> false) mi.children in
     (match vol with
      | Some (Item vi) ->
        check bool "with reference: Volume is Unchanged" true (vi.change = Unchanged)
      | _ -> check bool "with reference: Volume child present" true false)
   | _ -> check bool "with reference: mixer present" true false)


(* Modified tracks must carry TrackId/GroupId as Unchanged context fields so
   consumers (the web app) can nest them under their group track: grouping is
   by these fields and the item name only encodes the track id, never the
   group. *)
let test_modified_track_identity_fields () =
  let mk_track name group_id mixer =
    {
      Track.MidiTrack.id = 14; name; current_name = name; group_id;
      clips = []; automations = []; devices = [];
      mixer; routings = Track_helpers.make_empty_routing_set ();
    }
  in
  (* Same id -> Modified; group_id identical on both sides; mixer volume differs. *)
  let t1 = mk_track "Old" 91 (Track_helpers.make_mixer 0.70 (-0.30)) in
  let t2 = mk_track "New" 91 (Track_helpers.make_mixer 0.80 (-0.30)) in
  let patch = Track.MidiTrack.diff t1 t2 in
  let item = create_midi_track_item ~get_pointee_name:(fun _ -> "?") ~reference_track:t1 (`Modified patch) in
  let track_id_field = get_field (find_view_by_name "TrackId" item.children) in
  check bool "TrackId emitted for Modified track" true (track_id_field.change = Unchanged);
  (match track_id_field.newval with
   | Some (Fint 14) -> ()
   | _ -> check bool "TrackId newval is Fint 14" true false);
  let group_id_field = get_field (find_view_by_name "GroupId" item.children) in
  check bool "GroupId emitted for Modified track" true (group_id_field.change = Unchanged);
  (match group_id_field.newval with
   | Some (Fint 91) -> ()
   | _ -> check bool "GroupId newval is Fint 91" true false);
  (* Identity fields come first, matching the Added/Removed field order. *)
  (match item.children with
   | Field { name = "TrackId"; _ } :: Field { name = "GroupId"; _ } :: _ -> ()
   | _ -> check bool "TrackId/GroupId are the first children" true false)

(* When the group_id actually changes, the patch path emits the Modified
   GroupId field; the identity injection must not duplicate it. *)
let test_modified_track_group_change_no_duplicate () =
  let mk_track group_id =
    {
      Track.MidiTrack.id = 14; name = "Lead"; current_name = "Lead"; group_id;
      clips = []; automations = []; devices = [];
      mixer = Track_helpers.make_mixer 0.70 (-0.30);
      routings = Track_helpers.make_empty_routing_set ();
    }
  in
  let t1 = mk_track 91 in
  let t2 = mk_track (-1) in
  let patch = Track.MidiTrack.diff t1 t2 in
  let item = create_midi_track_item ~get_pointee_name:(fun _ -> "?") ~reference_track:t1 (`Modified patch) in
  let group_fields =
    List.filter (function
        | Field { name = "GroupId"; _ } -> true | _ -> false) item.children
  in
  check int "exactly one GroupId field" 1 (List.length group_fields);
  let f = get_field (List.hd group_fields) in
  check bool "GroupId field is Modified (patch path, not injected)" true (f.change = Modified);
  (match f.oldval, f.newval with
   | Some (Fint 91), Some (Fint (-1)) -> ()
   | _ -> check bool "GroupId old/new are 91/-1" true false)


let test_create_locator_item_added () =
  let loc : Liveset.Locator.t = { id = 7; name = "Verse"; time = 4.0 } in
  let item = create_locator_item (`Added loc) in
  check string "Name includes id" "Locator (id=7)" item.name;
  check bool "Item is Added" true (item.change = Added);
  let id_field = get_field (find_view_by_name "Id" item.children) in
  check bool "Id field is Added" true (id_field.change = Added)


let test_create_locator_item_modified () =
  let old_loc : Liveset.Locator.t = { id = 7; name = "Verse"; time = 4.0 } in
  let new_loc : Liveset.Locator.t = { id = 7; name = "Chorus"; time = 8.0 } in
  let patch = Liveset.Locator.diff old_loc new_loc in
  let item = create_locator_item (`Modified patch) in
  check string "Name includes id" "Locator (id=7)" item.name;
  check bool "Item is Modified" true (item.change = Modified);
  let name_field = get_field (find_view_by_name "Name" item.children) in
  check bool "Name field is Modified" true (name_field.change = Modified)


let test_build_liveset_track_sections_added () =
  let path = Utils.resolve_test_data_path "t4.xml" in
  let xml = read_file path in
  let ls = Liveset.create xml path in
  let tracks = build_liveset_tracks_items ~get_pointee_name:(fun _ -> "?") (`Added ls) in
  let returns = build_liveset_returns_items ~get_pointee_name:(fun _ -> "?") (`Added ls) in
  (* t4.xml has 1 MidiTrack + 1 AudioTrack; Main/Return excluded by the regular-track filter. *)
  check int "renders 2 regular tracks (Midi+Audio)" 2 (List.length tracks);
  check int "renders 0 returns" 0 (List.length returns)


let () =
  run "ViewModel" [
    "ViewBuilder.change_type_of", [
      test_case "Extracts change type correctly" `Quick test_change_type_of;
    ];

    "create_note_item", [
      test_case "Create note item for Added" `Quick test_create_note_item_added;
      test_case "Create note item for Modified" `Quick test_create_note_item_modified;
      test_case "Modified note carries reference context" `Quick test_modified_note_reference_context;
      test_case "Modified note without reference falls back to id name" `Quick test_modified_note_no_reference_falls_back_to_id_name;
    ];
    "create_note_item note_name_style", [
      test_case "Sharp style produces sharp notes" `Quick test_create_note_item_sharp_style;
      test_case "Flat style produces flat notes" `Quick test_create_note_item_flat_style;
      test_case "Flat style for Ab note" `Quick test_create_note_item_flat_style_ab_note;
      test_case "Default style is Sharp" `Quick test_create_note_item_default_is_sharp;
    ];
    "create_midi_clip_item", [
      test_case "Create item from patch" `Quick test_create_midi_clip_item;
      test_case "Inline field inherits parent domain" `Quick
        test_inline_field_inherits_parent_domain;
    ];
    "create_audio_clip_item", [
      test_case "Create item for Added clip" `Quick test_create_audio_clip_item_added;
    ];
    "create_automation_item", [
      test_case "Combined summary includes added curve details" `Quick test_create_automation_item_curve_added_summary;
      test_case "Combined summary includes removed curve details" `Quick test_create_automation_item_curve_removed_summary;
      test_case "Combined summary includes modified curve details" `Quick test_create_automation_item_curve_modified_summary;
      test_case "Added automation renders its events" `Quick test_create_automation_item_added_event_summary;
      test_case "Removed automation renders its events" `Quick test_create_automation_item_removed_event_summary;
    ];
    "create_events_item", [
      test_case "Curve-only Modified event carries Time/Value context" `Quick
        test_curve_only_event_carries_time_value_context;
    ];
    "create_locator_item", [
      test_case "Create locator item for Added" `Quick test_create_locator_item_added;
      test_case "Create locator item for Modified" `Quick test_create_locator_item_modified;
    ];
    "build_liveset_sections", [
      test_case "Added liveset renders regular tracks and returns" `Quick
        test_build_liveset_track_sections_added;
    ];
    "create_liveset_item", [
      test_case "Renders main track when it is the only change" `Quick test_create_liveset_item_with_main_only_change;
      test_case "Reference populates unchanged main mixer params" `Quick test_reference_populates_unchanged_main_mixer_params;
      test_case "Unchanged main track emitted with reference" `Quick test_unchanged_main_track_emitted_with_reference;
      test_case "Unchanged section placeholder handling" `Quick test_unchanged_section_placeholder_handling;
      test_case "LiveSet carries tempo context from reference" `Quick test_liveset_carries_tempo_context_with_reference;
      test_case "LiveSet carries new tempo from patch" `Quick test_liveset_tempo_context_from_patch;
      test_case "No tempo context without reference" `Quick test_liveset_no_tempo_context_without_reference;
      test_case "No tempo context on Unchanged liveset" `Quick test_liveset_no_tempo_context_when_unchanged;
      test_case "Reference liveset populates unchanged mixer" `Quick test_reference_populates_unchanged_mixer;
      test_case "Modified track carries identity fields" `Quick test_modified_track_identity_fields;
      test_case "Changed group emits single Modified GroupId" `Quick test_modified_track_group_change_no_duplicate;
    ];
  ]
