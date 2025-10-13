open Alsdiff_lib_base
open Alsdiff_lib_live
open Alsdiff_lib_diff

let test_audio_clip_diff () =
  let _, old_xml = Xml.read_file "audio_clip_old.xml" in
  let old_clip = Clip.AudioClip.create old_xml in

  let _, new_xml = Xml.read_file "audio_clip.xml" in
  let new_clip = Clip.AudioClip.create new_xml in

  let patch = Clip_patch.AudioClipPatch.diff old_clip new_clip in

  match patch with
  | Some p ->
    (match p.id with
     | `Modified { old; new_ } ->
       Alcotest.check Alcotest.int "id old" 18 old;
       Alcotest.check Alcotest.int "id new" 17 new_
     | _ -> Alcotest.fail "Expected modified id");

    (match p.start_time with
     | `Modified { old; new_ } ->
       Alcotest.check (Alcotest.float 0.001) "start_time old" 80.0 old;
       Alcotest.check (Alcotest.float 0.001) "start_time new" 79.5 new_
     | _ -> Alcotest.fail "Expected modified start_time");

    (match p.end_time with
     | `Modified { old; new_ } ->
       Alcotest.check (Alcotest.float 0.001) "end_time old" 101.0 old;
       Alcotest.check (Alcotest.float 0.001) "end_time new" 100.0 new_
     | _ -> Alcotest.fail "Expected modified end_time");

    (match p.loop with
     | Some loop_patch ->
       (match loop_patch.start_time with
        | `Modified { old; new_ } ->
          Alcotest.check (Alcotest.float 0.001) "loop start_time old" 30.0 old;
          Alcotest.check (Alcotest.float 0.001) "loop start_time new" 26.13179997086247 new_
        | _ -> Alcotest.fail "Expected modified loop start_time");
       (match loop_patch.end_time with
        | `Modified { old; new_ } ->
          Alcotest.check (Alcotest.float 0.001) "loop end_time old" 50.0 old;
          Alcotest.check (Alcotest.float 0.001) "loop end_time new" 46.631799970862474 new_
        | _ -> Alcotest.fail "Expected modified loop end_time");
       (match loop_patch.on with
        | `Modified { old; new_ } ->
          Alcotest.check Alcotest.bool "loop on old" true old;
          Alcotest.check Alcotest.bool "loop on new" false new_
        | _ -> Alcotest.fail "Expected modified loop on")
     | _ -> Alcotest.fail "Expected loop patch");

    (match p.signature with
     | `Modified { old; new_ } ->
       Alcotest.check Alcotest.int "signature numer old" 3 old.numer;
       Alcotest.check Alcotest.int "signature numer new" 4 new_.numer;
       Alcotest.check Alcotest.int "signature denom old" 8 old.denom;
       Alcotest.check Alcotest.int "signature denom new" 4 new_.denom
     | _ -> Alcotest.fail "Expected modified signature");

    (match p.sample_ref with
     | Some sample_ref_patch ->
       (match sample_ref_patch.file_path with
        | `Modified { old; new_ } ->
          Alcotest.check Alcotest.string "sample_ref file_path old" "/Users/krfantasy/Desktop/Prelude/Thick Air Project/Samples/Processed/Crop/Metal Sheet_old.wav" old;
          Alcotest.check Alcotest.string "sample_ref file_path new" "/Users/krfantasy/Desktop/Prelude/Thick Air Project/Samples/Processed/Crop/Metal Sheet [2022-04-27 164454].wav" new_
        | _ -> Alcotest.fail "Expected modified sample_ref file_path");
       (match sample_ref_patch.crc with
        | `Modified { old; new_ } ->
          Alcotest.check Alcotest.string "sample_ref crc old" "12345" old;
          Alcotest.check Alcotest.string "sample_ref crc new" "48320" new_
        | _ -> Alcotest.fail "Expected modified sample_ref crc");
       (match sample_ref_patch.last_modified_date with
        | `Modified { old; new_ } ->
          Alcotest.check Alcotest.string "sample_ref last_modified_date old" (Int64.to_string 1742403846L) (Int64.to_string old);
          Alcotest.check Alcotest.string "sample_ref last_modified_date new" (Int64.to_string 1742403845L) (Int64.to_string new_)
        | _ -> Alcotest.fail "Expected modified sample_ref last_modified_date")
     | _ -> Alcotest.fail "Expected sample_ref patch")

  | None -> Alcotest.fail "No patch generated"

let () =
  Alcotest.run "Diff AudioClip" [
    "diff-logic",
    [ Alcotest.test_case "Test audio clip diffing logic" `Quick test_audio_clip_diff ];
  ]
