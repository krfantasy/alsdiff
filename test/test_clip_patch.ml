open Alsdiff_base.Xml
open Alsdiff_live.Clip
open Alsdiff_diff.Clip_patch

let test_identical_clips () =
  (* Read the audio_clip.xml file twice *)
  let xml1 = read_file "audio_clip.xml" in
  let xml2 = read_file "audio_clip.xml" in

  (* Create two identical audio clips *)
  let clip1 = AudioClip.create xml1 in
  let clip2 = AudioClip.create xml2 in

  (* Diff them - should return None since they're identical *)
  let patch_opt = AudioClipPatch.diff clip1 clip2 in

  match patch_opt with
  | None -> () (* Expected result *)
  | Some _ -> Alcotest.fail "Expected None for identical clips"

let test_modified_start_time () =
  (* Read the audio_clip.xml file twice *)
  let xml1 = read_file "audio_clip.xml" in
  let xml2 = read_file "audio_clip.xml" in

  (* Create two audio clips *)
  let clip1 = AudioClip.create xml1 in
  let clip2 = AudioClip.create xml2 in

  (* Modify the start time of clip2 *)
  let modified_clip2 = { clip2 with start_time = clip2.start_time +. 1.0 } in

  (* Diff them - should return Some patch *)
  let patch_opt = AudioClipPatch.diff clip1 modified_clip2 in

  (* Check that we got a patch *)
  match patch_opt with
  | None -> Alcotest.fail "Expected some patch for modified start_time"
  | Some patch ->
      (* Check that start_time was modified *)
      match patch.start_time with
      | `Modified { old; new_ } ->
          Alcotest.(check (float 0.001)) "old start_time" clip1.start_time old;
          Alcotest.(check (float 0.001)) "new start_time" modified_clip2.start_time new_
      | _ -> Alcotest.fail "Expected start_time to be Modified"

let test_modified_name () =
  (* Read the audio_clip.xml file twice *)
  let xml1 = read_file "audio_clip.xml" in
  let xml2 = read_file "audio_clip.xml" in

  (* Create two audio clips *)
  let clip1 = AudioClip.create xml1 in
  let clip2 = AudioClip.create xml2 in

  (* Modify the name of clip2 *)
  let modified_clip2 = { clip2 with name = "New Name" } in

  (* Diff them - should return Some patch *)
  let patch_opt = AudioClipPatch.diff clip1 modified_clip2 in

  (* Check that we got a patch *)
  match patch_opt with
  | None -> Alcotest.fail "Expected some patch for modified name"
  | Some patch ->
      (* Check that name was modified *)
      match patch.name with
      | `Modified { old; new_ } ->
          Alcotest.(check string) "old name" clip1.name old;
          Alcotest.(check string) "new name" modified_clip2.name new_
      | _ -> Alcotest.fail "Expected name to be Modified"

let () =
  Alcotest.run "AudioClipPatch" [
    "identical_clips",
    [
      Alcotest.test_case "identical clips should have no diff" `Quick test_identical_clips
    ];
    "modified_clips",
    [
      Alcotest.test_case "modified start_time should create patch" `Quick test_modified_start_time;
      Alcotest.test_case "modified name should create patch" `Quick test_modified_name
    ]
  ]
