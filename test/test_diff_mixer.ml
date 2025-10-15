open Alsdiff_lib_base
open Alsdiff_lib_live.Track
open Alsdiff_lib_diff
open Alsdiff_lib_output

let test_mixer_diff () =
  (* Load the old mixer XML *)
  let (_, old_xml) = Xml.read_file "mixer_old.xml" in
  let old_mixer = Mixer.create old_xml in

  (* Load the new mixer XML *)
  let (_, new_xml) = Xml.read_file "mixer.xml" in
  let new_mixer = Mixer.create new_xml in

  (* Diff the mixers *)
  let patch = Track_patch.MixerPatch.diff old_mixer new_mixer in

  (* Render the diff *)
  let output = Text_output.render_mixer patch in

  (* Expected output *)
  let expected = "Mixer Patch:\n  ~ Volume changed from 0.7000 to 0.5012\n  ~ Pan changed from -0.3000 to 0.4700\n  ~ Mute changed from false to true\n  ~ Solo changed from true to false\n  Send Changes:\n    + Send to track 0 with amount 0.0003\n    - Send to track 0 with amount 0.2500\n    - Send to track 0 with amount 0.5000" in

  (* Check that the output matches expected *)
  Alcotest.(check string) "Mixer diff output" expected output

let () =
  Alcotest.run "Diff Mixer" [
    "diff-logic", [
      Alcotest.test_case "Test mixer diffing logic" `Quick test_mixer_diff;
    ];
  ]
