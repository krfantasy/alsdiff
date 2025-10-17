open Alsdiff_base.Xml
open Test_utils.Utils

let test_read_string () =
  let xml_str = "<root><a id=\"1\"><b>hello</b><c val=\"test\"/></a></root>" in
  let parsed = read_string xml_str in

  (* For this test, we'll verify that parsing produces the expected string representation *)
  (* We use string comparison instead of structural equality to avoid circular parent reference issues *)
  let expected_str = "<root><a id=\"1\"><b>hello</b><c val=\"test\"></c></a></root>" in
  let actual_str = xml_to_string parsed in

  Alcotest.(check string) "read_string" expected_str actual_str

let () =
  Alcotest.run "Xml" [
    "read_string",
    [
      Alcotest.test_case "parse XML string" `Quick test_read_string
    ]
  ]
