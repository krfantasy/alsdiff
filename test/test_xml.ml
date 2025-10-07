open Alsdiff_lib_base.Xml
open Test_utils.Utils

let test_read_string () =
  let xml_str = "<root><a id=\"1\"><b>hello</b><c val=\"test\"/></a></root>" in
  let (_, parsed) = read_string xml_str in
  let expected = Element {
    name = "root";
    attrs = [];
    childs = [
      Element {
        name = "a";
        attrs = [("id", "1")];
        childs = [
          Element { name = "b"; attrs = []; childs = [Data "hello"] };
          Element { name = "c"; attrs = [("val", "test")]; childs = [] };
        ]
      }
    ]
  } in
  Alcotest.check xml_testable "read_string" expected parsed

let () =
  Alcotest.run "Xml" [
    "read_string",
    [
      Alcotest.test_case "parse XML string" `Quick test_read_string
    ]
  ]
