open Alsdiff_base.Xml
open Alsdiff_base.Upath
open Test_utils.Utils

let sample_xml =
  Element {
    name = "root";
    attrs = [];
    childs = [
      Element {
        name = "parent1";
        attrs = [];
        childs = [
          Element { name = "child"; attrs = []; childs = [Data {value = "direct_child"; parent = None}]; parent = None }
        ];
        parent = None;
      };
      Element {
        name = "parent2";
        attrs = [];
        childs = [
          Element {
            name = "intermediate";
            attrs = [];
            childs = [
              Element { name = "child"; attrs = []; childs = [Data {value = "deep_child"; parent = None}]; parent = None }
            ];
            parent = None;
          }
        ];
        parent = None;
      }
    ];
    parent = None;
  }

let wildcard_testable = Alcotest.(list (pair string xml_testable))

let sort_results l =
  List.sort (fun (p1, x1) (p2, x2) ->
    let c = String.compare p1 p2 in
    if c <> 0 then c
    else String.compare (xml_to_string x1) (xml_to_string x2)
  ) l

let test_wildcard_behavior path_str expected () =
  let result = find_all path_str sample_xml |> sort_results in
  let expected = expected |> sort_results in
  Alcotest.check wildcard_testable ("wildcard test " ^ path_str) expected result

let test_cases =
  [
    ("/root/*/child",
      [
        ("/root/parent1/child", Element { name = "child"; attrs = []; childs = [Data {value = "direct_child"; parent = None}]; parent = None });
      ]);
    ("/root/**/child",
      [
        ("/root/parent1/child", Element { name = "child"; attrs = []; childs = [Data {value = "direct_child"; parent = None}]; parent = None });
        ("/root/parent2/intermediate/child", Element { name = "child"; attrs = []; childs = [Data {value = "deep_child"; parent = None}]; parent = None });
      ]);
  ]

let () =
  Alcotest.run "Wildcard" [
    "wildcard_tests",
    List.map (fun (path, expected) ->
      Alcotest.test_case path `Quick (test_wildcard_behavior path expected)
    ) test_cases;
  ]
