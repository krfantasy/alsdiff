open Alsdiff_base
open Upath
open Test_utils

let sample_xml =
  Xml.Element {
    name = "root";
    attrs = [];
    childs = [
      Xml.Element {
        name = "a";
        attrs = [("id", "1")];
        childs = [
          Xml.Element { name = "b"; attrs = []; childs = [Xml.Data {value = "hello"; parent = None}]; parent = None };
          Xml.Element { name = "c"; attrs = [("val", "test")]; childs = []; parent = None };
        ];
        parent = None;
      };
      Xml.Element {
        name = "a";
        attrs = [("id", "2")];
        childs = [
          Xml.Element { name = "d"; attrs = []; childs = []; parent = None };
          Xml.Element {
            name = "b";
            attrs = [("lang", "en")];
            childs = [Xml.Data {value = "world"; parent = None}];
            parent = None;
          };
        ];
        parent = None;
      };
      Xml.Element {
        name = "e";
        attrs = [];
        childs = [
          Xml.Element { name = "child"; attrs = [("id", "e-child")]; childs = []; parent = None; };
          Xml.Element {
            name = "f";
            attrs = [];
            childs = [
              Xml.Element { name = "b"; attrs = []; childs = []; parent = None }
            ];
            parent = None;
          }
        ];
        parent = None;
      };
    ];
    parent = None;
  }

let rec with_parents_mut (node:Xml.t) (parent:Xml.t option) =
  match node with
  | Xml.Element element ->
    element.parent <- parent;
    List.iter (fun child -> with_parents_mut child (Some node)) element.childs
  | Xml.Data data -> data.parent <- parent

let with_parents (node:Xml.t) =
  with_parents_mut node None;
  node

let find_path_testable = Alcotest.(option (pair string Utils.xml_testable))

let test_find_path path_str expected () =
  let processed_xml = with_parents sample_xml in
  let result = find_opt path_str processed_xml in
  Alcotest.check find_path_testable ("find_path " ^ path_str) expected result

let current_parent_test_cases =
  [
    (* Basic current node navigation *)
    ("/root/a/.", Some ("/root/a",
      Xml.Element { name = "a"; attrs = [("id", "1")]; childs = [
        Xml.Element { name = "b"; attrs = []; childs = [Xml.Data {value = "hello"; parent = None}]; parent = None };
        Xml.Element { name = "c"; attrs = [("val", "test")]; childs = []; parent = None };
      ]; parent = None; }));

    (* Basic parent node navigation *)
    ("/root/a/b/..", Some ("/root/a",
      Xml.Element { name = "a"; attrs = [("id", "1")]; childs = [
        Xml.Element { name = "b"; attrs = []; childs = [Xml.Data {value = "hello"; parent = None}]; parent = None };
        Xml.Element { name = "c"; attrs = [("val", "test")]; childs = []; parent = None };
      ]; parent = None; }));

    (* Combined navigation *)
    ("/root/a/b/../c", Some ("/root/a/c",
      Xml.Element { name = "c"; attrs = [("val", "test")]; childs = []; parent = None }));

    (* Direct current node from root *)
    ("/root/.", Some ("/root",
      Xml.Element { name = "root"; attrs = []; childs = [
        Xml.Element { name = "a"; attrs = [("id", "1")]; childs = [
          Xml.Element { name = "b"; attrs = []; childs = [Xml.Data {value = "hello"; parent = None}]; parent = None };
          Xml.Element { name = "c"; attrs = [("val", "test")]; childs = []; parent = None };
        ]; parent = None; };
        Xml.Element { name = "a"; attrs = [("id", "2")]; childs = [
          Xml.Element { name = "d"; attrs = []; childs = []; parent = None };
          Xml.Element { name = "b"; attrs = [("lang", "en")]; childs = [Xml.Data {value = "world"; parent = None}]; parent = None };
        ]; parent = None; };
        Xml.Element { name = "e"; attrs = []; childs = [
          Xml.Element { name = "child"; attrs = [("id", "e-child")]; childs = []; parent = None; };
          Xml.Element { name = "f"; attrs = []; childs = [
            Xml.Element { name = "b"; attrs = []; childs = []; parent = None }
          ]; parent = None; }
        ]; parent = None; };
      ]; parent = None; }));

    (* Direct parent to root *)
    ("/root/a/..", Some ("/root",
      Xml.Element { name = "root"; attrs = []; childs = [
        Xml.Element { name = "a"; attrs = [("id", "1")]; childs = [
          Xml.Element { name = "b"; attrs = []; childs = [Xml.Data {value = "hello"; parent = None}]; parent = None };
          Xml.Element { name = "c"; attrs = [("val", "test")]; childs = []; parent = None };
        ]; parent = None; };
        Xml.Element { name = "a"; attrs = [("id", "2")]; childs = [
          Xml.Element { name = "d"; attrs = []; childs = []; parent = None };
          Xml.Element { name = "b"; attrs = [("lang", "en")]; childs = [Xml.Data {value = "world"; parent = None}]; parent = None };
        ]; parent = None; };
        Xml.Element { name = "e"; attrs = []; childs = [
          Xml.Element { name = "child"; attrs = [("id", "e-child")]; childs = []; parent = None; };
          Xml.Element { name = "f"; attrs = []; childs = [
            Xml.Element { name = "b"; attrs = []; childs = []; parent = None }
          ]; parent = None; }
        ]; parent = None; };
      ]; parent = None; }));

    (* Parent from root should fail *)
    ("/root/..", None);

    (* Multiple parent navigation *)
    ("/root/a/../..", None);

    (* Complex nested navigation *)
    ("/root/a/../a@id=\"2\"", Some ("/root/a",
      Xml.Element { name = "a"; attrs = [("id", "2")]; childs = [
        Xml.Element { name = "d"; attrs = []; childs = []; parent = None };
        Xml.Element { name = "b"; attrs = [("lang", "en")]; childs = [Xml.Data {value = "world"; parent = None}]; parent = None };
      ]; parent = None; }));

    (* Deep nested navigation *)
    ("/root/a/b/../../e/f", Some ("/root/e/f",
      Xml.Element { name = "f"; attrs = []; childs = [
        Xml.Element { name = "b"; attrs = []; childs = []; parent = None }
      ]; parent = None; }));

    (* Navigation with indexes *)
    ("/root/a[0]/b/../c", Some ("/root/a/c",
      Xml.Element { name = "c"; attrs = [("val", "test")]; childs = []; parent = None }));

    (* Parent navigation with attributes *)
    ("/root/a/b@lang=\"en\"/..", Some ("/root/a",
      Xml.Element { name = "a"; attrs = [("id", "2")]; childs = [
        Xml.Element { name = "d"; attrs = []; childs = []; parent = None };
        Xml.Element { name = "b"; attrs = [("lang", "en")]; childs = [Xml.Data {value = "world"; parent = None}]; parent = None };
      ]; parent = None; }));

    (* Nonexistent paths with parent navigation *)
    ("/root/../nonexistent", None);
    ("../a", None);
    ("/root/nonexistent/..", None);
    ("../..", None);
  ]

let find_all_testable = Alcotest.(list (pair string Utils.xml_testable))

let test_find_all path_str expected () =
  let sort_results l =
    List.sort (fun (p1, x1) (p2, x2) ->
      let c = String.compare p1 p2 in
      if c <> 0 then c
      else String.compare (Utils.xml_to_string x1) (Utils.xml_to_string x2)
    ) l
  in
  let processed_xml = with_parents sample_xml in
  let result = find_all path_str processed_xml |> sort_results in
  let expected = expected |> sort_results in
  Alcotest.check find_all_testable ("find_all " ^ path_str) expected result

let current_parent_filter_test_cases =
  [
    ("/**/b/.",
      [
        ("/root/a/b",Xml.Element { name = "b"; attrs = []; childs = [Xml.Data {value = "hello"; parent = None}]; parent = None });
        ("/root/a/b",Xml.Element { name = "b"; attrs = [("lang", "en")]; childs = [Xml.Data {value = "world"; parent = None}]; parent = None });
        ("/root/e/f/b",Xml.Element { name = "b"; attrs = []; childs = []; parent = None });
      ]);

    ("/**/b/..",
      [
        ("/root/a",Xml.Element { name = "a"; attrs = [("id", "1")]; childs = [
          Xml.Element { name = "b"; attrs = []; childs = [Xml.Data {value = "hello"; parent = None}]; parent = None };
          Xml.Element { name = "c"; attrs = [("val", "test")]; childs = []; parent = None };
        ]; parent = None; });
        ("/root/a",Xml.Element { name = "a"; attrs = [("id", "2")]; childs = [
          Xml.Element { name = "d"; attrs = []; childs = []; parent = None };
          Xml.Element { name = "b"; attrs = [("lang", "en")]; childs = [Xml.Data {value = "world"; parent = None}]; parent = None };
        ]; parent = None; });
        ("/root/e/f",Xml.Element { name = "f"; attrs = []; childs = [
          Xml.Element { name = "b"; attrs = []; childs = []; parent = None }
        ]; parent = None; });
      ]);

    ("/**/c/../b",
      [
        ("/root/a/b",Xml.Element { name = "b"; attrs = []; childs = [Xml.Data {value = "hello"; parent = None}]; parent = None });
      ]);
  ]

let () =
  Alcotest.run "Upath Current Parent" [
    "find_path",
    List.map (fun (path, expected) ->
      Alcotest.test_case path `Quick (test_find_path path expected)
    )
    current_parent_test_cases;
    "find_all",
    List.map (fun (path, expected) ->
      Alcotest.test_case path `Quick (test_find_all path expected)
    )
    current_parent_filter_test_cases;
  ]
