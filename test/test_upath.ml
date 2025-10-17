open Alsdiff_base.Xml
open Alsdiff_base.Upath
open Test_utils.Utils

let sample_xml =
  Element {
    name = "root";
    attrs = [];
    childs = [
      Element {
        name = "a";
        attrs = [("id", "1")];
        childs = [
          Element { name = "b"; attrs = []; childs = [Data {value = "hello"; parent = None}]; parent = None };
          Element { name = "c"; attrs = [("val", "test")]; childs = []; parent = None };
        ];
        parent = None;
      };
      Element {
        name = "a";
        attrs = [("id", "2")];
        childs = [
          Element { name = "d"; attrs = []; childs = []; parent = None };
          Element {
            name = "b";
            attrs = [("lang", "en")];
            childs = [Data {value = "world"; parent = None}];
            parent = None;
          };
        ];
        parent = None;
      };
      Element {
        name = "e";
        attrs = [];
        childs = [
          Element {
            name = "f";
            attrs = [];
            childs = [
              Element { name = "b"; attrs = []; childs = []; parent = None }
            ];
            parent = None;
          }
        ];
        parent = None;
      }
    ];
    parent = None;
  }

let find_path_testable = Alcotest.(option (pair string xml_testable))

let test_find_path path_str expected () =
  let result = find path_str sample_xml in
  Alcotest.check find_path_testable ("find_path " ^ path_str) expected result

let test_cases =
  [
    ("/root/a/b", Some ("/root/a/b", Element { name = "b"; attrs = []; childs = [Data {value = "hello"; parent = None}]; parent = None }));
    ("/root/a[0]/b", Some ("/root/a/b", Element { name = "b"; attrs = []; childs = [Data {value = "hello"; parent = None}]; parent = None }));
    ("/root/a[1]/b", Some ("/root/a/b", Element { name = "b"; attrs = [("lang", "en")]; childs = [Data {value = "world"; parent = None}]; parent = None }));
    ("/root/a@id=\"1\"/c", Some ("/root/a/c", Element { name = "c"; attrs = [("val", "test")]; childs = []; parent = None }));
    ("/root/a@id=\"2\"/d", Some ("/root/a/d", Element { name = "d"; attrs = []; childs = []; parent = None }));
    ("/root/a@id=\"3\"/d", None);
    ("/root/*/b", Some ("/root/a/b", Element { name = "b"; attrs = []; childs = [Data {value = "hello"; parent = None}]; parent = None }));
    ("/root/**/b", Some ("/root/a/b", Element { name = "b"; attrs = []; childs = [Data {value = "hello"; parent = None}]; parent = None }));
    ("/**/f/b", Some ("/root/e/f/b", Element { name = "b"; attrs = []; childs = []; parent = None }));
    ("/root/e/**/b", Some ("/root/e/f/b", Element { name = "b"; attrs = []; childs = []; parent = None }));
    ("/**/d", Some ("/root/a/d", Element { name = "d"; attrs = []; childs = []; parent = None }));
    ("/root/a/nonexistent", None);
    ("/root/a[2]/b", None);
    ("/**/b@lang=\"en\"", Some ("/root/a/b", Element { name = "b"; attrs = [("lang", "en")]; childs = [Data {value = "world"; parent = None}]; parent = None }));
    ("/**/b@lang=\"fr\"", None);
  ]

let filter_path_testable = Alcotest.(list (pair string xml_testable))

let test_find_all path_str expected () =
  let sort_results l =
    List.sort (fun (p1, x1) (p2, x2) ->
      let c = String.compare p1 p2 in
      if c <> 0 then c
      else String.compare (xml_to_string x1) (xml_to_string x2)
    ) l
  in
  let result = find_all path_str sample_xml |> sort_results in
  let expected = expected |> sort_results in
  Alcotest.check filter_path_testable ("filter_path " ^ path_str) expected result

let filter_test_cases =
  [
    ("/root/a/b",
      [
        ("/root/a/b", Element { name = "b"; attrs = []; childs = [Data {value = "hello"; parent = None}]; parent = None });
        ("/root/a/b", Element { name = "b"; attrs = [("lang", "en")]; childs = [Data {value = "world"; parent = None}]; parent = None });
      ]);
    ("/root/a@id=\"1\"/b",
      [
        ("/root/a/b", Element { name = "b"; attrs = []; childs = [Data {value = "hello"; parent = None}]; parent = None });
      ]);
    ("/root/a@id=\"2\"/b",
      [
        ("/root/a/b", Element { name = "b"; attrs = [("lang", "en")]; childs = [Data {value = "world"; parent = None}]; parent = None });
      ]);
    ("/root/*/b",
      [
        ("/root/a/b", Element { name = "b"; attrs = []; childs = [Data {value = "hello"; parent = None}]; parent = None });
        ("/root/a/b", Element { name = "b"; attrs = [("lang", "en")]; childs = [Data {value = "world"; parent = None}]; parent = None });
      ]);
    ("/root/nonexistent", []);
    ("/root/a/nonexistent", []);
    ("/**/b",
      [
        ("/root/a/b", Element { name = "b"; attrs = []; childs = [Data {value = "hello"; parent = None}]; parent = None });
        ("/root/a/b", Element { name = "b"; attrs = [("lang", "en")]; childs = [Data {value = "world"; parent = None}]; parent = None });
        ("/root/e/f/b", Element { name = "b"; attrs = []; childs = []; parent = None });
      ]);
  ]

let sample_xml_nested =
  Element {
    name = "root";
    attrs = [];
    childs = [
      Element {
        name = "a";
        attrs = [("id", "1")];
        childs = [
          Element { name = "b"; attrs = []; childs = [
            Element { name = "c"; attrs = [("v", "1")]; childs = []; parent = None };
            Element { name = "c"; attrs = [("v", "2")]; childs = []; parent = None };
          ]; parent = None};
        ];
        parent = None;
      };
      Element {
        name = "a";
        attrs = [("id", "2")];
        childs = [
          Element { name = "d"; attrs = []; childs = [
            Element { name = "b"; attrs = []; childs = [
              Element { name = "c"; attrs = [("v", "3")]; childs = []; parent = None }
            ]; parent = None}
          ]; parent = None};
        ];
        parent = None;
      };
    ];
    parent = None;
  }

let test_find_path_nested path_str expected () =
  let result = find path_str sample_xml_nested in
  Alcotest.check find_path_testable ("find_path_nested " ^ path_str) expected result

let nested_test_cases = [
  ("/root/a/b", Some ("/root/a/b", Element { name = "b"; attrs = []; childs = [
    Element { name = "c"; attrs = [("v", "1")]; childs = []; parent = None };
    Element { name = "c"; attrs = [("v", "2")]; childs = []; parent = None };
  ]; parent = None }));
  ("/root/a/d/b", Some("/root/a/d/b", Element { name = "b"; attrs = []; childs = [
    Element { name = "c"; attrs = [("v", "3")]; childs = []; parent = None }
  ]; parent = None }));
  ("/root/**/b", Some ("/root/a/b", Element { name = "b"; attrs = []; childs = [
    Element { name = "c"; attrs = [("v", "1")]; childs = []; parent = None };
    Element { name = "c"; attrs = [("v", "2")]; childs = []; parent = None };
  ]; parent = None }));
  ("/root/a/b/c[1]", Some("/root/a/b/c", Element { name = "c"; attrs = [("v", "2")]; childs = []; parent = None }));
  ("/root/a[1]/d/b", Some("/root/a/d/b", Element { name = "b"; attrs = []; childs = [
    Element { name = "c"; attrs = [("v", "3")]; childs = []; parent = None }
  ]; parent = None }));
]

let test_find_all_nested path_str expected () =
  let sort_results l =
    List.sort (fun (p1, x1) (p2, x2) ->
      let c = String.compare p1 p2 in
      if c <> 0 then c
      else String.compare (xml_to_string x1) (xml_to_string x2)
    ) l
  in
  let result = find_all path_str sample_xml_nested |> sort_results in
  let expected = expected |> sort_results in
  Alcotest.check filter_path_testable ("find_all_nested " ^ path_str) expected result

let nested_filter_test_cases = [
  ("/root/a/b", [
    ("/root/a/b", Element { name = "b"; attrs = []; childs = [
      Element { name = "c"; attrs = [("v", "1")]; childs = []; parent = None };
      Element { name = "c"; attrs = [("v", "2")]; childs = []; parent = None };
    ]; parent = None });
  ]);
  ("/root/**/b", [
    ("/root/a/b", Element { name = "b"; attrs = []; childs = [
      Element { name = "c"; attrs = [("v", "1")]; childs = []; parent = None };
      Element { name = "c"; attrs = [("v", "2")]; childs = []; parent = None };
    ]; parent = None });
    ("/root/a/d/b", Element { name = "b"; attrs = []; childs = [
      Element { name = "c"; attrs = [("v", "3")]; childs = []; parent = None }
    ]; parent = None });
  ]);
  ("/root/**/c", [
    ("/root/a/b/c", Element { name = "c"; attrs = [("v", "1")]; childs = []; parent = None });
    ("/root/a/b/c", Element { name = "c"; attrs = [("v", "2")]; childs = []; parent = None });
    ("/root/a/d/b/c", Element { name = "c"; attrs = [("v", "3")]; childs = []; parent = None });
  ]);
  ("/root/a/b/c", [
    ("/root/a/b/c", Element { name = "c"; attrs = [("v", "1")]; childs = []; parent = None });
    ("/root/a/b/c", Element { name = "c"; attrs = [("v", "2")]; childs = []; parent = None });
  ]);
  ("/root/a[0]/**/c", [
    ("/root/a/b/c", Element { name = "c"; attrs = [("v", "1")]; childs = []; parent = None });
    ("/root/a/b/c", Element { name = "c"; attrs = [("v", "2")]; childs = []; parent = None });
  ]);
  ("/root/a[1]/**/c", [
    ("/root/a/d/b/c", Element { name = "c"; attrs = [("v", "3")]; childs = []; parent = None });
  ]);
]

let () =
  Alcotest.run "Upath" [
    "find_path",
    List.map (fun (path, expected) ->
      Alcotest.test_case path `Quick (test_find_path path expected)
    )
    test_cases;
    "find_all",
    List.map (fun (path, expected) ->
      Alcotest.test_case path `Quick (test_find_all path expected)
    )
    filter_test_cases;
    "find_path_nested",
    List.map (fun (path, expected) ->
      Alcotest.test_case path `Quick (test_find_path_nested path expected)
    )
    nested_test_cases;
    "find_all_nested",
    List.map (fun (path, expected) ->
      Alcotest.test_case path `Quick (test_find_all_nested path expected)
    )
    nested_filter_test_cases;
  ]
