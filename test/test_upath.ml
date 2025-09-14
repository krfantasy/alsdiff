open Live_git_lib.Xml
open Live_git_lib.Upath

let sample_xml =
  Element {
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
      };
      Element {
        name = "a";
        attrs = [("id", "2")];
        childs = [
          Element { name = "d"; attrs = []; childs = [] };
          Element {
            name = "b";
            attrs = [("lang", "en")];
            childs = [Data "world"]
          };
        ]
      };
      Element {
        name = "e";
        attrs = [];
        childs = [
          Element {
            name = "f";
            attrs = [];
            childs = [
              Element { name = "b"; attrs = []; childs = [] }
            ]
          }
        ]
      }
    ]
  }

let rec xml_to_string = function
  | Element {name; attrs; childs} ->
      let attrs_str = attrs |> List.map (fun (k, v) -> Printf.sprintf "%s=\"%s\"" k v) |> String.concat " " in
      let childs_str = childs |> List.map xml_to_string |> String.concat "" in
      Printf.sprintf "<%s %s>%s</%s>" name attrs_str childs_str name
  | Data s -> s

let pp_xml fmt xml = Fmt.string fmt (xml_to_string xml)

let xml_testable = Alcotest.testable pp_xml (==)

let find_path_testable = Alcotest.(option (pair string xml_testable))

let test_find_path path_str expected () =
  let path =
    match Parser.parse_path path_str with
    | Ok p -> p
    | Error msg -> Alcotest.fail ("Failed to parse path: " ^ path_str ^ " with error: " ^ msg)
  in
  let result = find_path sample_xml path in
  Alcotest.check find_path_testable ("find_path " ^ path_str) expected result

let test_cases =
  [
    ("/root/a/b", Some ("/root/a/b", Element { name = "b"; attrs = []; childs = [Data "hello"] }));
    ("/root/a[0]/b", Some ("/root/a/b", Element { name = "b"; attrs = []; childs = [Data "hello"] }));
    ("/root/a[1]/b", Some ("/root/a/b", Element { name = "b"; attrs = [("lang", "en")]; childs = [Data "world"] }));
    ("/root/a@id=\"1\"/c", Some ("/root/a/c", Element { name = "c"; attrs = [("val", "test")]; childs = [] }));
    ("/root/a@id=\"2\"/d", Some ("/root/a/d", Element { name = "d"; attrs = []; childs = [] }));
    ("/root/a@id=\"3\"/d", None);
    ("/root/*/b", Some ("/root/a/b", Element { name = "b"; attrs = []; childs = [Data "hello"] }));
    ("/root/**/b", Some ("/root/a/b", Element { name = "b"; attrs = []; childs = [Data "hello"] }));
    ("/**/f/b", Some ("/root/e/f/b", Element { name = "b"; attrs = []; childs = [] }));
    ("/root/e/**/b", Some ("/root/e/f/b", Element { name = "b"; attrs = []; childs = [] }));
    ("/**/d", Some ("/root/a/d", Element { name = "d"; attrs = []; childs = [] }));
    ("/root/a/nonexistent", None);
    ("/root/a[2]/b", None);
    ("/**/b@lang=\"en\"", Some ("/root/a/b", Element { name = "b"; attrs = [("lang", "en")]; childs = [Data "world"] }));
    ("/**/b@lang=\"fr\"", None);
  ]

let () =
  Alcotest.run "Upath" [
    "find_path",
    List.map (fun (path, expected) ->
      Alcotest.test_case path `Quick (test_find_path path expected)
    ) test_cases
  ]
