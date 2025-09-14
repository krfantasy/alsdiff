open Live_git_lib.Xml

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

let () =
  print_endline "Testing path finding:";

  (* Test case 1: Valid path that exists *)
  let parser_result1 = Live_git_lib.Upath.Parser.parse_path "/root/a/b" in
  (match parser_result1 with
   | Error s -> Printf.printf "Parse error for /root/a/b: %s\n" s
   | Ok path1 ->
     Printf.printf "Testing /root/a/b: ";
     let result1 = Live_git_lib.Upath.find_path sample_xml path1 in
     match result1 with
     | Some (p, _) -> Printf.printf "Found path: %s\n" p
     | None -> Printf.printf "Path not found\n");

  (* Test case 2: Invalid path (wrong root) *)
  let parser_result2 = Live_git_lib.Upath.Parser.parse_path "/a/b" in
  (match parser_result2 with
   | Error s -> Printf.printf "Parse error for /a/b: %s\n" s
   | Ok path2 ->
     Printf.printf "Testing /a/b: ";
     let result2 = Live_git_lib.Upath.find_path sample_xml path2 in
     match result2 with
     | Some (p, _) -> Printf.printf "Found path: %s\n" p
     | None -> Printf.printf "Path not found\n");

  (* Test case 3: Non-existent path *)
  let parser_result3 = Live_git_lib.Upath.Parser.parse_path "/xyz/a/b" in
  match parser_result3 with
  | Error s -> Printf.printf "Parse error for /xyz/a/b: %s\n" s
  | Ok path3 ->
    Printf.printf "Testing /xyz/a/b: ";
    let result3 = Live_git_lib.Upath.find_path sample_xml path3 in
    match result3 with
    | Some (p, _) -> Printf.printf "Found path: %s\n" p
    | None -> Printf.printf "Path not found\n"
