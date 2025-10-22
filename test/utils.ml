open Alsdiff_base.Xml

let rec xml_to_string = function
  | Element {name; attrs; childs; parent = _} ->
      let attrs_str = attrs |> List.map (fun (k, v) -> Printf.sprintf "%s=\"%s\"" k v) |> String.concat " " in
      let childs_str = childs |> List.map xml_to_string |> String.concat "" in
      Printf.sprintf "<%s%s>%s</%s>" name (if attrs_str <> "" then " " ^ attrs_str else "") childs_str name
  | Data {value = s; parent = _} -> s

let rec xml_equal_ignore_parent x y =
  match x, y with
  | Element {name = n1; attrs = a1; childs = c1; parent = _},
    Element {name = n2; attrs = a2; childs = c2; parent = _} ->
      n1 = n2 && a1 = a2 && List.length c1 = List.length c2 &&
      List.for_all2 xml_equal_ignore_parent c1 c2
  | Data {value = v1; parent = _}, Data {value = v2; parent = _} ->
      v1 = v2
  | _ -> false

let pp_xml fmt xml = Fmt.string fmt (xml_to_string xml)

let xml_testable = Alcotest.testable pp_xml xml_equal_ignore_parent
