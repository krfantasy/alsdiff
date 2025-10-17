open Alsdiff_base.Xml

let rec xml_to_string = function
  | Element {name; attrs; childs; parent = _} ->
      let attrs_str = attrs |> List.map (fun (k, v) -> Printf.sprintf "%s=\"%s\"" k v) |> String.concat " " in
      let childs_str = childs |> List.map xml_to_string |> String.concat "" in
      Printf.sprintf "<%s%s>%s</%s>" name (if attrs_str <> "" then " " ^ attrs_str else "") childs_str name
  | Data {value = s; parent = _} -> s

let pp_xml fmt xml = Fmt.string fmt (xml_to_string xml)

let xml_testable = Alcotest.testable pp_xml (=)
