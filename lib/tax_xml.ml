type xml_tree =
  | Element of { name: string; attrs: (string * string) list; childs: xml_tree list }
  | Data of string


let get_name (tag : Xmlm.tag) =
  let ((_, name), _) = tag in name

let get_attrs (tag : Xmlm.tag) =
  let ((_, _), attrs) = tag in
  List.map (fun a -> let ((_, k), v) = a in (k, v)) attrs

let read_xml_tree i =
  let el tag childs = Element {
      name = get_name tag;
      attrs = get_attrs tag;
      childs = childs;
    } in
  let data d = Data d in
  Xmlm.input_doc_tree ~el ~data i

let create_events (xmlevs : xml_tree list) =
  xmlevs
  |> List.map (fun ev ->
      match ev with
      | Element {attrs; _} ->
        (
          Live.{
            id = List.assoc "Id" attrs |> int_of_string;
            time = List.assoc "Time" attrs |> float_of_string;
            value = List.assoc "Value" attrs |> float_of_string;
            others = [];
          } : Live.envelope_event
        )
      | _ -> assert false
    )
  |> Array.of_list


let () =
  let i = Xmlm.make_input ~strip:true @@
    `Channel (open_in "/Users/krfantasy/Desktop/Prelude/Thick Air Project/t") in
  let _ = read_xml_tree i
  in
  ()
