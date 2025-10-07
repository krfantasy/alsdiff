type t =
  | Element of { name: string; attrs: (string * string) list; childs: t list }
  | Data of string


(* Helper functions for parsing XML *)
let get_name (tag : Xmlm.tag) =
  let ((_, name), _) = tag in name

let get_attrs (tag : Xmlm.tag) =
  let ((_, _), attrs) = tag in
  List.map (fun a -> let ((_, k), v) = a in (k, v)) attrs

let make_element tag childs = Element {
    name = get_name tag;
    attrs = get_attrs tag;
    childs = childs;
  }

let make_data d = Data d

let read_file filename =
  let i = Xmlm.make_input ~strip:true (`Channel (In_channel.open_text filename)) in
  Xmlm.input_doc_tree ~el:make_element ~data:make_data i

let read_string s =
  let i = Xmlm.make_input ~strip:true (`String (0, s)) in
  Xmlm.input_doc_tree ~el:make_element ~data:make_data i
