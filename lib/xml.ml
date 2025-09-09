type t =
  | Element of { name: string; attrs: (string * string) list; childs: t list }
  | Data of string

let read_file filename =
  let get_name (tag : Xmlm.tag) =
    let ((_, name), _) = tag in name in

  let get_attrs (tag : Xmlm.tag) =
    let ((_, _), attrs) = tag in
    List.map (fun a -> let ((_, k), v) = a in (k, v)) attrs in

  let el tag childs = Element {
      name = get_name tag;
      attrs = get_attrs tag;
      childs = childs;
    } in
  let data d = Data d in

  let i = Xmlm.make_input ~strip:true (`Channel (In_channel.open_text filename)) in
  Xmlm.input_doc_tree ~el ~data i
