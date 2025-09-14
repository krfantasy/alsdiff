(** A simple path parser for a subset of XPath-like syntax.
    Supports:
    - Tag names with optional attributes (e.g., `tag`, `tag@attr="value"`, `tag@attr=*`)
    - Indexing (e.g., `[0]`, `[1]`)
    - Single wildcard (`*`)
    - Multi wildcard (`**`)
    - Paths separated by `/` (e.g., `/tag1/tag2@attr="value"/*/tag3/**/tag4`)

    Does not support:
    - Functions
    - Complex predicates
    - Namespaces
    - Other XPath features

    Example usage:
    let result = parse_path "/bookstore/book@category=\"cooking\"/title"
*)


type attribute_value =
  | Exact of string
  | Any

type attribute = {
  name : string;
  value : attribute_value;
}

type path_component =
  | Tag of string * attribute list
  | Index of int * string option
  | SingleWildcard
  | MultiWildcard

type path = path_component list


module Parser =
struct

  open Angstrom

  let is_identifier_char = function
    | '/' | '[' | ']' | '@' | '=' | '*' -> false
    | _ -> true

  let identifier = take_while1 is_identifier_char <?> "identifier"

  let integer =
    take_while1 (function '0'..'9' -> true | _ -> false)
    >>| int_of_string
        <?> "integer"

  let p_quoted_string = 
    let p_escaped = char '\\' *> any_char >>| String.make 1 in
    let p_unescaped = take_while1 (fun c -> c <> '"' && c <> '\\') in
    let p_content = many (p_escaped <|> p_unescaped) >>| String.concat "" in
    char '"' *> p_content <* char '"' <?> "quoted string"

  let p_unquoted_string = 
    let p_escaped = char '\\' *> any_char >>| String.make 1 in
    let is_value_terminator = function
      | '@' | '/' | '[' | ']' -> true
      | _ -> false
    in
    let p_unescaped = take_while1 (fun c -> c <> '\\' && not (is_value_terminator c))
    in
    many1 (p_escaped <|> p_unescaped) >>| String.concat "" <?> "unquoted string"

  let p_attr_value = 
    let p_wildcard_val = char '*' *> return Any in
    let p_quoted_val = p_quoted_string >>| fun s -> Exact s in
    let p_unquoted_val = p_unquoted_string >>| fun s -> Exact s in
    choice [
      p_wildcard_val;
      p_quoted_val;
      p_unquoted_val;
    ]

  let p_attribute = 
    let p_key_value = 
      lift2 (fun name value -> { name; value })
        (char '@' *> identifier <* char '=')
        p_attr_value
    in
    let p_key_only = 
      char '@' *> identifier >>| fun name -> { name; value = Any }
    in
    p_key_value <|> p_key_only <?> "attribute"

  let p_component = 
    let p_index = 
      lift2 (fun tag index -> Index (index, tag))
        (option None (identifier >>| Option.some))
        (char '[' *> integer <* char ']')
    in
    let p_single_wildcard = char '*' *> return SingleWildcard in
    let p_multi_wildcard = string "**" *> return MultiWildcard in
    let p_tag = 
      lift2 (fun name attrs -> Tag (name, attrs))
        identifier
        (many p_attribute)
    in
    choice [ p_index;
             p_multi_wildcard;
             p_single_wildcard;
             p_tag ] <?> "path component"

  let path_parser = 
    char '/' *> sep_by1 (char '/') p_component <?> "path"

  let parse_path s = 
    parse_string ~consume:All path_parser s

end

(** the searching engine *)

let match_attributes (tree : Xml.t) (pattrs : attribute list) : bool =
  match tree with
  | Xml.Element { attrs; _ } ->
    let match_attribute attrs k v = 
      try 
        let x = List.assoc k attrs in 
        match v with 
        | Exact v -> x = v 
        | Any -> true
      with 
      | Not_found -> false
    in 
    List.for_all (fun { name; value } -> match_attribute attrs name value) pattrs
  | _ -> assert false


let match_component tree = function
  | Tag (name, attrs) -> 
    (match tree with 
     | Xml.Element { name=tag; _ } when tag = name -> match_attributes tree attrs
     | _ -> false)
  | SingleWildcard -> 
    (match tree with 
     | Xml.Element _ -> true
     | Xml.Data _ -> false)
  | Index _ -> false
  | MultiWildcard -> true


let path_to_string path = 
  List.fold_right (fun acc x -> "/" ^ acc ^ x) path ""


type search_node = {
  path_to_parent: string;
  node: Xml.t;
}

(** Find the first XML element in [tree] that matches the [path]. *)
let find_path (tree : Xml.t) (path : path) : (string * Xml.t) option =
  let rec find (p: path_component list) (nodes: search_node list) : search_node option =
    let children_of n = 
      match n.node with 
      | Xml.Element {childs; name; _} -> 
        let new_path = n.path_to_parent ^ "/" ^ name in 
        List.map (fun child -> {path_to_parent=new_path; node=child}) childs
      | Xml.Data _ -> []
    in 
    match p with 
    | [] -> List.nth_opt nodes 0
    | c :: rest -> 
      match c with 
      | Tag _ | SingleWildcard -> 
        (* Check if current nodes match the component *)
        let matched_nodes = List.filter (fun sn -> match_component sn.node c) nodes in
        (match matched_nodes with 
         | [] -> 
           (* No matches at current level, search children of all nodes *)
           let all_children = List.concat_map children_of nodes in 
           if all_children = [] then None (* No more children to search *) 
           else find p all_children (* Continue searching with same component *)
         | matched -> 
           (* We have matches at current level *)
           if rest = [] then List.nth_opt matched 0 (* Return first match if no more components *) 
           else 
             (* Search children of matched nodes for remaining components *)
             let all_children = List.concat_map children_of matched in 
             find rest all_children)
      | Index (i, tag_opt) ->
        let all_children = List.concat_map children_of nodes in
        let filtered_nodes =
          match tag_opt with
          | None -> all_children
          | Some tag ->
            List.filter (fun sn ->
                match sn.node with
                | Xml.Element { name = t; _ } -> t = tag
                | _ -> false
              ) all_children
        in
        (match List.nth_opt filtered_nodes i with
         | Some n -> find rest [n]
         | None -> None)
      | MultiWildcard -> 
        let rec search_deep nodes' = 
          match find rest nodes' with 
          | Some res -> Some res 
          | None -> 
            let grandchildren = List.concat_map children_of nodes' in 
            if grandchildren = [] then None 
            else search_deep grandchildren 
        in 
        search_deep nodes
  in 

  let initial_node = { path_to_parent = ""; node = tree } in 
  (* First, check if the root element matches the first path component *)
  match path with 
  | [] -> None (* Empty path should not match anything *)
  | first_component :: rest_path -> 
    if match_component tree first_component then 
      (* Root matches, continue search with remaining path components *)
      match find rest_path [initial_node] with 
      | None -> None 
      | Some {path_to_parent; node} -> 
        let final_path = match node with 
          | Xml.Element {name; _} -> path_to_parent ^ "/" ^ name 
          | Xml.Data _ -> path_to_parent 
        in 
        Some (final_path, node)
    else 
      None (* Root doesn't match first component *)
