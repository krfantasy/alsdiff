(** A simple path parser for a subset of XPath-like syntax.
    Supports:
    - Tag names with optional attributes (e.g., `tag`, `tag@attr="value"`, `tag@attr=*`)
    - Indexing with optional Tag name (e.g., `[0]`, `[1]`, `tag[3]`)
    - Single wildcard (`*`) for single level matching
    - Multi wildcard (`**`) for multiple levels matching
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
  List.fold_right (fun x acc -> "/" ^ x ^ acc) path ""


type search_node = {
  path_to_parent: string;
  node: Xml.t;
}

module Seq = Stdlib.Seq

(** Find all XML elements in [tree] that match the [path] as a lazy sequence. *)
let find_all_seq (tree : Xml.t) (path : string) : (string * Xml.t) Seq.t =
  let parsed_path =
    match Parser.parse_path path with
    | Ok p -> p
    | Error msg -> failwith ("Failed to parse path: " ^ path ^ " with error: " ^ msg)
  in
  let children_of n =
    match n.node with
    | Xml.Element {childs; name; _} ->
      let new_path = n.path_to_parent ^ "/" ^ name in
      Seq.map (fun child -> {path_to_parent=new_path; node=child}) (List.to_seq childs)
    | Xml.Data _ -> Seq.empty
  in
  let rec find_all (p: path_component list) (nodes: search_node Seq.t) : search_node Seq.t =
    match p with
    | [] -> nodes
    | c :: rest ->
      match c with
      | Tag _ | SingleWildcard ->
          let children = Seq.flat_map children_of nodes in
          let matched_children = Seq.filter (fun sn -> match_component sn.node c) children in
          find_all rest matched_children
      | Index (i, tag_opt) ->
          let children = Seq.flat_map children_of nodes in
          let filtered_children =
            match tag_opt with
            | None -> children
            | Some tag ->
              Seq.filter (fun sn ->
                  match sn.node with
                  | Xml.Element { name = t; _ } -> t = tag
                  | _ -> false
                ) children
          in
          (match Seq.drop i filtered_children |> Seq.uncons with
          | Some (n, _) -> find_all rest (Seq.return n)
          | None -> Seq.empty)
      | MultiWildcard ->
          (* MultiWildcard matches any number of intermediate elements, so we need to collect
           * all descendant nodes (including the current nodes) and apply the rest of the path *)
          let rec collect_all_descendants acc current_nodes =
            if Seq.is_empty current_nodes then acc
            else
              let current_nodes_list = List.of_seq current_nodes in
              let all_nodes = Seq.append acc (List.to_seq current_nodes_list) in
              let next_level = Seq.flat_map children_of current_nodes in
              collect_all_descendants all_nodes next_level
          in
          find_all rest (collect_all_descendants Seq.empty nodes)
  in
  let initial_node = { path_to_parent = ""; node = tree } in
  let result_seq =
    match parsed_path with
    | (Tag(name, _) as first) :: rest ->
        if name = (match tree with Xml.Element {name=n;_} -> n | _ -> "") && match_component tree first then
          find_all rest (Seq.return initial_node)
        else
          find_all parsed_path (Seq.return initial_node)
    | _ ->
        find_all parsed_path (Seq.return initial_node)
  in
  result_seq |> Seq.map (fun {path_to_parent; node} ->
      let final_path = match node with
        | Xml.Element {name; _} -> path_to_parent ^ "/" ^ name
        | Xml.Data _ -> path_to_parent
      in
      (final_path, node)
    )
  |> Seq.memoize

(** Find all XML elements in [tree] that match the [path]. *)
let find_all (tree : Xml.t) (path : string) : (string * Xml.t) list =
  find_all_seq tree path |> List.of_seq

(** Find the first XML element in [tree] that matches the [path]. *)
let find (tree : Xml.t) (path : string) : (string * Xml.t) option =
  find_all_seq tree path |> Seq.uncons |> Option.map fst
