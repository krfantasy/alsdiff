(** A simple path parser for a subset of XPath-like syntax.
    Supports:
    - Tag names with optional attributes (e.g., `tag`, `tag[@attr="value"]`, `tag[@attr=*]`)
    - Indexing (e.g., `[0]`, `[1]`)
    - Single wildcard (`*`)
    - Multi wildcard (`**`)
    - Paths separated by `/` (e.g., `/tag1/tag2[@attr="value"]/*/[0]/**/tag3`)

    Does not support:
    - Functions
    - Complex predicates
    - Namespaces
    - Other XPath features

    Example usage:
    let result = parse_path "/bookstore/book[@category=\"cooking\"]/title"
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
  | Index of int
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
    let p_index = lift (fun i -> Index i) (char '[' *> integer <* char ']') in
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

(** the matching engine *)
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
    begin
      match tree with
      | Xml.Element { name=tag; _ } when tag = name -> match_attributes tree attrs
      | _ -> false
    end
  | Index _ -> false
  | SingleWildcard -> true
  | MultiWildcard -> true


let path_to_string path =
  List.fold_right (fun acc x -> "/" ^ acc ^ x) path ""

(** Find the first XML element in [tree] that matches the [path]. *)
let find_path (tree : Xml.t) (path : path) : (string * Xml.t) option =
  let rec aux (tree : Xml.t) (prev_state : path_component) (path : path) (final_path : string list) : (string * Xml.t) option =
    match (tree, path) with
    | (_, []) -> Some (path_to_string @@ List.rev final_path, tree)
    | (Element {name; childs; _}, x :: xs) ->
      let new_final_path = name :: final_path in
      (match (prev_state, x) with
         | (MultiWildcard, Tag (_, _)) ->
           if match_component tree x
           then
             List.find_map (fun child -> aux child x xs new_final_path) childs
           else
             List.find_map
               (fun child -> aux child prev_state xs new_final_path) childs
         | (MultiWildcard, _) -> None
         | (_, Tag (_, _)) ->
           if match_component tree x
           then
             List.find_map (fun child -> aux child x xs new_final_path) childs
           else None
         | (_, Index i) ->
           if (i >= 0) && (i < (List.length childs))
           then aux (List.nth childs i) x xs new_final_path
           else None
         | (_, SingleWildcard) ->
           List.find_map (fun child -> aux child x xs new_final_path) childs
         | _ -> assert false
      )
    | _ -> assert false
  in
  aux tree SingleWildcard path []
