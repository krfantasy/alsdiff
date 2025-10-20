(** A simple path parser for a subset of XPath-like syntax.
    Supports:
    - Tag names with optional attributes (e.g., `tag`, `tag@attr="value"`, `tag@attr=*`)
    - Indexing with optional Tag name (e.g., `[0]`, `[1]`, `tag[3]`)
    - Single wildcard (`*`) for single level matching
    - Multi wildcard (`**`) for multiple levels matching
    - Wildcards with optional attributes (e.g., `*@attr="value"`, `**@attr`)
    - Paths separated by `/` (e.g., `/tag1/tag2@attr="value"/*/tag3/**/tag4`)

    Does not support:
    - Functions
    - Complex predicates
    - Namespaces
    - Other XPath features

    Example usage:
    let result = parse_path "/bookstore/book@category=\"cooking\"/title"
    let wildcard_with_attr = parse_path "//*[@type=\"magic\"]"
*)


type attribute_value =
  | Exact of string
  | Any

type attribute = {
  name : string;
  value : attribute_value;
} [@@deriving eq]

type path_component =
  | Tag of string * attribute list
  | Index of int * string option
  | SingleWildcard of attribute list
  | MultiWildcard of attribute list

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
    let p_single_wildcard =
      lift2 (fun _ attrs -> SingleWildcard attrs)
        (char '*')
        (many p_attribute)
    in
    let p_multi_wildcard =
      lift2 (fun _ attrs -> MultiWildcard attrs)
        (string "**")
        (many p_attribute)
    in
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
    let optional_slash = option None (char '/' >>| fun _ -> Some ()) in
    optional_slash *> sep_by1 (char '/') p_component <?> "path"

  let parse_path s =
    parse_string ~consume:All path_parser s

end

(** the search engine *)

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
  | SingleWildcard attrs ->
    (match tree with
     | Xml.Element _ -> match_attributes tree attrs
     | Xml.Data _ -> false)
  | Index _ -> false
  | MultiWildcard attrs ->
    (match tree with
     | Xml.Element _ -> match_attributes tree attrs
     | Xml.Data _ -> false)


let path_to_string path =
  List.fold_right (fun x acc -> "/" ^ x ^ acc) path ""


type search_node = {
  path_to_parent: string;
  node: Xml.t;
}

module Seq = Stdlib.Seq

let parse_path path =
  match Parser.parse_path path with
  | Ok p -> p
  | Error msg -> failwith ("Failed to parse path: " ^ path ^ " with error: " ^ msg)

(** Find all XML elements in [tree] that match the [path] as a lazy sequence. *)
let find_all_seq_0 (path : path_component list) (tree : Xml.t) : (string * Xml.t) Seq.t =
  let children_of n =
    match n.node with
    | Xml.Element {childs; name; _} ->
      let new_path = n.path_to_parent ^ "/" ^ name in
      Seq.map (fun child -> {path_to_parent=new_path; node=child}) (List.to_seq childs)
    | Xml.Data _ -> Seq.empty
  in
  (* Recursively finds matches for a path.
      IMPORTANT: This function has a specific behavior. For each component of the path,
      it searches within the *children* of the nodes from the previous step.
      This is why the initial call needs to be handled carefully. *)
  let rec find_path_in_children (p: path_component list) (nodes: search_node Seq.t) : search_node Seq.t =
    match p with
    | [] -> nodes
    | c :: rest ->
      match c with
      | Tag _ | SingleWildcard _ ->
          let children = Seq.flat_map children_of nodes in
          let matched_children = Seq.filter (fun sn -> match_component sn.node c) children in
          find_path_in_children rest matched_children
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
          | Some (n, _) -> find_path_in_children rest (Seq.return n)
          | None -> Seq.empty)
      | MultiWildcard attrs ->
          (* MultiWildcard matches any number of intermediate elements.
             To do this without consuming memory, we create a fully lazy sequence of all descendants
             and then continue the search from there. *)
          let collect_all_descendants nodes_to_scan =
            let rec aux worklist () =
              match worklist () with
              | Seq.Nil -> Seq.Nil
              | Seq.Cons(hd, tl) ->
                  let children = children_of hd in
                  Seq.Cons(hd, aux (Seq.append tl children))
            in
            aux nodes_to_scan
          in
          let all_descendants = collect_all_descendants nodes in
          let matched_descendants =
            Seq.filter (fun sn -> match_component sn.node (MultiWildcard attrs)) all_descendants
          in
          find_path_in_children rest matched_descendants
  in
  let initial_node = { path_to_parent = ""; node = tree } in
  let result_seq =
    match path with
    | (Tag(name, _) as first) :: rest ->
        if name = (match tree with Xml.Element {name=n;_} -> n | _ -> "") && match_component tree first then
          (* The first path component matches the root `tree` itself.
             So, we start searching for the rest of the path within the children of `tree`. *)
          find_path_in_children rest (Seq.return initial_node)
        else
          (* The first path component does not match the root `tree`.
             Per design, we now search for the *entire* path within the children of `tree`. *)
          find_path_in_children path (Seq.return initial_node)
    | _ ->
        (* The path does not start with a Tag.
           Per design, we search for the *entire* path within the children of `tree`. *)
        find_path_in_children path (Seq.return initial_node)
  in
  result_seq |> Seq.map (fun {path_to_parent; node} ->
      let final_path = match node with
        | Xml.Element {name; _} -> path_to_parent ^ "/" ^ name
        | Xml.Data _ -> path_to_parent
      in
      (final_path, node)
    )
  |> Seq.memoize


let find_all_seq_1 (path : string) (tree : Xml.t) : (string * Xml.t) Seq.t =
  find_all_seq_0 (parse_path path) tree


(** Find all XML elements in [tree] that match the [path]. *)
let find_all (path : string) (tree : Xml.t) : (string * Xml.t) list =
  find_all_seq_1 path tree |> List.of_seq


(** Find the first XML element in [tree] that matches the [path]. *)
let find_opt (path : string) (tree : Xml.t) : (string * Xml.t) option =
  find_all_seq_1 path tree |> Seq.uncons |> Option.map fst


(** Find the first XML element in [tree] that matches the [path].
    @raise [Not_found] when no XML element found *)
let find (path : string) (tree : Xml.t) : string * Xml.t =
  match find_opt path tree with
  | Some result -> result
  | _ -> raise Not_found


(** Find a XML element that path matches [path], and return the attribute [attr] value of it.
    @raise [Invalid_argument] if [path] is invalid, like wildcards path or indexes path.
*)
let find_attr_opt (path : string) (attr : string) (tree : Xml.t) : (string * string) option =
  let parsed_path = parse_path path in
  let last_component = List.hd @@ List.rev parsed_path in
  match last_component with
  | Tag (tag_name, existing_attrs) ->
    (* Check if the required attribute is already in the path constraints *)
    let attr_already_exists = List.exists (fun a -> a.name = attr) existing_attrs in

    (* Add the attribute constraint if it's not already there *)
    let final_attrs =
      if attr_already_exists then
        existing_attrs
      else
        { name = attr; value = Any } :: existing_attrs
    in

    (* Create the final path with the attribute constraint added *)
    let final_last_component = Tag (tag_name, final_attrs) in
    let final_path =
      if List.length parsed_path = 1 then
        [final_last_component]
      else
        let all_but_last = List.rev (List.tl (List.rev parsed_path)) in
        all_but_last @ [final_last_component]
    in

    (* Find the element that matches the updated path (which now includes the attribute requirement) *)
    let matched_element = find_all_seq_0 final_path tree |> Seq.uncons |> Option.map fst in
    Option.map (fun (element_path, xml) ->
        let attr_val = Xml.get_attr attr xml in
        (element_path, attr_val)
      ) matched_element
  | _ -> raise (Invalid_argument "Invalid path for find_attr, the last component must matches a tag")


let find_attr (path : string) (attr : string) (tree : Xml.t) : string * string =
  match find_attr_opt path attr tree with
  | Some result -> result
  | _ -> raise Not_found


let get_attr_opt path attr tree = find_attr_opt path attr tree |> Option.map snd
let get_int_attr_opt path attr tree = Option.bind (get_attr_opt path attr tree) int_of_string_opt
let get_float_attr_opt path attr tree = Option.bind (get_attr_opt path attr tree) float_of_string_opt
let get_bool_attr_opt path attr tree = Option.bind (get_attr_opt path attr tree) (fun x -> x |> String.lowercase_ascii |> bool_of_string_opt)
let get_int64_attr_opt path attr tree = Option.bind (get_attr_opt path attr tree) Int64.of_string_opt

let get_attr path attr tree = find_attr path attr tree |> snd
let get_int_attr path attr tree = get_attr path attr tree |> int_of_string
let get_float_attr path attr tree = get_attr path attr tree |> float_of_string
let get_bool_attr path attr tree = get_attr path attr tree |> String.lowercase_ascii |> bool_of_string
let get_int64_attr path attr tree = get_attr path attr tree |> Int64.of_string


(* Equality and pretty printing functions for testing *)
let equal_path path1 path2 =
  path1 = path2


let pp_path fmt path =
  let pp_component fmt = function
    | Tag (name, attrs) ->
      Format.fprintf fmt "%s" name;
      List.iter (fun {name; value} ->
        match value with
        | Any -> Format.fprintf fmt "@%s" name
        | Exact v -> Format.fprintf fmt "@%s=\"%s\"" name v
      ) attrs
    | Index (i, tag_opt) ->
      (match tag_opt with
       | Some tag -> Format.fprintf fmt "%s" tag
       | None -> ());
      Format.fprintf fmt "[%d]" i
    | SingleWildcard attrs ->
      Format.fprintf fmt "*";
      List.iter (fun {name; value} ->
        match value with
        | Any -> Format.fprintf fmt "@%s" name
        | Exact v -> Format.fprintf fmt "@%s=\"%s\"" name v
      ) attrs
    | MultiWildcard attrs ->
      Format.fprintf fmt "**";
      List.iter (fun {name; value} ->
        match value with
        | Any -> Format.fprintf fmt "@%s" name
        | Exact v -> Format.fprintf fmt "@%s=\"%s\"" name v
      ) attrs
  in
  match path with
  | [] -> Format.fprintf fmt "/"
  | components ->
      Format.fprintf fmt "/%s"
        (String.concat "/" (List.map (fun c ->
           Format.asprintf "%a" pp_component c
         ) components))
