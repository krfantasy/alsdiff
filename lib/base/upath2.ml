(** Path-to-NFA compiler and streaming evaluator.

    Declare path queries upfront, compile into a combined NFA with prefix
    sharing, evaluate all queries in a single pass over [Xml2] StAX events.
    No DOM allocation.

    Supports all features: MultiWildcard ([**]), SingleWildcard ([*]),
    ParentNode ([..]), CurrentNode ([.]), Regex names, attribute constraints,
    Index, and attribute extraction ([@attr] on last component).

    Syntax (aligned with Upath):
    - [@attr] on last component → attribute extraction
    - [@attr="value"] → exact match constraint
    - [@attr=*] → existence constraint
    - [@attr] on non-last components → existence constraint *)

(* --- Types (ported from Upath, owned locally) --- *)

type attribute_value = Exact of string | Any

type attribute = {
  name : string;
  value : attribute_value;
}

type name_component =
  | Raw of string
  | Regex of string * Re.Pcre.regexp

type path_component =
  | Tag of name_component * attribute list
  | Index of int * name_component option
  | SingleWildcard of attribute list
  | MultiWildcard of attribute list
  | CurrentNode
  | ParentNode

exception Path_parse_error of string * string

(* --- NFA types --- *)

type query_id = int

type name_matcher =
  | Exact of string
  | Regex of string * Re.Pcre.regexp  (* pattern string + compiled regexp *)
  | Any                                (* matches any element name *)

type attr_constraint = {
  name : string;
  value : string option;  (* None = existence check, Some v = exact match *)
}

type query = {
  path : path_component list;       (** Parsed path components *)
  attr : string option;             (** [None] = match element, [Some name] = extract attribute *)
}

type match_result = {
  query_id : query_id;
  element_name : string;
  attrs : (string * string) list;
  depth : int;
  mutable text_content : string option;  (* text between start/end tags *)
}

(* --- NFA representation --- *)

type state_id = int

type transition = {
  tid : int;                    (* unique ID for fire-count tracking *)
  matcher : name_matcher;    (* label: how to match element names *)
  constraints : attr_constraint list;  (* attribute constraints *)
  target : state_id;
  index : int option;           (* None = normal, Some n = only fire at Nth match *)
  depth_limit : int option;     (* None = any depth, Some d = only at exact depth d *)
}

type nfa_state = {
  id : state_id;
  mutable transitions : transition list;
  mutable accepting : (query_id * attr_constraint list) list;
  mutable is_wildcard_loop : bool;      (* MultiWildcard self-propagation *)
  mutable end_transitions : state_id list;  (* ParentNode: activate on El_end *)
}

type nfa = {
  states : nfa_state array;
  start : state_id;
}

(* Evaluator stack frame — needed for ParentNode name/attr resolution *)
type stack_frame = {
  element_name : string;
  element_attrs : (string * string) list;
  active : state_id list;
  text_buf : Buffer.t;
  frame_results : match_result list;  (* results created at this element's El_start *)
}

(* --- Angstrom parser (ported from Upath.Parser) --- *)

module Parser = struct

  open Angstrom

  let is_identifier_char = function
    | '/' | '[' | ']' | '@' | '=' | '*' | '\'' -> false
    | _ -> true

  let identifier = take_while1 is_identifier_char <?> "identifier"

  let compile_regex pattern =
    try
      Re.Pcre.regexp pattern
    with
    | Re.Pcre.Parse_error | Re.Pcre.Not_supported ->
      failwith (Printf.sprintf "Invalid PCRE regex pattern '%s'" pattern)

  let p_quoted_regex =
    let p_regex_content =
      let p_escaped =
        char '\\' *> any_char >>| fun c ->
        "\\" ^ String.make 1 c
      in
      let p_unescaped = take_while1 (fun c -> c <> '\'' && c <> '\\') in
      many (p_escaped <|> p_unescaped) >>| String.concat ""
    in
    char '\'' *> p_regex_content <* char '\'' >>= fun pattern ->
    if pattern = "" then
      fail "Invalid path: empty regex '' is not allowed"
    else
      return (Regex (pattern, compile_regex pattern) : name_component)

  let p_name_component : name_component Angstrom.t =
    choice [
      p_quoted_regex;
      identifier >>| fun name -> Raw name;
    ]

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
    let p_wildcard_val = char '*' *> return (Any : attribute_value) in
    let p_quoted_val = p_quoted_string >>| fun s -> (Exact s : attribute_value) in
    let p_unquoted_val = p_unquoted_string >>| fun s -> (Exact s : attribute_value) in
    choice [
      p_wildcard_val;
      p_quoted_val;
      p_unquoted_val;
    ]

  let p_attribute =
    let p_key_value =
      lift2 (fun name value -> ({ name; value } : attribute))
        (char '@' *> identifier <* char '=')
        p_attr_value
    in
    let p_key_only =
      char '@' *> identifier >>| fun name -> ({ name; value = (Any : attribute_value) } : attribute)
    in
    p_key_value <|> p_key_only <?> "attribute"

  let p_component : path_component Angstrom.t =
    let p_current_node = char '.' *> return CurrentNode in
    let p_parent_node = string ".." *> return ParentNode in
    let p_index =
      lift2 (fun tag index -> Index (index, tag))
        (option None (p_name_component >>| Option.some))
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
        p_name_component
        (many p_attribute)
    in
    choice [ p_parent_node;
             p_current_node;
             p_index;
             p_multi_wildcard;
             p_single_wildcard;
             p_tag ] <?> "path component"

  let validate_path_components components =
    let has_leading_parent =
      match components with
      | ParentNode :: _ -> true
      | _ -> false
    in
    let rec find_invalid_pattern = function
      | MultiWildcard _ :: MultiWildcard _ :: _
      | MultiWildcard _ :: SingleWildcard _ :: _
      | SingleWildcard _ :: MultiWildcard _ :: _ ->
        Some "Invalid path: adjacent wildcard pairs '**/**', '**/*', and '*/**' are not allowed"
      | MultiWildcard _ :: ParentNode :: _
      | SingleWildcard _ :: ParentNode :: _ ->
        Some "Invalid path: wildcard-parent adjacency '**/..' and '*/..' is not allowed"
      | _ :: rest -> find_invalid_pattern rest
      | [] -> None
    in
    if has_leading_parent then
      fail "Invalid path: leading parent node '..' is not allowed"
    else
      match find_invalid_pattern components with
      | Some msg -> fail msg
      | None -> return components

  let path_parser =
    let optional_slash = option None (char '/' >>| fun _ -> Some ()) in
    optional_slash *> sep_by1 (char '/') p_component >>= validate_path_components <?> "path"

  let parse_path s =
    try
      match parse_string ~consume:All path_parser s with
      | Ok p -> p
      | Error msg -> raise (Path_parse_error (s, msg))
    with
    | Failure msg ->
      raise (Path_parse_error (s, msg))

end

(* --- Post-processing: extract @attr from last component --- *)

(** If the last [Tag] component has a bare [@attr] (value = Any), treat
    it as extraction target. The attribute is kept as an existence
    constraint (element must have the attribute) AND recorded in [attr].
    Bare [@attr] on wildcards remains as existence constraint only. *)
let extract_attr components =
  let find_last_any (attrs : attribute list) =
    let rec loop (rev_rest : attribute list) (elts : attribute list) =
      match elts with
      | [] -> None, List.rev rev_rest
      | [ a ] ->
        (match a.value with
         | Any -> Some a.name, List.rev (a :: rev_rest)
         | Exact _ -> None, List.rev (a :: rev_rest))
      | a :: rest ->
        (match a.value with
         | Any ->
           (match loop (a :: rev_rest) rest with
            | Some name, kept -> Some name, kept
            | None, kept -> None, a :: kept)
         | Exact _ -> loop (a :: rev_rest) rest)
    in
    loop [] attrs
  in
  let rec process_last = function
    | [] -> None, []
    | [ Tag (name, attrs) ] ->
      let attr, kept = find_last_any attrs in
      attr, [ Tag (name, kept) ]
    | [ SingleWildcard _ as wc ] -> None, [ wc ]
    | [ MultiWildcard _ as wc ] -> None, [ wc ]
    | [ Index _ as idx ] -> None, [ idx ]
    | [ CurrentNode ] -> None, [ CurrentNode ]
    | [ ParentNode ] -> None, [ ParentNode ]
    | comp :: rest ->
      let attr, rest' = process_last rest in
      attr, comp :: rest'
  in
  process_last components

(* --- Name / attribute matching --- *)

let match_name tag_name = function
  | Exact s -> tag_name = s
  | Regex (_, re) -> Re.execp re tag_name
  | Any -> true

let check_attrs (xml_attrs : (string * string) list) (constraints : attr_constraint list) =
  List.for_all (fun (c : attr_constraint) ->
      match List.assoc_opt c.name xml_attrs with
      | None -> false
      | Some v ->
        match c.value with
        | None -> true          (* existence check *)
        | Some expected -> v = expected
    ) constraints

(* --- Name matcher equality (for prefix sharing) --- *)

let equal_name_matcher a b =
  match a, b with
  | Exact sa, Exact sb -> sa = sb
  | Regex (pa, _), Regex (pb, _) -> pa = pb
  | Any, Any -> true
  | _ -> false

let equal_attr_constraints (a : attr_constraint list) (b : attr_constraint list) =
  List.length a = List.length b &&
  List.for_all2 (fun (x : attr_constraint) (y : attr_constraint) ->
      x.name = y.name &&
      match x.value, y.value with
      | None, None -> true
      | Some vx, Some vy -> vx = vy
      | _ -> false
    ) a b

let equal_transition_key t matcher constraints index depth_limit =
  equal_name_matcher t.matcher matcher && equal_attr_constraints t.constraints constraints &&
  t.index = index && t.depth_limit = depth_limit

(* --- NFA minimization via bisimulation --- *)
(* Iterative signature refinement: group states with identical outgoing
   behavior (transitions, accepting, wildcard, end_transitions) into blocks.
   Iterate until the partition stabilizes. *)

let matcher_key = function
  | Exact s -> "E" ^ s
  | Regex (p, _) -> "R" ^ p
  | Any -> "A"

let constraints_key cs =
  String.concat "~" (List.map (fun (c : attr_constraint) ->
      match c.value with
      | None -> c.name ^ "=*"
      | Some v -> c.name ^ "=" ^ v) cs)

let accepting_key acc =
  let sorted = List.sort (fun (a, _) (b, _) -> compare a b) acc in
  String.concat "|" (List.map (fun (qid, attrs) ->
      Printf.sprintf "%d:%s" qid (constraints_key attrs)) sorted)

let trans_dedup_key matcher constraints index depth_limit target_block =
  Printf.sprintf "%s|%s|%s|%s->%d"
    (matcher_key matcher)
    (constraints_key constraints)
    (match index with None -> "" | Some n -> string_of_int n)
    (match depth_limit with None -> "" | Some d -> string_of_int d)
    target_block

let transition_sig partition t =
  trans_dedup_key t.matcher t.constraints t.index t.depth_limit partition.(t.target)

let full_signature states partition i =
  let s = states.(i) in
  let trans_keys =
    List.sort String.compare
      (List.map (transition_sig partition) s.transitions)
  in
  let end_keys =
    List.sort_uniq Int.compare
      (List.map (fun target -> partition.(target)) s.end_transitions)
  in
  String.concat "|"
    [ string_of_bool s.is_wildcard_loop
    ; accepting_key s.accepting
    ; String.concat "," trans_keys
    ; String.concat "," (List.map string_of_int end_keys) ]

let minimize_nfa states start_id =
  let count = Array.length states in
  if count <= 1 then states, start_id
  else begin
    let partition = Array.init count (fun i -> i) in
    let prev_sigs = ref None in
    let stable = ref false in
    while not !stable do
      let sigs = Array.init count (full_signature states partition) in
      (match !prev_sigs with
       | None -> ()
       | Some ps ->
         if Array.for_all2 String.equal sigs ps then stable := true);
      if not !stable then begin
        let tbl : (string, int) Hashtbl.t = Hashtbl.create 16 in
        let next_block = ref 0 in
        for i = 0 to count - 1 do
          match Hashtbl.find_opt tbl sigs.(i) with
          | Some block -> partition.(i) <- block
          | None ->
            let block = !next_block in
            incr next_block;
            Hashtbl.add tbl sigs.(i) block;
            partition.(i) <- block
        done;
        prev_sigs := Some sigs
      end
    done;
    let block_count = 1 + Array.fold_left (fun mx b -> max mx b) 0 partition in
    if block_count >= count then states, start_id
    else begin
      let new_states = Array.make block_count (Obj.magic () : nfa_state) in
      let next_tid = ref 0 in
      let repr = Hashtbl.create block_count in
      for i = 0 to count - 1 do
        let block = partition.(i) in
        if not (Hashtbl.mem repr block) then begin
          let rep = states.(i) in
          let fresh_trans =
            let seen = Hashtbl.create 8 in
            List.filter_map (fun t ->
                let target_block = partition.(t.target) in
                let key = trans_dedup_key t.matcher t.constraints
                    t.index t.depth_limit target_block in
                if Hashtbl.mem seen key then None
                else begin
                  Hashtbl.add seen key ();
                  let tid = !next_tid in incr next_tid;
                  Some { t with tid; target = target_block }
                end
              ) rep.transitions
          in
          let fresh_end =
            List.sort_uniq Int.compare
              (List.map (fun target -> partition.(target)) rep.end_transitions)
          in
          Hashtbl.add repr block ();
          new_states.(block) <- {
            id = block;
            transitions = fresh_trans;
            accepting = rep.accepting;
            is_wildcard_loop = rep.is_wildcard_loop;
            end_transitions = fresh_end;
          }
        end
      done;
      new_states, partition.(start_id)
    end
  end

(* --- Name component / attribute conversion to NFA types --- *)

let name_component_to_matcher = function
  | Raw s -> Exact s
  | Regex (p, re) -> Regex (p, re)

let attributes_to_constraints (attrs : attribute list) : attr_constraint list =
  List.map (fun (a : attribute) ->
      { name = a.name;
        value = match a.value with
          | Exact v -> Some v
          | Any -> None }
    ) attrs

(* --- Compilation --- *)

let compile (queries : query list) =
  let states = Hashtbl.create 32 in
  let next_id = ref 0 in
  let next_tid = ref 0 in
  let make_state () =
    let id = !next_id in
    incr next_id;
    let s = { id; transitions = []; accepting = [];
              is_wildcard_loop = false; end_transitions = [] } in
    Hashtbl.add states id s;
    s
  in
  let start_state = make_state () in
  (* Root-fallback: start state self-propagates so that paths not starting
     with the root element name (e.g. ["/Tracks/*/Name/EffectiveName"]) are
     still tried at every depth, matching DOM-based Upath behavior. *)
  start_state.is_wildcard_loop <- true;
  (* Build NFA with prefix sharing *)
  List.iteri (fun qid q ->
      let rec walk path (state : nfa_state) =
        match path with
        | [] ->
          state.accepting <- (qid, []) :: state.accepting
        | comp :: rest ->
          match comp with
          | Tag (name_comp, attrs) ->
            let matcher = name_component_to_matcher name_comp in
            let cattrs = attributes_to_constraints attrs in
            (match List.find_opt (fun t -> equal_transition_key t matcher cattrs None None) state.transitions with
             | Some t ->
               walk rest (Hashtbl.find states t.target)
             | None ->
               let new_state = make_state () in
               let tid = !next_tid in
               incr next_tid;
               state.transitions <- { tid; matcher; constraints = cattrs; target = new_state.id; index = None; depth_limit = None } :: state.transitions;
               walk rest new_state)
          | SingleWildcard attrs ->
            let cattrs = attributes_to_constraints attrs in
            let depth_limit = if state.id = start_state.id then Some 2 else None in
            (match List.find_opt (fun t -> equal_transition_key t Any cattrs None depth_limit) state.transitions with
             | Some t ->
               walk rest (Hashtbl.find states t.target)
             | None ->
               let new_state = make_state () in
               let tid = !next_tid in
               incr next_tid;
               state.transitions <- { tid; matcher = Any; constraints = cattrs; target = new_state.id; index = None; depth_limit } :: state.transitions;
               walk rest new_state)
          | MultiWildcard attrs ->
            let cattrs = attributes_to_constraints attrs in
            (* Find or create a wildcard-loop state *)
            let wl_state =
              match List.find_opt (fun t ->
                  t.matcher = Any && equal_attr_constraints t.constraints cattrs &&
                  t.index = None && t.depth_limit = None
                ) state.transitions with
              | Some t -> Hashtbl.find states t.target
              | None ->
                let new_state = make_state () in
                new_state.is_wildcard_loop <- true;
                let tid = !next_tid in
                incr next_tid;
                state.transitions <- { tid; matcher = Any; constraints = cattrs; target = new_state.id; index = None; depth_limit = None } :: state.transitions;
                new_state
            in
            if rest = [] then begin
              (* MultiWildcard is last component: add accepting with attrs for self-match *)
              wl_state.accepting <- (qid, cattrs) :: wl_state.accepting
            end else begin
              (* Continue compiling rest from the wildcard state *)
              walk rest wl_state
            end
          | ParentNode ->
            (* Add end_transition: target state activates when El_end fires *)
            let target_state = make_state () in
            state.end_transitions <- target_state.id :: state.end_transitions;
            walk rest target_state
          | CurrentNode ->
            (* Epsilon: skip to next component in same state *)
            walk rest state
          | Index (n, name_comp_opt) ->
            let matcher = match name_comp_opt with
              | Some nc -> name_component_to_matcher nc
              | None -> Any
            in
            let depth_limit = if state.id = start_state.id then Some 2 else None in
            (match List.find_opt (fun t -> equal_transition_key t matcher [] (Some n) depth_limit) state.transitions with
             | Some t ->
               walk rest (Hashtbl.find states t.target)
             | None ->
               let new_state = make_state () in
               let tid = !next_tid in
               incr next_tid;
               state.transitions <- { tid; matcher; constraints = []; target = new_state.id; index = Some n; depth_limit } :: state.transitions;
               walk rest new_state)
      in
      walk q.path start_state
    ) queries;
  (* Build array from hashtable *)
  let state_arr = Array.make !next_id (Obj.magic () : nfa_state) in
  Hashtbl.iter (fun id s -> state_arr.(id) <- s) states;
  let state_arr, start_id = minimize_nfa state_arr start_state.id in
  { states = state_arr; start = start_id }

(* --- Evaluation --- *)

let evaluate nfa stream =
  let results = ref [] in
  let fire_counts : (int, int) Hashtbl.t = Hashtbl.create 16 in
  let new_frame_results = ref [] in
  let stack = ref [{
      element_name = "";
      element_attrs = [];
      active = [ nfa.start ];
      text_buf = Buffer.create 0;
      frame_results = [];
    }] in
  let new_active = ref [] in
  let end_targets = ref [] in
  Xml2.iter_signals (fun sigv ->
      match sigv with
      | Xml2.El_start (name, attrs) ->
        let frame = List.hd !stack in
        let active = frame.active in
        new_active := [];
        new_frame_results := [];
        List.iter (fun sid ->
            let state = nfa.states.(sid) in
            (* Wildcard loop: self-propagate *)
            if state.is_wildcard_loop then begin
              if not (List.mem sid !new_active) then
                new_active := sid :: !new_active
            end;
            (* Normal transitions *)
            List.iter (fun t ->
                if match_name name t.matcher && check_attrs attrs t.constraints then begin
                  let depth_ok = match t.depth_limit with
                    | None -> true
                    | Some d -> Xml2.depth stream = d
                  in
                  if depth_ok then begin
                    let fire = match t.index with
                      | None -> true
                      | Some n ->
                        let count = Hashtbl.find_opt fire_counts t.tid
                          |> Option.value ~default:0 in
                        Hashtbl.replace fire_counts t.tid (count + 1);
                        count = n
                    in
                    if fire then begin
                      if not (List.mem t.target !new_active) then
                        new_active := t.target :: !new_active;
                      (* Check accepting at target *)
                      let target_state = nfa.states.(t.target) in
                      List.iter (fun (qid, acc_attrs) ->
                          if check_attrs attrs acc_attrs then begin
                            let r = { query_id = qid; element_name = name;
                                      attrs; depth = Xml2.depth stream;
                                      text_content = None } in
                            results := r :: !results;
                            new_frame_results := r :: !new_frame_results
                          end
                        ) target_state.accepting
                    end
                  end
                end
              ) state.transitions;
            (* Wildcard self-match accepting *)
            if state.is_wildcard_loop then
              List.iter (fun (qid, acc_attrs) ->
                  if check_attrs attrs acc_attrs then begin
                    let r = { query_id = qid; element_name = name;
                              attrs; depth = Xml2.depth stream;
                              text_content = None } in
                    results := r :: !results;
                    new_frame_results := r :: !new_frame_results
                  end
                ) state.accepting
          ) active;
        stack := { element_name = name; element_attrs = attrs;
                   active = !new_active;
                   text_buf = Buffer.create 16;
                   frame_results = !new_frame_results } :: !stack
      | Xml2.El_end ->
        (match !stack with
         | popped :: parent :: rest ->
           (* Update text_content for results from this frame *)
           let text = Buffer.contents popped.text_buf in
           if text <> "" then
             List.iter (fun (r : match_result) ->
                 r.text_content <- Some text
               ) popped.frame_results;
           (* End transitions: ParentNode handling *)
           end_targets := [];
           List.iter (fun sid ->
               let state = nfa.states.(sid) in
               List.iter (fun target_id ->
                   if not (List.mem target_id !end_targets) then
                     end_targets := target_id :: !end_targets
                 ) state.end_transitions
             ) popped.active;
           (* Merge end targets into parent active set *)
           let merged_active =
             List.fold_left (fun acc tid ->
                 if List.mem tid acc then acc else tid :: acc
               ) parent.active !end_targets
           in
           List.iter (fun target_id ->
               (* Check accepting at end-transition target *)
               let target_state = nfa.states.(target_id) in
               List.iter (fun (qid, acc_attrs) ->
                   if check_attrs popped.element_attrs acc_attrs then
                     results := { query_id = qid; element_name = popped.element_name;
                                  attrs = popped.element_attrs;
                                  depth = Xml2.depth stream;
                                  text_content = None } :: !results
                 ) target_state.accepting
             ) !end_targets;
           stack := { parent with active = merged_active } :: rest
         | popped :: rest ->
           (* Update text_content for top-level frame *)
           let text = Buffer.contents popped.text_buf in
           if text <> "" then
             List.iter (fun (r : match_result) ->
                 r.text_content <- Some text
               ) popped.frame_results;
           ignore popped;
           stack := rest
         | [] -> ())
      | Data text ->
        (match !stack with
         | frame :: _ -> Buffer.add_string frame.text_buf text
         | [] -> ())
    ) stream;
  List.rev !results

(* --- API --- *)

(** Constructor from query string. Parses the path, extracts attribute
    from the last component's bare [@attr] if present. *)
let query_of_path query_str =
  let parsed = Parser.parse_path query_str in
  let attr, path = extract_attr parsed in
  { path; attr }

(* --- Helpers --- *)

let get_attr result name =
  List.assoc_opt name result.attrs

let get_int_attr result name =
  Option.bind (get_attr result name) int_of_string_opt

let get_float_attr result name =
  Option.bind (get_attr result name) float_of_string_opt

let get_text_content (r : match_result) : string option = r.text_content

(* --- Query-level result accessors --- *)

let find_result results qid =
  List.find_opt (fun (r : match_result) -> r.query_id = qid) results

let find_all_results results qid =
  List.filter (fun (r : match_result) -> r.query_id = qid) results

let query_attr results qid attr =
  Option.bind (find_result results qid) (fun r -> get_attr r attr)

let query_int_attr results qid attr =
  Option.bind (query_attr results qid attr) int_of_string_opt

let query_float_attr results qid attr =
  Option.bind (query_attr results qid attr) float_of_string_opt

let query_bool_attr results qid attr =
  Option.bind (query_attr results qid attr)
    (fun x -> String.lowercase_ascii x |> bool_of_string_opt)

(* --- DOM-to-stream bridge --- *)

let stream_of_xml xml =
  Xml2.stream_from_string (Fmt.str "%a" Xml.pp_compact xml)

(* --- Pretty printers --- *)

let pp_name_matcher fmt = function
  | Exact s -> Fmt.pf fmt "%s" s
  | Regex (p, _) -> Fmt.pf fmt "'%s'" p
  | Any -> Fmt.pf fmt "*"

let pp_attr_constraint fmt (c : attr_constraint) =
  match c.value with
  | None -> Fmt.pf fmt "@%s" c.name
  | Some v -> Fmt.pf fmt "@%s=\"%s\"" c.name v

let pp_transition fmt t =
  let index_str = match t.index with
    | None -> "" | Some n -> Printf.sprintf "[%d]" n
  in
  let depth_str = match t.depth_limit with
    | None -> "" | Some d -> Printf.sprintf " depth=%d" d
  in
  Fmt.pf fmt "  -[%a%a%s%s]-> %d"
    pp_name_matcher t.matcher
    (Fmt.list pp_attr_constraint) t.constraints
    index_str depth_str
    t.target

let pp_nfa_state fmt s =
  Fmt.pf fmt "State %d%s%s%s@\n%a"
    s.id
    (match s.accepting with [] -> "" | qs -> Printf.sprintf " [accept: %s]"
                                               (String.concat "," (List.map (fun (qid, _) -> string_of_int qid) qs)))
    (if s.is_wildcard_loop then " [wildcard-loop]" else "")
    (match s.end_transitions with [] -> "" | targets -> Printf.sprintf " [end->%s]"
                                                          (String.concat "," (List.map string_of_int targets)))
    (Fmt.list pp_transition) s.transitions

let pp_nfa fmt nfa =
  Fmt.pf fmt "NFA: %d states, start=%d@\n%a"
    (Array.length nfa.states) nfa.start
    (Fmt.list pp_nfa_state) (Array.to_list nfa.states)

let pp_match_result fmt r =
  let attr_str = match get_attr r "Value" with
    | Some v -> Printf.sprintf " Value=%s" v
    | None -> (match r.attrs with
          [] -> ""
        | _ -> Printf.sprintf " attrs=%s"
                 (String.concat "," (List.map (fun (k, v) -> k ^ "=" ^ v) r.attrs)))
  in
  Fmt.pf fmt "q%d: <%s> at depth %d%s" r.query_id r.element_name r.depth attr_str
