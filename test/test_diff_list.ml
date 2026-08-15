open Alcotest
open Alsdiff_base.Diff

(* Create a simple DIFFABLE_EQ module for int *)
module IntDiffEq = struct
  type t = int
  let equal a b = a = b
  module Patch = struct
    type t = int atomic_patch
    let is_empty = function {oldval; newval} -> oldval = newval
  end
  let diff a b = { oldval = a; newval = b}
end

(** Test helpers *)
let pp_atomic_patch pp_val ppf {oldval; newval} =
  Fmt.pf ppf "{oldval=%a; newval=%a}" pp_val oldval pp_val newval

let change_testable (type a p) (pp_a : a Fmt.t) (pp_p : p Fmt.t) (eq_a : a -> a -> bool) (eq_p : p -> p -> bool) =
  let pp ppf = function
    | `Unchanged -> Fmt.pf ppf "`Unchanged"
    | `Added v -> Fmt.pf ppf "`Added %a" pp_a v
    | `Removed v -> Fmt.pf ppf "`Removed %a" pp_a v
    | `Modified p -> Fmt.pf ppf "`Modified %a" pp_p p
  in
  let eq x y = match x, y with
    | `Unchanged, `Unchanged -> true
    | `Added a, `Added b -> eq_a a b
    | `Removed a, `Removed b -> eq_a a b
    | `Modified a, `Modified b -> eq_p a b
    | _ -> false
  in
  Alcotest.testable pp eq

let int_change_testable = change_testable Fmt.int (pp_atomic_patch Fmt.int) Int.equal (fun a b -> a = b)
let string_change_testable = change_testable Fmt.string (pp_atomic_patch Fmt.string) String.equal (fun a b -> a = b)

(** Test basic Myers algorithm functionality *)
let test_diff_list_basic () =

  (* Test case 1: Empty lists *)
  let result1 = diff_list (module IntDiffEq) [] [] in
  check (list int_change_testable) "empty lists" [] result1;

  (* Test case 2: Adding elements *)
  let result2 = diff_list (module IntDiffEq) [] [1; 2; 3] in
  check (list int_change_testable) "add elements" [`Added 1; `Added 2; `Added 3] result2;

  (* Test case 3: Removing elements *)
  let result3 = diff_list (module IntDiffEq) [1; 2; 3] [] in
  check (list int_change_testable) "remove elements" [`Removed 1; `Removed 2; `Removed 3] result3;

  (* Test case 4: No changes *)
  let result4 = diff_list (module IntDiffEq) [1; 2; 3] [1; 2; 3] in
  check (list int_change_testable) "no changes" [`Unchanged; `Unchanged; `Unchanged] result4;

  (* Test case 5: Simple replacement *)
  let result5 = diff_list (module IntDiffEq) [1; 2; 3] [1; 4; 3] in
  check (list int_change_testable) "replacement" [`Unchanged; `Removed 2; `Added 4; `Unchanged] result5;

  (* Test case 6: Mixed operations *)
  let result6 = diff_list (module IntDiffEq) [1; 2; 3] [2; 4] in
  (* Should be: remove 1, unchanged 2, remove 3, add 4 *)
  check (list int_change_testable) "mixed operations" [`Removed 1; `Unchanged; `Removed 3; `Added 4] result6;

  ()

(* Create a DIFFABLE_EQ module for strings *)
module StringDiffEq = struct
  type t = string
  let equal = String.equal
  module Patch = struct
    type t = string atomic_patch
    let is_empty = function {oldval; newval} -> oldval = newval
  end
  let diff a b = { oldval = a; newval = b}
end

(** Test Myers algorithm with strings *)
let test_diff_list_strings () =
  (* Test case 1: String replacement *)
  let result1 = diff_list (module StringDiffEq) ["a"; "b"; "c"] ["a"; "x"; "c"] in
  check (list string_change_testable) "string replacement" [`Unchanged; `Removed "b"; `Added "x"; `Unchanged] result1;

  (* Test case 2: String addition *)
  let result2 = diff_list (module StringDiffEq) ["hello"] ["hello"; "world"] in
  check (list string_change_testable) "string addition" [`Unchanged; `Added "world"] result2;

  (* Test case 3: String removal *)
  let result3 = diff_list (module StringDiffEq) ["foo"; "bar"; "baz"] ["foo"] in
  check (list string_change_testable) "string removal" [`Unchanged; `Removed "bar"; `Removed "baz"] result3;

  ()

(** Test Myers algorithm edge cases *)
let test_diff_list_edge_cases () =

  (* Test case 1: Single element identical *)
  let result1 = diff_list (module IntDiffEq) [42] [42] in
  check (list int_change_testable) "single identical" [`Unchanged] result1;

  (* Test case 2: Single element different *)
  let result2 = diff_list (module IntDiffEq) [1] [2] in
  check (list int_change_testable) "single different" [`Removed 1; `Added 2] result2;

  (* Test case 3: Duplicate elements *)
  let result3 = diff_list (module IntDiffEq) [1; 1; 1] [1; 1] in
  check (list int_change_testable) "duplicates" [`Unchanged; `Unchanged; `Removed 1] result3;

  (* Test case 4: Reverse order *)
  let result4 = diff_list (module IntDiffEq) [1; 2; 3] [3; 2; 1] in
  check (list int_change_testable) "reverse" [`Removed 1; `Removed 2; `Unchanged; `Added 2; `Added 1] result4;

  (* Test case 5: Completely different *)
  let result5 = diff_list (module IntDiffEq) [1; 2; 3] [4; 5; 6] in
  check (list int_change_testable) "completely different"
    [`Removed 1; `Removed 2; `Removed 3; `Added 4; `Added 5; `Added 6] result5;

  ()

(** Test Myers algorithm optimal edit distance *)
let test_diff_list_optimal () =

  (* Test case 1: Move detection [1,2,3] -> [2,3,1] *)
  let result1 = diff_list (module IntDiffEq) [1; 2; 3] [2; 3; 1] in
  (* Myers should find optimal solution with 4 operations: remove 1, unchanged 2, unchanged 3, add 1 *)
  check (list int_change_testable) "move detection" [`Removed 1; `Unchanged; `Unchanged; `Added 1] result1;

  (* Test case 2: Swap adjacent [1,2,3,4] -> [2,1,3,4] *)
  let result2 = diff_list (module IntDiffEq) [1; 2; 3; 4] [2; 1; 3; 4] in
  (* Should be: remove 1, unchanged 2, add 1, unchanged 3, unchanged 4 *)
  check (list int_change_testable) "adjacent swap" [`Removed 1; `Unchanged; `Added 1; `Unchanged; `Unchanged] result2;

  (* Test case 3: Complex rearrangement *)
  let result3 = diff_list (module IntDiffEq) [1; 2; 3; 4; 5] [3; 4; 1; 2; 5] in
  (* Myers should find minimal edit script *)
  check (list int_change_testable) "complex rearrangement"
    [`Removed 1; `Removed 2; `Unchanged; `Unchanged; `Added 1; `Added 2; `Unchanged] result3;
  ()



(** Test Myers algorithm with custom equality *)
let test_diff_list_custom () =
  let module CustomIntDiffEq = struct
    type t = int
    let equal a b = abs (a - b) <= 1  (* Equal if within 1 *)
    module Patch = struct
      type t = int atomic_patch
      let is_empty = function {oldval; newval} -> oldval = newval
    end
    let diff a b = { oldval = a; newval = b}
  end in

  let result = diff_list (module CustomIntDiffEq) [1; 5; 10] [2; 6; 10] in
  (* All should be unchanged since 1≈2, 5≈6, 10=10 *)
  (* All should be unchanged since 1≈2, 5≈6, 10=10 *)
  check (list int_change_testable) "custom equality unchanged" [`Unchanged; `Unchanged; `Unchanged] result;
  ()


(** Additional test to check correctness of Myers implementation *)
let test_myers_correctness () =
  (* Test case: [1; 2; 3; 4] -> [1; 3; 4; 5] *)
  let old_list = [1; 2; 3; 4] in
  let new_list = [1; 3; 4; 5] in

  let myers_result = diff_list (module IntDiffEq) old_list new_list in

  Printf.printf "Test case: [1; 2; 3; 4] -> [1; 3; 4; 5]\n";
  Printf.printf "Myers result length: %d\n" (List.length myers_result);

  (* Myers should produce reasonable number of operations *)
  (* Myers should produce reasonable number of operations *)
  (* 1->1(U), 2 removed, 3->3(U), 4->4(U), 5 added *)
  check (list int_change_testable) "Myers correctness"
    [`Unchanged; `Removed 2; `Unchanged; `Unchanged; `Added 5] myers_result;
  ()


(** Test merge_adjacent_changes function *)
let test_merge_adjacent_changes () =
  let diff a b = { oldval = a; newval = b } in

  (* Test case 1: Adjacent Removed+Added becomes Modified *)
  let changes1 = [`Removed 1; `Added 2; `Unchanged] in
  let result1 = merge_adjacent_changes ~diff changes1 in
  check (list int_change_testable) "adjacent removed+added"
    [`Modified { oldval = 1; newval = 2 }; `Unchanged] result1;

  (* Test case 2: Non-adjacent pairs stay separate *)
  let changes2 = [`Removed 1; `Unchanged; `Added 2] in
  let result2 = merge_adjacent_changes ~diff changes2 in
  check (list int_change_testable) "non-adjacent stays separate"
    [`Removed 1; `Unchanged; `Added 2] result2;

  (* Test case 3: Multiple adjacent pairs *)
  let changes3 = [`Removed 1; `Added 2; `Removed 3; `Added 4] in
  let result3 = merge_adjacent_changes ~diff changes3 in
  check (list int_change_testable) "multiple adjacent pairs"
    [`Modified { oldval = 1; newval = 2 }; `Modified { oldval = 3; newval = 4 }] result3;

  (* Test case 4: Empty list *)
  let result4 = merge_adjacent_changes ~diff [] in
  check (list int_change_testable) "empty list" [] result4;

  (* Test case 5: Only unchanged *)
  let changes5 = [`Unchanged; `Unchanged] in
  let result5 = merge_adjacent_changes ~diff changes5 in
  check (list int_change_testable) "only unchanged" [`Unchanged; `Unchanged] result5;

  (* Test case 6: Added then Removed (wrong order, should not merge) *)
  let changes6 = [`Added 2; `Removed 1] in
  let result6 = merge_adjacent_changes ~diff changes6 in
  check (list int_change_testable) "wrong order no merge"
    [`Added 2; `Removed 1] result6;

  ()


(** Test diff_list_merged - full integration test *)
let test_diff_list_merged () =
  (* Test case 1: Simple replacement [1;2;3] -> [4;2;3] *)
  let result1 = diff_list_merged (module IntDiffEq) [1; 2; 3] [4; 2; 3] in
  check (list int_change_testable) "simple replacement at start"
    [`Modified { oldval = 1; newval = 4 }; `Unchanged; `Unchanged] result1;

  (* Test case 2: Replacement in middle *)
  let result2 = diff_list_merged (module IntDiffEq) [1; 2; 3] [1; 5; 3] in
  check (list int_change_testable) "replacement in middle"
    [`Unchanged; `Modified { oldval = 2; newval = 5 }; `Unchanged] result2;

  (* Test case 3: No changes - should be same as diff_list *)
  let result3 = diff_list_merged (module IntDiffEq) [1; 2; 3] [1; 2; 3] in
  check (list int_change_testable) "no changes"
    [`Unchanged; `Unchanged; `Unchanged] result3;

  (* Test case 4: All replaced [1;2] -> [3;4]

     Note: We use semantic correctness checks rather than exact structural assertions
     because the Myers diff algorithm can produce multiple equally-valid minimal edit
     scripts for the same input. The linear-space variant (used by Git) may produce
     different orderings than the quadratic-space version depending on implementation
     details like diagonal iteration order. Both are correct as long as:
     - All old values appear in Removed or Modified operations
     - All new values appear in Added or Modified operations
     - The total edit distance is minimal

     See: https://blog.jcoglan.com/2017/03/22/myers-diff-in-linear-space-theory/ *)
  let result4 = diff_list_merged (module IntDiffEq) [1; 2] [3; 4] in

  (* Verify semantic correctness: all old and new values are accounted for *)
  let old_values = result4
    |> List.filter_map (function
        | `Removed v | `Modified {oldval=v; _} -> Some v
        | _ -> None)
    |> List.sort Int.compare in
  let new_values = result4
    |> List.filter_map (function
        | `Added v | `Modified {newval=v; _} -> Some v
        | _ -> None)
    |> List.sort Int.compare in

  (* All old values [1;2] should be present *)
  check (list int) "all replaced - all old values present" [1; 2] old_values;
  (* All new values [3;4] should be present *)
  check (list int) "all replaced - all new values present" [3; 4] new_values;
  ()


(** Exhaustive validity/minimality checking *)

(** A record with a numeric id and a value, for exercising the ID-based path. *)
type id_int_item = { id : int; value : int }

(** A DIFFABLE_ID module over [id_int_item]: identity is the [id] field. *)
module IdIntDiffEq = struct
  type t = id_int_item
  let equal a b = a = b
  let has_same_id a b = a.id = b.id
  let id_hash a = Hashtbl.hash a.id

  module Patch = struct
    type t = id_int_item atomic_patch
    let is_empty = function {oldval; newval} -> oldval = newval
  end

  let diff a b = { oldval = a; newval = b }
end

(** Independent minimal edit distance oracle (LCS dynamic program). Returns
    the minimum number of Added+Removed operations needed to turn [old_list]
    into [new_list] when elements satisfying [equal] can be matched. *)
let min_edit_distance ~equal old_list new_list =
  let old_arr = Array.of_list old_list in
  let new_arr = Array.of_list new_list in
  let n = Array.length old_arr and m = Array.length new_arr in
  let dp = Array.init (n + 1) (fun _ -> Array.make (m + 1) 0) in
  for i = n downto 0 do
    for j = m downto 0 do
      if i = n then dp.(i).(j) <- m - j
      else if j = m then dp.(i).(j) <- n - i
      else if equal old_arr.(i) new_arr.(j) then dp.(i).(j) <- dp.(i + 1).(j + 1)
      else dp.(i).(j) <- min (1 + dp.(i + 1).(j)) (1 + dp.(i).(j + 1))
    done
  done;
  dp.(0).(0)

(** Apply a change script to [old_list] and verify it exactly produces
    [new_list]: matched pairs must satisfy [equal] and match the outcome of
    [classify] (the on_match rule), and each Added/Removed must consume the
    expected element. Returns the number of Added+Removed operations, or None
    if the script is invalid. *)
let apply_check (type a p k)
    ~(equal : a -> a -> bool)
    ~(classify : a -> a -> (a, p, k) change)
    (old_list : a list) (new_list : a list) (script : (a, p, k) change list) : int option =
  let rec go old_list new_list ops = function
    | [] -> if old_list = [] && new_list = [] then Some ops else None
    | `Unchanged :: rest -> (
        match (old_list, new_list) with
        | o :: os, n :: ns when equal o n -> (
            match classify o n with
            | `Unchanged -> go os ns ops rest
            | `Modified _ | `Added _ | `Removed _ -> None)
        | _ -> None)
    | `Modified p :: rest -> (
        match (old_list, new_list) with
        | o :: os, n :: ns when equal o n -> (
            match classify o n with
            | `Modified q when q = p -> go os ns ops rest
            | `Unchanged | `Added _ | `Removed _ | `Modified _ -> None)
        | _ -> None)
    | `Added x :: rest -> (
        match new_list with
        | n :: ns when n = x -> go old_list ns (ops + 1) rest
        | _ -> None)
    | `Removed x :: rest -> (
        match old_list with
        | o :: os when o = x -> go os new_list (ops + 1) rest
        | _ -> None)
  in
  go old_list new_list 0 script

(** Enumerate all lists of length <= [max_len] over [alphabet]. *)
let all_lists alphabet max_len =
  let rec of_len len =
    if len = 0 then [[]]
    else List.concat_map (fun tail -> List.map (fun x -> x :: tail) alphabet) (of_len (len - 1))
  in
  List.concat_map of_len (List.init (max_len + 1) Fun.id)

let show_ints xs = "[" ^ String.concat ";" (List.map string_of_int xs) ^ "]"

let show_items xs =
  "[" ^ String.concat ";" (List.map (fun {id; value} -> Printf.sprintf "(%d,%d)" id value) xs) ^ "]"

(** Assert that [script] is a valid, minimal edit script for
    [old_list] -> [new_list]. *)
let check_valid_minimal ~equal ~classify ~show old_list new_list script =
  match apply_check ~equal ~classify old_list new_list script with
  | None ->
    fail
      (Printf.sprintf "invalid script for %s -> %s" (show old_list) (show new_list))
  | Some ops ->
    let expected = min_edit_distance ~equal old_list new_list in
    if ops <> expected then
      fail
        (Printf.sprintf "non-minimal (%d ops, expected %d) for %s -> %s" ops expected
           (show old_list) (show new_list))

(** Exhaustive sweep of diff_list over all int lists of length <= 6 over
    {0,1,2} (1,194,649 pairs, including duplicates). *)
let test_diff_list_exhaustive () =
  let equal = ( = ) in
  let classify o n = if o = n then `Unchanged else `Modified (IntDiffEq.diff o n) in
  let lists = all_lists [0; 1; 2] 6 in
  List.iter
    (fun old_list ->
       List.iter
         (fun new_list ->
            let script = diff_list (module IntDiffEq) old_list new_list in
            check_valid_minimal ~equal ~classify ~show:show_ints old_list new_list script)
         lists)
    lists

(** Exhaustive sweep of diff_list_generic with a non-trivial equality (same
    parity) that still produces [`Modified] matches. *)
let test_diff_list_generic_exhaustive () =
  let equal a b = a mod 2 = b mod 2 in
  let classify o n = if o = n then `Unchanged else `Modified (IntDiffEq.diff o n) in
  let lists = all_lists [0; 1; 2] 6 in
  List.iter
    (fun old_list ->
       List.iter
         (fun new_list ->
            let script = diff_list_generic ~compare:equal ~on_match:classify old_list new_list in
            check_valid_minimal ~equal ~classify ~show:show_ints old_list new_list script)
         lists)
    lists

(** Exhaustive sweep of diff_list_id over id/value records (ids {0,1},
    values {0,1}), lengths <= 5 — exercises ID matching and duplicates. *)
let test_diff_list_id_exhaustive () =
  let items =
    List.concat_map (fun id -> List.map (fun value -> { id; value }) [0; 1]) [0; 1]
  in
  let classify o n =
    let patch = IdIntDiffEq.diff o n in
    if IdIntDiffEq.Patch.is_empty patch then `Unchanged else `Modified patch
  in
  let lists = all_lists items 5 in
  List.iter
    (fun old_list ->
       List.iter
         (fun new_list ->
            let script = diff_list_id (module IdIntDiffEq) old_list new_list in
            check_valid_minimal ~equal:IdIntDiffEq.has_same_id ~classify ~show:show_items
              old_list new_list script)
         lists)
    lists

(** Seeded random longer lists: validity and minimality on deeper inputs. *)
let test_diff_list_random_large () =
  let state = Random.State.make [| 42 |] in
  let classify o n = if o = n then `Unchanged else `Modified (IntDiffEq.diff o n) in
  for _ = 1 to 300 do
    let old_list = List.init (Random.State.int state 41) (fun _ -> Random.State.int state 4) in
    let new_list = List.init (Random.State.int state 41) (fun _ -> Random.State.int state 4) in
    let script = diff_list (module IntDiffEq) old_list new_list in
    check_valid_minimal ~equal:( = ) ~classify ~show:show_ints old_list new_list script
  done

(** Apply a merged change script (where [`Modified] consumes an arbitrary
    old/new pair) and verify it exactly reproduces [new_list]. *)
let apply_check_merged old_list new_list script =
  let rec go old_list new_list = function
    | [] -> old_list = [] && new_list = []
    | `Unchanged :: rest -> (
        match (old_list, new_list) with
        | o :: os, n :: ns when o = n -> go os ns rest
        | _ -> false)
    | `Modified { oldval; newval } :: rest -> (
        match (old_list, new_list) with
        | o :: os, n :: ns when o = oldval && n = newval -> go os ns rest
        | _ -> false)
    | `Added x :: rest -> (
        match new_list with
        | n :: ns when n = x -> go old_list ns rest
        | _ -> false)
    | `Removed x :: rest -> (
        match old_list with
        | o :: os when o = x -> go os new_list rest
        | _ -> false)
  in
  go old_list new_list script

(** Semantic coverage of diff_list_merged on duplicate-heavy inputs: the
    merged script must apply exactly, regardless of which equally-minimal
    alignment was chosen. *)
let test_diff_list_merged_semantic () =
  let cases =
    [
      ([1; 1; 2], [1; 2; 2]);
      ([0; 0; 0], [0]);
      ([0], [0; 0; 0]);
      ([1; 2; 3], [3; 2; 1]);
      ([1; 2], [3; 4]);
      ([1; 1; 1; 1], [1; 1; 2; 2]);
      ([1; 2; 1; 2], [2; 1; 2; 1]);
    ]
  in
  List.iter
    (fun (old_list, new_list) ->
       let script = diff_list_merged (module IntDiffEq) old_list new_list in
       if not (apply_check_merged old_list new_list script) then
         fail
           (Printf.sprintf "merged script does not apply for %s -> %s"
              (show_ints old_list) (show_ints new_list)))
    cases

(** Alcotest test suite setup. *)
let () =
  run "Diff List Algorithms" [
    "Myers Diff", [
      test_case "Test basic Myers functionality" `Quick test_diff_list_basic;
      test_case "Test Myers with strings" `Quick test_diff_list_strings;
      test_case "Test edge cases" `Quick test_diff_list_edge_cases;
      test_case "Test optimal edit distance" `Quick test_diff_list_optimal;
      test_case "Test custom equality" `Quick test_diff_list_custom;
      test_case "Test correctness" `Quick test_myers_correctness;
    ];
    "Merge Adjacent Changes", [
      test_case "Test merge_adjacent_changes" `Quick test_merge_adjacent_changes;
      test_case "Test diff_list_merged" `Quick test_diff_list_merged;
      test_case "Test diff_list_merged semantic coverage" `Quick test_diff_list_merged_semantic;
    ];
    "Exhaustive Validity", [
      test_case "Exhaustive diff_list over {0,1,2} lengths <= 6" `Quick test_diff_list_exhaustive;
      test_case "Exhaustive diff_list_generic with parity equality" `Quick test_diff_list_generic_exhaustive;
      test_case "Exhaustive diff_list_id over id/value records" `Quick test_diff_list_id_exhaustive;
      test_case "Seeded random large lists" `Quick test_diff_list_random_large;
    ]
  ]
