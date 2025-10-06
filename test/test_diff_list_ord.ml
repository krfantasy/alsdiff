open Alcotest
open Live_git_lib
open Live_git_lib.Diff

(** Test the sequence-based diff function *)
let test_diff_list_ord () =
  (* Test with simple integer lists *)
  let (module IntEq) = (module Equality.IntEq : Equality.EQUALABLE with type t = int) in

  (* Test case 1: Empty lists *)
  let result1 = diff_list_ord (module IntEq) [] [] in
  check int "empty lists" 0 (List.length result1);

  (* Test case 2: Adding elements *)
  let result2 = diff_list_ord (module IntEq) [] [1; 2; 3] in
  check int "add elements length" 3 (List.length result2);

  (* Test case 3: Removing elements *)
  let result3 = diff_list_ord (module IntEq) [1; 2; 3] [] in
  check int "remove elements length" 3 (List.length result3);

  (* Test case 4: No changes *)
  let result4 = diff_list_ord (module IntEq) [1; 2; 3] [1; 2; 3] in
  check int "no changes length" 3 (List.length result4);

  (* Test case 5: Mixed changes *)
  let result5 = diff_list_ord (module IntEq) [1; 2; 3] [1; 4; 3] in
  (* For [1; 2; 3] -> [1; 4; 3], we expect:
     - `Unchanged for 1 (index 0)
     - `Removed for 2 (index 1)
     - `Added for 4 (index 1)
     - `Unchanged for 3 (index 2)
     So that's 4 changes total *)
  check int "mixed changes length" 4 (List.length result5);

  ()

(** Additional tests for diff_list_ord *)
let test_diff_list_ord_advanced () =
  (* Test with simple integer lists *)
  let (module IntEq) = (module Equality.IntEq : Equality.EQUALABLE with type t = int) in

  (* Test case 1: No changes - verify all unchanged *)
  let result1 = diff_list_ord (module IntEq) [1; 2; 3; 4] [1; 2; 3; 4] in
  check int "ord no changes length" 4 (List.length result1);
  let all_unchanged = List.for_all (function `Unchanged -> true | _ -> false) result1 in
  check bool "ord all unchanged" true all_unchanged;

  (* Test case 2: Replacement in middle *)
  let result2 = diff_list_ord (module IntEq) [1; 2; 3] [1; 9; 3] in
  check int "ord middle replacement length" 4 (List.length result2);

  (* Test case 3: Multiple changes *)
  let result3 = diff_list_ord (module IntEq) [1; 2; 3; 4] [1; 5; 6; 4] in
  check int "ord multiple changes length" 6 (List.length result3);

  (* Test case 4: Empty result for identical empty lists *)
  let result4 = diff_list_ord (module IntEq) [] [] in
  check int "ord both empty length" 0 (List.length result4);

  ()

(** Test diff_list_ord with different data types *)
let test_diff_list_ord_strings () =
  (* Test with string lists *)
  let module StringEq = struct
    type t = string
    let equal = String.equal
  end in

  let (module StrEq) = (module StringEq : Equality.EQUALABLE with type t = string) in

  (* Test case 1: Basic string diffing *)
  let result1 = diff_list_ord (module StrEq) ["a"; "b"; "c"] ["a"; "x"; "c"] in
  check int "ord strings replacement length" 4 (List.length result1);

  (* Test case 2: String addition *)
  let result2 = diff_list_ord (module StrEq) ["hello"] ["hello"; "world"] in
  check int "ord strings add length" 2 (List.length result2);

  (* Test case 3: String removal *)
  let result3 = diff_list_ord (module StrEq) ["foo"; "bar"; "baz"] ["foo"] in
  check int "ord strings remove length" 3 (List.length result3);

  ()

(** Alcotest test suite setup. *)
let () =
  run "Diff List Ord" [
    "diff-list-ord", [ test_case "Test sequence-based diffing" `Quick test_diff_list_ord ];
    "diff-list-ord-advanced", [ test_case "Test advanced cases" `Quick test_diff_list_ord_advanced ];
    "diff-list-ord-strings", [ test_case "Test with strings" `Quick test_diff_list_ord_strings ];
  ]
