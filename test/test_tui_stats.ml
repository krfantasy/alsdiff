open Alsdiff_tui_lib.Model

(* Regression (BUG-18): the "By Domain" breakdown must count the whole
   subtree, not just top-level nodes — it used to fold over flat_nodes
   without recursing into children, so a single root item with hundreds of
   children reported "Liveset — 1". *)

let node ~path change children =
  let label = match List.rev path with last :: _ -> last | [] -> "Root" in
  { path; label; change; depth = List.length path - 1; is_expandable = children <> [];
    children }

let test_domain_counts_include_children () =
  let root = node ~path:["Liveset"] Modified [
      node ~path:["Liveset"; "Track 1"] Added [];
      node ~path:["Liveset"; "Track 2"] Removed [];
      node ~path:["Liveset"; "Track 3"] Added [
        node ~path:["Liveset"; "Track 3"; "Devices"] Modified [];
      ];
    ] in
  let (total, domains) = compute_stats [root] in
  Alcotest.(check int) "total counts whole tree" 5 total.total;
  Alcotest.(check int) "domain list has one entry" 1 (List.length domains);
  (match domains with
   | [d] ->
     Alcotest.(check string) "domain name" "Liveset" d.name;
     Alcotest.(check int) "domain total includes children" 5 d.changes.total;
     Alcotest.(check int) "domain added count" 2 d.changes.added;
     Alcotest.(check int) "domain removed count" 1 d.changes.removed;
     Alcotest.(check int) "domain modified count" 2 d.changes.modified
   | _ -> Alcotest.fail "unexpected domain list")

let test_separate_domains () =
  let a = node ~path:["Liveset"] Modified [
      node ~path:["Liveset"; "T"] Added [] ] in
  let b = node ~path:["Master"] Removed [] in
  let (_, domains) = compute_stats [a; b] in
  Alcotest.(check int) "two domains" 2 (List.length domains);
  (match domains with
   | [d1; d2] ->
     Alcotest.(check int) "Liveset subtree total" 2 d1.changes.total;
     Alcotest.(check int) "Master total" 1 d2.changes.total
   | _ -> Alcotest.fail "unexpected domain list")

let () =
  Alcotest.run "TuiStats" [
    "compute_stats", [
      Alcotest.test_case "domain counts include children" `Quick test_domain_counts_include_children;
      Alcotest.test_case "domains stay separate" `Quick test_separate_domains;
    ];
  ]
