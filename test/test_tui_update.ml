open Alsdiff_tui_lib

let node ~path change =
  {
    Model.path;
    label = String.concat "/" path;
    change;
    depth = List.length path - 1;
    is_expandable = false;
    children = [];
  }

let diff_model ?(cursor = 0) ?(nav_back = []) ?(nav_forward = []) (nodes : Model.tree_node list) =
  {
    (Model.init []) with
    Model.flat_nodes = nodes;
    cursor_index = cursor;
    nav_back;
    nav_forward;
  }

(* BUG-21: backspace must drop the last UTF-8 *character* (walk back over
   continuation bytes), not a single byte — byte-wise deletion used to leave a
   corrupt partial character in the query. *)
let test_backspace_deletes_utf8_char () =
  let base = { (diff_model []) with Model.search_mode = true; search_query = Some "h" } in
  let typed = Update.update_search base "\xC3\xA9" in
  Alcotest.(check string) "typing é appends both bytes"
    "hé" (Option.value typed.search_query ~default:"");
  let after_bs = Update.update_search typed "\127" in
  Alcotest.(check string) "backspace after é drops both bytes of é"
    "h" (Option.value after_bs.search_query ~default:"");
  Alcotest.(check int) "no partial byte left behind"
    1 (String.length (Option.value after_bs.search_query ~default:""));
  let single = Update.update_search { base with Model.search_query = Some "ab" } "\127" in
  Alcotest.(check string) "backspace on ASCII drops one char"
    "a" (Option.value single.search_query ~default:"")

(* BUG-24: every real manual move pushes the pre-move position onto nav_back
   and clears nav_forward, so NavBack can always return. *)
let test_manual_move_pushes_history () =
  let nodes = [ node ~path:["a"] Added; node ~path:["b"] Modified ] in
  let model = diff_model ~cursor:0 ~nav_forward:[ [ "lost" ] ] nodes in
  let moved, _ = Update.update model Msg.MoveDown in
  Alcotest.(check int) "cursor moved down" 1 moved.cursor_index;
  Alcotest.(check int) "pre-move path pushed to nav_back" 1 (List.length moved.nav_back);
  (match moved.nav_back with
   | [ [ "a" ] ] -> ()
   | back -> Alcotest.fail ("unexpected nav_back: " ^ String.concat "," (List.concat back)));
  Alcotest.(check int) "nav_forward cleared" 0 (List.length moved.nav_forward)

(* No-op moves (clamped at the ends) must not pollute nav_back — otherwise
   NavBack appears dead after repeated edge presses (0cd3c7f). *)
let test_noop_move_skips_history () =
  let nodes = [ node ~path:["a"] Added; node ~path:["b"] Modified ] in
  let model = diff_model ~cursor:1 nodes in
  let moved, _ = Update.update model Msg.MoveDown in
  Alcotest.(check int) "cursor stays clamped at the end" 1 moved.cursor_index;
  Alcotest.(check int) "no history entry for a no-op move" 0 (List.length moved.nav_back)

(* BUG-22: arrow/page/home/end keys must be swallowed on the Help and Stats
   screens (the footer says "Press Esc to close"), while Esc still closes and
   Diff mode keeps its navigation bindings. *)
let msg_name = function
  | Msg.MoveDown -> "MoveDown"
  | Msg.MoveUp -> "MoveUp"
  | Msg.HideHelp -> "HideHelp"
  | Msg.HideStats -> "HideStats"
  | _ -> "?"

let press k = Mosaic.Event.Key.of_input (Matrix.Input.Key.make k)

let test_help_stats_swallow_navigation_keys () =
  let key mode k =
    Option.map msg_name
      (Keymap.handle_key ~mode ~search_mode:false ~export_selector_active:false (press k))
  in
  let module K = Matrix.Input.Key in
  Alcotest.(check (option string)) "Down swallowed on Help" None
    (key Model.Help K.Down);
  Alcotest.(check (option string)) "Up swallowed on Stats" None
    (key Model.Stats K.Up);
  Alcotest.(check (option string)) "PageDown swallowed on Help" None
    (key Model.Help K.Page_down);
  Alcotest.(check (option string)) "Home swallowed on Help" None
    (key Model.Help K.Home);
  Alcotest.(check (option string)) "End swallowed on Stats" None
    (key Model.Stats K.End);
  Alcotest.(check (option string)) "Esc still closes Help" (Some "HideHelp")
    (key Model.Help K.Escape);
  Alcotest.(check (option string)) "Esc still closes Stats" (Some "HideStats")
    (key Model.Stats K.Escape);
  Alcotest.(check (option string)) "Down still navigates in Diff" (Some "MoveDown")
    (key Model.Diff K.Down)

let () =
  Alcotest.run "TuiUpdate" [
    "update_search", [
      Alcotest.test_case "backspace deletes a UTF-8 character" `Quick test_backspace_deletes_utf8_char;
    ];
    "history", [
      Alcotest.test_case "manual move pushes history" `Quick test_manual_move_pushes_history;
      Alcotest.test_case "no-op move skips history" `Quick test_noop_move_skips_history;
    ];
    "keymap", [
      Alcotest.test_case "help/stats swallow navigation keys" `Quick test_help_stats_swallow_navigation_keys;
    ];
  ]
