open Msg

(* Uchar.to_char raises for any codepoint above U+00FF; command keys are
   matched on Latin-1 only, and anything else is ignored instead of crashing. *)
let char_of c =
  if Uchar.to_int c <= 0xFF then Some (Uchar.to_char c) else None

(* Full UTF-8 encoding for search input, so Latin-1-supplement/CJK/emoji
   queries work. Note: U+0080-U+00FF must be 2-byte UTF-8 (e.g. é U+00E9
   is C3 A9), not single Latin-1 bytes. *)
let utf8_of_uchar c =
  let b = Buffer.create 4 in
  Buffer.add_utf_8_uchar b c;
  Buffer.contents b

let char_to_msg_browser c =
  match c with
  | 'q' -> Some Quit
  | '?' -> Some ToggleHelp
  | 'j' -> Some MoveDown
  | 'k' -> Some MoveUp
  (* Documented in the browser help screen ("↑ / k / p", "↓ / j / n"). *)
  | 'p' -> Some MoveUp
  | 'n' -> Some MoveDown
  | _ -> None

let char_to_msg_diff c =
  match c with
  | 'q' -> Some BackToBrowser
  | 'd' -> Some CycleDetailMode
  | '/' -> Some StartSearch
  | 'f' -> Some (ToggleChangeFilter None)
  | '?' -> Some ToggleHelp
  | 's' -> Some ToggleStats
  | '[' -> Some NavBack
  | ']' -> Some NavForward
  | 'E' -> Some ShowExportSelector
  | 'z' -> Some EnterFocus
  | 'h' -> Some MoveLeft
  | 'j' -> Some MoveDown
  | 'k' -> Some MoveUp
  | 'l' -> Some MoveRight
  | ' ' -> Some ToggleExpand
  | _ -> None

let char_to_msg_export_preview c =
  match c with
  | 's' -> Some ExportSaveToFile
  | 'c' -> Some ExportToClipboard
  | 'p' -> Some ExportToStdout
  | _ -> None

let handle_key ~(mode : Model.mode) ~(search_mode : bool) ~(export_selector_active : bool)
    (ev : Mosaic.Event.key) : t option =
  let key_data = Mosaic.Event.Key.data ev in
  let static_screen = (mode = Model.Help || mode = Model.Stats) in
  match key_data.Matrix.Input.Key.key with
  | Matrix.Input.Key.Char c ->
    if search_mode then
      Some (UpdateSearch (utf8_of_uchar c))
    else
      (match char_of c with
       | None -> None
       | Some c ->
         (match mode with
          | Model.Browser -> char_to_msg_browser c
          | Model.Diff -> char_to_msg_diff c
          | Model.Export ->
            if export_selector_active then None
            else char_to_msg_export_preview c
          | Model.Help | Model.Stats -> None))
  | Matrix.Input.Key.Up when static_screen -> None
  | Matrix.Input.Key.Down when static_screen -> None
  | Matrix.Input.Key.Left when static_screen -> None
  | Matrix.Input.Key.Right when static_screen -> None
  | Matrix.Input.Key.Up ->
    if export_selector_active then Some (MoveExportSelection (-1))
    else if mode = Model.Export && not search_mode then Some (ExportScroll (-1))
    else if search_mode then None else Some MoveUp
  | Matrix.Input.Key.Down ->
    if export_selector_active then Some (MoveExportSelection 1)
    else if mode = Model.Export && not search_mode then Some (ExportScroll 1)
    else if search_mode then None else Some MoveDown
  | Matrix.Input.Key.Left -> if search_mode then None else Some MoveLeft
  | Matrix.Input.Key.Right -> if search_mode then None else Some MoveRight
  | Matrix.Input.Key.Enter ->
    if export_selector_active then Some ExecuteExport
    else if search_mode then Some EndSearch
    else
      (match mode with
       | Model.Browser -> Some BrowserActivate
       | Model.Diff -> Some ToggleExpand
       | Model.Export | Model.Help | Model.Stats -> None)
  | Matrix.Input.Key.Escape ->
    if export_selector_active then Some HideExportSelector
    else if mode = Model.Export then Some HideExport
    else if search_mode then Some ClearSearch
    else
      (match mode with
       | Model.Browser -> Some BrowserGoUp
       | Model.Diff -> Some BackToBrowser
       | Model.Help -> Some HideHelp
       | Model.Stats -> Some HideStats
       | Model.Export -> Some HideExport)
  | Matrix.Input.Key.Backspace ->
    if search_mode then Some (UpdateSearch "\127") else None
  | Matrix.Input.Key.Page_up when static_screen -> None
  | Matrix.Input.Key.Page_down when static_screen -> None
  | Matrix.Input.Key.Home when static_screen -> None
  | Matrix.Input.Key.End when static_screen -> None
  | Matrix.Input.Key.Page_up ->
    if export_selector_active then None
    else if mode = Model.Export then Some (ExportScroll (-20))
    else if search_mode then None else Some PageUp
  | Matrix.Input.Key.Page_down ->
    if export_selector_active then None
    else if mode = Model.Export then Some (ExportScroll 20)
    else if search_mode then None else Some PageDown
  | Matrix.Input.Key.Home ->
    if export_selector_active then None
    else if mode = Model.Export then Some (ExportScroll (-max_int))
    else if search_mode then None else Some MoveToStart
  | Matrix.Input.Key.End ->
    if export_selector_active then None
    else if mode = Model.Export then Some (ExportScroll max_int)
    else if search_mode then None else Some MoveToEnd
  | _ -> None
