(* PPX rejection tests (5364b2a, 44b9f78, 9d712d6, 182840c).

   The derivers' rejections surface as [%%ocaml.error "..."] extensions in
   the expanded AST — the compile fails downstream when the compiler
   consumes the AST. So a fixture "fails to compile" iff its expansion
   carries an ocaml.error node, and the expected message must appear inside
   it. Positive-control fixtures must expand cleanly: an unexpected
   ocaml.error there means a rejection over-fired.

   Expansion runs in-process via [Ppxlib.Driver.map_structure] after linking
   the real derivers — no subprocess and no shell, so the suite cannot be
   broken by command-line mangling (cmd.exe on Windows made the earlier
   standalone-driver version unrunnable there). *)

let () =
  ignore
    ( Ppx_impl.Ppx_patch.deriver,
      Ppx_impl.Ppx_view_spec.deriver,
      Ppx_impl.Ppx_id.deriver )

(* [ocaml.error] payloads are [PStr "message"]. *)
let message_of_payload = function
  | Ppxlib.PStr
      [ { pstr_desc =
            Pstr_eval
              ( { pexp_desc = Pexp_constant (Pconst_string (msg, _, _)); _ },
                _ )
        ; _ } ] -> Some msg
  | _ -> None

let error_messages_of_structure structure =
  let folder =
    object
      inherit ['acc] Ppxlib.Ast_traverse.fold

      method! extension (name, payload) acc =
        if name.Ppxlib.txt = "ocaml.error" then
          match message_of_payload payload with
          | Some msg -> msg :: acc
          | None -> "<unprintable ocaml.error payload>" :: acc
        else acc
    end
  in
  List.rev (folder#structure structure [])

(* [expand_fixture path] expands [path] with the linked derivers and returns
   every ocaml.error message; a deriver or the parser aborting with
   [Location.Error] contributes that message alone. *)
let expand_fixture path =
  match
    let ic = open_in_bin path in
    Fun.protect
      ~finally:(fun () -> close_in ic)
      (fun () ->
         let lexbuf = Lexing.from_channel ic in
         lexbuf.Lexing.lex_curr_p <-
           { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = path };
         Ppxlib.Driver.map_structure (Ppxlib.Parse.implementation lexbuf))
  with
  | structure -> error_messages_of_structure structure
  | exception Ppxlib.Location.Error e -> [ Ppxlib.Location.Error.message e ]

let contains ~needle ~haystack =
  let ln = String.length needle and lh = String.length haystack in
  let rec go i = i + ln <= lh && (String.sub haystack i ln = needle || go (i + 1)) in
  ln = 0 || go 0

let cases =
  [
    ("view_spec_non_record.ml", Some "Cannot derive view_spec for non-record types");
    ("view_spec_multi_decl.ml", Some "multiple mutually defined types");
    ("patch_skip_view_label.ml", Some "cannot also carry");
    ("patch_variant_attr.ml", Some "Cannot derive patch for variant type with attributes on constructor A");
    ("view_spec_context_on_atomic.ml", Some "there is no section placeholder to fill");
    ("variant_dispatch_on_record.ml", Some "view.variant_dispatch is for variant (sum) types");
    ("variant_dispatch_on_open.ml", Some "view.variant_dispatch is for variant (sum) types");
    ("variant_dispatch_on_abstract.ml", Some "view.variant_dispatch is for variant (sum) types");
    ("variant_dispatch_bad_arity.ml", Some "4 string literals");
    ("variant_dispatch_mixed_mode.ml", Some "mixes self-routed and builder-routed");
    ("variant_dispatch_skip_in_self.ml", Some "cannot skip constructors");
    ("variant_dispatch_no_type_label.ml", Some "requires [@view.type_label]");
    ("variant_dispatch_dup_patch.ml", Some "duplicate patch constructor");
    ("variant_dispatch_dup_builder.ml", Some "duplicate builder label");
    ("variant_dispatch_skip_with_builder.ml", Some "skip entry cannot carry a builder label");
    ("valid_record.ml", None);
    ("valid_variant_deprecated.ml", None);
  ]

let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "usage: ppx_negative <fixtures-dir>";
    exit 2
  end;
  let dir = Sys.argv.(1) in
  let failures = ref 0 in
  List.iter
    (fun (name, expected) ->
       let fixture = Filename.concat dir name in
       let messages = expand_fixture fixture in
       let outcome =
         match expected with
         | Some msg ->
           if messages = [] then Error "no rejection raised"
           else if
             not (List.exists (fun m -> contains ~needle:msg ~haystack:m) messages)
           then
             Error
               (Printf.sprintf "rejection messages do not mention %S: %s" msg
                  (String.concat " | " messages))
           else Ok ()
         | None ->
           if messages = [] then Ok ()
           else
             Error
               (Printf.sprintf "unexpected rejection: %s"
                  (String.concat " | " messages))
       in
       match outcome with
       | Ok () -> Printf.printf "[ppx-negative] PASS %s\n" name
       | Error why ->
         incr failures;
         Printf.printf "[ppx-negative] FAIL %s: %s\n" name why)
    cases;
  if !failures > 0 then exit 1
