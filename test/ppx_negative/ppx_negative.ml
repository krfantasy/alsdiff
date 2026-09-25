(* PPX rejection tests (5364b2a, 44b9f78, 9d712d6, 182840c).

   The derivers' rejections surface as [%%ocaml.error "..."] extensions in
   the standalone driver's expanded output — the compile fails downstream
   when the compiler consumes the AST. So a fixture "fails to compile" iff
   its expansion carries an ocaml.error node, and the expected message must
   appear inside it. Positive-control fixtures must expand cleanly: an
   unexpected ocaml.error there means a rejection over-fired. *)

let read_file path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let contains ~needle ~haystack =
  let ln = String.length needle and lh = String.length haystack in
  let rec go i = i + ln <= lh && (String.sub haystack i ln = needle || go (i + 1)) in
  ln = 0 || go 0

(* [run_driver driver fixture] expands [fixture] with the standalone ppx
   driver and returns the expanded source. *)
let run_driver driver fixture =
  let out = Filename.temp_file "alsdiff_ppx_negative" ".ml" in
  let cmd =
    Printf.sprintf "%s -impl %s -o %s"
      (Filename.quote driver) (Filename.quote fixture) (Filename.quote out)
  in
  if Sys.command cmd <> 0 then None
  else
    let content = read_file out in
    Sys.remove out;
    Some content

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
  if Array.length Sys.argv <> 3 then begin
    prerr_endline "usage: ppx_negative <driver.exe> <fixtures-dir>";
    exit 2
  end;
  let driver = Sys.argv.(1) and dir = Sys.argv.(2) in
  let failures = ref 0 in
  List.iter
    (fun (name, expected) ->
       let fixture = Filename.concat dir name in
       let outcome =
         match run_driver driver fixture with
         | None -> Error "driver did not run"
         | Some output ->
           let rejected = contains ~needle:"ocaml.error" ~haystack:output in
           match expected with
           | Some msg ->
             if not rejected then Error "no rejection raised"
             else if not (contains ~needle:msg ~haystack:output) then
               Error (Printf.sprintf "rejection message does not mention %S" msg)
             else Ok ()
           | None ->
             if rejected then
               Error (Printf.sprintf "unexpected rejection:\n%s"
                        (String.concat "\n" (List.filter (fun l -> String.length l > 0)
                                               (List.tl (String.split_on_char '\n' output)))))
             else Ok ()
       in
       match outcome with
       | Ok () -> Printf.printf "[ppx-negative] PASS %s\n" name
       | Error why ->
         incr failures;
         Printf.printf "[ppx-negative] FAIL %s: %s\n" name why)
    cases;
  if !failures > 0 then exit 1
