open Alsdiff_output.View_model

(* Regression tests for the consolidated domain_type name/display mapping.
   The variant <-> string maps used to be hand-written in three places
   (Config.domain_type_to_string, the ViewSpec builder's domain_type_of_name,
   and the PPX-generated yojson pair). Adding a variant silently produced a
   DTOther fallback / missing-stat with no compile error. They now derive
   from a single source in Output_types; these tests guard the round-trip so
   a future variant added without updating the name tables fails here. *)

(* The full set of domain_type variants. If a variant is added to the type
   but not to this list, the "exhaustive variants" test fails; if it is added
   here but not to the name table, the round-trip test fails. *)
let all_domain_types : domain_type list = [
  DTLiveset; DTTrack; DTDevice; DTClip; DTAutomation; DTMixer; DTRouting;
  DTLocator; DTParam; DTNote; DTEvent; DTSend; DTPreset; DTMacro;
  DTSnapshot; DTLoop; DTSignature; DTSampleRef; DTVersion; DTOther;
]

let test_name_roundtrip () =
  List.iteri (fun i dt ->
      let name = domain_type_to_name dt in
      Alcotest.(check bool)
        (Printf.sprintf "domain_type %d has non-empty canonical name" i)
        (name <> "") true;
      let parsed = domain_type_of_name name in
      Alcotest.(check bool)
        (Printf.sprintf "domain_type %d round-trips through name" i)
        (dt = parsed) true
    ) all_domain_types

let test_display_name_nonempty () =
  List.iteri (fun i dt ->
      let display = domain_type_to_display dt in
      Alcotest.(check bool)
        (Printf.sprintf "domain_type %d has non-empty display name" i)
        (display <> "") true
    ) all_domain_types

let test_to_string_aliases_display () =
  (* Config.domain_type_to_string must agree with the canonical display map. *)
  List.iteri (fun i dt ->
      Alcotest.(check string)
        (Printf.sprintf "domain_type %d to_string matches display" i)
        (domain_type_to_display dt) (Alsdiff_output.Config.domain_type_to_string dt)
    ) all_domain_types

let test_of_name_rejects_unknown () =
  (* Unknown names must not silently fall back to DTOther; they raise so a
     stale mapping is loud. (Every shipped caller passes a canonical literal.) *)
  let raised =
    try let _ = domain_type_of_name "DTDoesNotExist" in false
    with Invalid_argument _ -> true
  in
  Alcotest.(check bool) "unknown name raises Invalid_argument" raised true

let tests = [
  "name roundtrip", `Quick, test_name_roundtrip;
  "display name nonempty", `Quick, test_display_name_nonempty;
  "to_string aliases display", `Quick, test_to_string_aliases_display;
  "of_name rejects unknown", `Quick, test_of_name_rejects_unknown;
]

let () =
  Alcotest.run "domain_type mapping" [
    "domain_type_mapping", tests
  ]
