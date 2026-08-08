open Alcotest

(* argv set by the dune rule below: alsdiff-bin base modified corrupt *)
let alsdiff_bin = Sys.argv.(1)
let base_file = Sys.argv.(2)
let modified_file = Sys.argv.(3)
let corrupt_file = Sys.argv.(4)

type run_result = {
  exit_code : int;
  stdout : string;
  stderr : string;
}

(* Run the real alsdiff binary with [args] appended after the program path. *)
let run_alsdiff (args : string list) : run_result =
  let argv = Array.of_list (alsdiff_bin :: args) in
  let out_file = Filename.temp_file "alsdiff_test_stdout" ".txt" in
  let err_file = Filename.temp_file "alsdiff_test_stderr" ".txt" in
  let out_fd = Unix.openfile out_file [ Unix.O_WRONLY; Unix.O_TRUNC; Unix.O_CREAT ] 0o644 in
  let err_fd = Unix.openfile err_file [ Unix.O_WRONLY; Unix.O_TRUNC; Unix.O_CREAT ] 0o644 in
  let dev_null = Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0 in
  (* create_process prog args stdin stdout stderr *)
  let pid = Unix.create_process alsdiff_bin argv dev_null out_fd err_fd in
  Unix.close out_fd;
  Unix.close err_fd;
  Unix.close dev_null;
  let _, status = Unix.waitpid [] pid in
  let exit_code = match status with Unix.WEXITED n -> n | _ -> -1 in
  let read_file f = In_channel.with_open_bin f In_channel.input_all in
  let stdout = read_file out_file in
  let stderr = read_file err_file in
  Sys.remove out_file;
  Sys.remove err_file;
  { exit_code; stdout; stderr }

let check_exit expected (r : run_result) =
  check int "exit code" expected r.exit_code

let contains (needle : string) (haystack : string) : bool =
  let n = String.length needle and m = String.length haystack in
  let rec go i = i + n <= m && (String.sub haystack i n = needle || go (i + 1)) in
  n <= m && go 0

let check_contains needle (r : run_result) =
  check bool (Printf.sprintf "output contains %S" needle) true (contains needle r.stdout)

let git_args path old_f new_f =
  [ path; old_f; "aaaa1111"; "100644"; new_f; "bbbb2222"; "100644" ]

(* Git appends "new-path" and "similarity index N%" for renames/copies *)
let rename_args path old_f new_f new_path similarity =
  [ path; old_f; "aaaa1111"; "100644"; new_f; "bbbb2222"; "100644"; new_path; similarity ]

(* git passes "/dev/null" (with "." for hex/mode) for the missing side *)
let add_args path new_f = [ path; "/dev/null"; "."; "."; new_f; "bbbb2222"; "100644" ]
let delete_args path old_f = [ path; old_f; "aaaa1111"; "100644"; "/dev/null"; "."; "." ]

let test_unchanged_exit_0 () =
  let r = run_alsdiff ("--git" :: git_args "song.als" base_file base_file) in
  check_exit 0 r;
  (* The tree renderer emits a trailing newline even without changes; assert
     there is no actual diff content instead. *)
  check bool "no diff content when unchanged" true (not (contains "MidiTrack" r.stdout));
  check bool "no diff header when unchanged" true (not (contains "diff --git" r.stdout))

let test_modified_exit_1 () =
  let r = run_alsdiff ("--git" :: git_args "song.als" base_file modified_file) in
  check_exit 1 r;
  check_contains "1-Tela-Renamed" r;
  check bool "diff header present" true (contains "diff --git a/song.als b/song.als" r.stdout)

let test_rename_no_change_9_args () =
  let r =
    run_alsdiff
      ("--git" :: rename_args "song.als" base_file base_file "renamed.als" "similarity index 100%")
  in
  check_exit 0 r

let test_rename_with_change_9_args () =
  let r =
    run_alsdiff
      ("--git" :: rename_args "song.als" base_file modified_file "renamed.als" "similarity index 50%")
  in
  check_exit 1 r;
  check_contains "1-Tela-Renamed" r

let test_wrong_arg_count_exit_2 () =
  (* 6 args is malformed: must not crash the driver with a backtrace *)
  let r =
    run_alsdiff
      ("--git" :: [ "song.als"; base_file; "aaaa1111"; "100644"; modified_file; "bbbb2222" ])
  in
  check_exit 2 r;
  check bool "usage error on stderr" true (contains "requires 7 positional arguments" r.stderr);
  check bool "no backtrace" true (not (contains "Raised at" r.stderr))

let test_eight_args_exit_2 () =
  (* 8 args (missing the trailing similarity message) is malformed *)
  let r =
    run_alsdiff
      ("--git"
       :: [ "song.als"; base_file; "aaaa1111"; "100644"; modified_file; "bbbb2222"; "100644"; "renamed.als" ])
  in
  check_exit 2 r

let test_added_file () =
  let r = run_alsdiff ("--git" :: add_args "song.als" base_file) in
  check_exit 1 r;
  check_contains "MidiTrack" r;
  check_contains "Added" r;
  check bool "no spurious version diff" true (not (contains "Version" r.stdout));
  check bool "no spurious main track diff" true (not (contains "MainTrack" r.stdout))

let test_deleted_file () =
  let r = run_alsdiff ("--git" :: delete_args "song.als" base_file) in
  check_exit 1 r;
  check_contains "MidiTrack" r;
  check_contains "Removed" r;
  check bool "no spurious version diff" true (not (contains "Version" r.stdout));
  check bool "no backtrace" true (not (contains "Raised at" r.stderr))

let test_corrupt_new_file_clean_exit_1 () =
  let r = run_alsdiff ("--git" :: git_args "song.als" base_file corrupt_file) in
  check_exit 1 r;
  check bool "error message on stderr" true (contains "Error" r.stderr);
  check bool "no OCaml backtrace" true (not (contains "Raised at" r.stderr));
  check bool "no output on stdout" true (r.stdout = "")

let test_corrupt_old_file_clean_exit_1 () =
  let r = run_alsdiff ("--git" :: git_args "song.als" corrupt_file base_file) in
  check_exit 1 r;
  check bool "no OCaml backtrace" true (not (contains "Raised at" r.stderr))

let test_rename_header_shows_new_path () =
  let r =
    run_alsdiff
      ("--git" :: rename_args "song.als" base_file modified_file "renamed.als" "similarity index 50%")
  in
  check_exit 1 r;
  check bool "rename header" true (contains "diff --git a/song.als b/renamed.als" r.stdout)

let test_json_mode_has_no_header () =
  let r = run_alsdiff ("--mode" :: "json" :: "--git" :: git_args "song.als" base_file modified_file) in
  check_exit 1 r;
  check bool "json starts with {" true (String.length r.stdout > 0 && r.stdout.[0] = '{');
  check bool "no diff header in json" true (not (contains "diff --git" r.stdout))

let () =
  (* Do not let Alcotest parse the harness args (alsdiff bin + fixtures). *)
  Alcotest.run_with_args
    ~argv:[| "test_git_mode" |]
    "Git mode" (Cmdliner.Term.const ()) [
    "baseline", [
      test_case "unchanged file exits 0 with no output" `Quick test_unchanged_exit_0;
    ];
    "rename and arg count", [
      test_case "pure rename (9 args) exits 0 without crashing" `Quick test_rename_no_change_9_args;
      test_case "rename with content change (9 args) exits 1" `Quick test_rename_with_change_9_args;
      test_case "6 args exits 2 with usage error" `Quick test_wrong_arg_count_exit_2;
      test_case "8 args exits 2" `Quick test_eight_args_exit_2;
    ];
    "add and delete", [
      test_case "added file (old=/dev/null) shows tracks added" `Quick test_added_file;
      test_case "deleted file (new=/dev/null) shows tracks removed" `Quick test_deleted_file;
    ];
    "unparseable files", [
      test_case "corrupt new file exits 1 with clean error" `Quick test_corrupt_new_file_clean_exit_1;
      test_case "corrupt old file exits 1 with clean error" `Quick test_corrupt_old_file_clean_exit_1;
    ];
    "headers", [
      test_case "modified file prints diff --git header" `Quick test_modified_exit_1;
      test_case "rename header shows new path" `Quick test_rename_header_shows_new_path;
      test_case "json mode has no header" `Quick test_json_mode_has_no_header;
    ];
  ]
