let decompress_als filename =
  let basename = Filename.basename filename |> Filename.remove_extension in
  let temp_file = Filename.temp_file basename "" in
  let cmd = Printf.sprintf "gzip -d -c -S .als '%s' > '%s'" filename temp_file in
  match Sys.command cmd with
  | 0 -> temp_file
  | _ -> failwith @@ Printf.sprintf "Failed to decompress the gzip file: %s" filename

(** open the .als file with [filename], and return the parsed XML tree. *)
let open_als filename =
  filename
  |> decompress_als
  |> Xml.read_file


let time_it (f : unit -> 'a)  =
  let start_time = Sys.time() in
  let result = f () in
  let end_time = Sys.time() in
  Printf.printf "Execute time: %f seconds\n" (end_time -. start_time);
  result
