let decompress_als filename =
  let basename = Filename.basename filename |> Filename.remove_extension in
  let temp_file = Filename.temp_file basename ".xml" in
  let gz_in = Gzip.open_in filename in
  let out_chan = open_out temp_file in
  let buffer = Bytes.create 4096 in
  let rec copy_loop () =
    let bytes_read = Gzip.input gz_in buffer 0 (Bytes.length buffer) in
    if bytes_read > 0 then (
      output out_chan buffer 0 bytes_read;
      copy_loop ()
    )
  in
  (try copy_loop () with End_of_file -> ());
  Gzip.close_in gz_in;
  close_out out_chan;
  temp_file


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
