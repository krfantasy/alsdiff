open Alsdiff_tui_lib.Keymap

let test_ascii () =
  Alcotest.(check string) "A is single byte" "A" (utf8_of_uchar (Uchar.of_int 0x41))

let test_latin1_supplement () =
  (* U+00E9 é must be 2-byte UTF-8 C3 A9, not single Latin-1 byte E9 *)
  Alcotest.(check string) "é is C3 A9" "\xC3\xA9" (utf8_of_uchar (Uchar.of_int 0xE9))

let test_cjk () =
  Alcotest.(check string) "CJK 3 bytes" "\xE4\xB8\xAD" (utf8_of_uchar (Uchar.of_int 0x4E2D))

let test_emoji_roundtrip () =
  let s = utf8_of_uchar (Uchar.of_int 0x1F600) in
  Alcotest.(check int) "emoji 4 bytes" 4 (String.length s)

let () =
  Alcotest.run "Keymap" [
    "utf8_of_uchar", [
      Alcotest.test_case "ascii" `Quick test_ascii;
      Alcotest.test_case "latin1-supplement is UTF-8" `Quick test_latin1_supplement;
      Alcotest.test_case "cjk" `Quick test_cjk;
      Alcotest.test_case "emoji" `Quick test_emoji_roundtrip;
    ];
  ]
