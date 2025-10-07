(lang dune 3.19)

(name alsdiff)

(generate_opam_files true)

(source
 (github krfantasy/alsdiff))

(authors "Jay Xu <jay.xu.krfantasy@gmail.com>")

(maintainers "Jay Xu <jay.xu.krfantasy@gmail.com>")

(license LICENSE.txt)

(documentation https://github.com/krfantasy/alsdiff)

(package
 (name alsdiff)
 (synopsis "Git helper for Ableton Live Set(.als)")
 (description "Git helper for Ableton Live Set(.als)")
 (depends ocaml camlzip xmlm angstrom eio eio_main alcotest ppx_deriving ppx_deriving_yojson ppx_deriving_jsonschema)
 (tags
  ("ableton live" "git" "version control")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
