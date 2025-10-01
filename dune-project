(lang dune 3.19)

(name live_git)

(generate_opam_files false)

(source
 (github krfantasy/live_git))

(authors "Jay Xu <jay.xu.krfantasy@gmail.com>")

(maintainers "Jay Xu <jay.xu.krfantasy@gmail.com>")

(license LICENSE.txt)

(documentation https://github.com/krfantasy/live_git)

(package
 (name live_git)
 (synopsis "Git helper for Ableton Live Set(.als)")
 (description "Git helper for Ableton Live Set(.als)")
 (depends ocaml camlzip xmlm angstrom eio eio_main alcotest ppx_deriving)
 (tags
  ("ableton live" "git" "version control")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
