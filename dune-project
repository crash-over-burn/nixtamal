(lang dune 3.20)

(name nixtamal)

(generate_opam_files true)

(source
 (uri "https://darcs.toastal.in.th/nixtamal/trunk"))

(authors "toastal <toastal@posteo.net>")

(maintainers "toastal <toastal@posteo.net>")

(license GPL-3.0-or-later)

(package
 (name nixtamal)
 (synopsis "Fulfilling Nix version pinning")
 (description "TODO")
 (depends
  camomile
  cmdliner
  eio
  eio_main
  fmt
  jingoo
  jsont
  kdl
  logs
  ppx_deriving
  ppx_deriving_qcheck
  uri
  (alcotest :with-test)
  (qcheck :with-test)
  (qcheck-alcotest :with-test))
 (tags ("nix")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
