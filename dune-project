(lang dune 3.11)

(name camel_finder)

(generate_opam_files true)

(source
 (github edwinans/camel-finder))

(authors "Edwin Ansari")

(maintainers "Edwin Ansari")

(license MIT)

(package
 (name camel_finder)
 (allow_empty)
 (synopsis "Minesweeper-like game running on Web")
 (description "Implementation of Minesweeper (with camels) in ocaml-vdom")
 (depends ocaml dune vdom js_of_ocaml)
 (tags
  ("game" "web" "vdom")))
