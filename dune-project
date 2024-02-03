(lang dune 3.11)

(name erk)

(generate_opam_files true)

(source
 (github jaredborders/erk))

(authors "Jared Borders")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name erk)
 (synopsis "Merkle & Verkle Trees in OCaml")
 (description "A command-line application for managing Merkle Patricia and Verkle Trees, utilizing libraries from Jane Street and MirageOS")
 (depends
  (ocaml (>= 4.08))
  core
  async
  ppx_jane
  digestif))