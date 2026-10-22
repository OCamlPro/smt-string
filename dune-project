(lang dune 3.0)

(generate_opam_files true)

(name smt-string)

(license "LGPL-2.1-only WITH OCaml-LGPL-linking-exception")

(authors "Hichem Rami Ait El Hara <hra@ocamlpro.com>")

(maintainers "Hichem Rami Ait El Hara <hra@ocamlpro.com>")

(source
 (github ocamlpro/smt-string))

(package
 (name smt-string)
 (synopsis "A library of Unicode string and regular expression values following the SMT-LIB standard")
 (description
  "A library of Unicode string and regular expression values following the \
   SMT-LIB standard, where characters are Unicode code points in the range \
   0x0-0x2FFFF (planes 0-2).")
 (depends
  (ocaml
   (>= 5.2))
  dune
  zarith))
