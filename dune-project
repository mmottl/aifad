(lang dune 2.7)
(name aifad)
(using menhir 2.1)

(generate_opam_files true)

(source (github mmottl/aifad))
(license "LGPL-2.1-or-later with OCaml-LGPL-linking-exception")
(homepage "https://mmottl.github.io/aifad")
(documentation "https://mmottl.github.io/aifad/api")

(maintainers "Markus Mottl <markus.mottl@gmail.com>")
(authors "Markus Mottl <markus.mottl@gmail.com>")

(package
  (name aifad)
  (synopsis "AIFAD - Automated Induction of Functions over Algebraic Datatypes")
  (description "\
AIFAD is a machine learning tool that generalizes decision tree learning to
algebraic datatypes.")
  (depends
    (ocaml (>= 4.04))
    (menhir (>= "20180523"))
    res
    pcre
    (cfg (>= 2.1))
  )
)
