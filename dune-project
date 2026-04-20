(lang dune 3.0)

(name orgcaml)

(authors "Brad White")

(license MIT)

(generate_opam_files true)

(package
 (name orgcaml)
 (synopsis "Org-mode parser and HTML renderer for OCaml")
 (description
  "A library for parsing Org-mode documents into an AST and rendering them to HTML. Use as an SDK to build Org-mode tooling in OCaml.")
 (depends
  (ocaml (>= 4.14))
  (angstrom (>= 0.15))
  (dune (>= 3.0))))

(package
 (name orgcaml-bin)
 (synopsis "Org-mode to HTML CLI converter")
 (depends
  (ocaml (>= 4.14))
  (orgcaml (= :version))
  (dune (>= 3.0))))
