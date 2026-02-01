(lang dune 3.20)

(name nofunc-keyed)

(generate_opam_files)

(license "LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception")

(authors "OCaml Stdlib" "Mathieu Barbin")

(maintainers "Mathieu Barbin <opensource@mbarbin.org>")

(source
 (github mbarbin/nofunc-keyed))

(documentation "https://mbarbin.github.io/nofunc-keyed/")

(implicit_transitive_deps false-if-hidden-includes-supported)

(package
 (name nofunc-stdmap)
 (synopsis "Map adapted from Stdlib but no functors")
 (depends
  (ocaml
   (>= 4.14))))

(package
 (name nofunc-stdset)
 (synopsis "Set adapted from Stdlib but no functors")
 (depends
  (ocaml
   (>= 4.14))))

(package
 (name nofunc-keyed-dev)
 (synopsis "Package to regroup dev targets, documentation, and more")
 (allow_empty)
 (depends
  (ocaml
   (>= 5.2))
  (ocamlformat
   (= 0.28.1))
  (sherlodoc
   (>= 0.2))))
