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
 (name nofunc-keyed)
 (synopsis
  "Meta empty package needed by some tools to make things work - NOT PUBLISHED")
 (allow_empty))

(package
 (name nofunc-hset)
 (synopsis "Hashset with an opinionated nofunc API (non-std)")
 (depends
  (ocaml
   (>= 5.2))
  (dyn
   (>= 3.20))
  (nofunc-htbl
   (= :version))
  (ordering
   (>= 3.20))
  (sexplib0
   (>= v0.16))))

(package
 (name nofunc-htbl)
 (synopsis "Hashtbl with an opinionated nofunc API (non-std)")
 (depends
  (ocaml
   (>= 5.2))
  (dyn
   (>= 3.20))
  (ordering
   (>= 3.20))
  (sexplib0
   (>= v0.16))))

(package
 (name nofunc-map)
 (synopsis "Map with an opinionated nofunc API (non-std)")
 (depends
  (ocaml
   (>= 4.14))
  (dyn
   (>= 3.20))
  (ordering
   (>= 3.20))
  (sexplib0
   (>= v0.16))))

(package
 (name nofunc-set)
 (synopsis "Set with an opinionated nofunc API (non-std)")
 (depends
  (ocaml
   (>= 4.14))
  (dyn
   (>= 3.20))
  (ordering
   (>= 3.20))
  (sexplib0
   (>= v0.16))))

(package
 (name nofunc-stdhset)
 (synopsis "Hashset implemented from nofunc Hashtbl")
 (depends
  (ocaml
   (>= 5.2))
  (nofunc-stdhtbl
   (= :version))))

(package
 (name nofunc-stdhtbl)
 (synopsis "Hashtbl adapted from Stdlib but no functors")
 (depends
  (ocaml
   (>= 5.2))
  (nofunc-htbl
   (= :version))))

(package
 (name nofunc-stdmap)
 (synopsis "Map adapted from Stdlib but no functors")
 (depends
  (ocaml
   (>= 4.14))
  (nofunc-map
   (= :version))
  (ordering
   (>= 3.20))))

(package
 (name nofunc-stdset)
 (synopsis "Set adapted from Stdlib but no functors")
 (depends
  (ocaml
   (>= 4.14))
  (nofunc-set
   (= :version))
  (ordering
   (>= 3.20))))

(package
 (name nofunc-keyed-tests)
 (synopsis "Tests for nofunc-keyed")
 (depends
  (ocaml
   (>= 5.2))
  (dyn
   (>= 3.20))
  (nofunc-hset
   (= :version))
  (nofunc-htbl
   (= :version))
  (nofunc-map
   (= :version))
  (nofunc-set
   (= :version))
  (nofunc-stdhset
   (= :version))
  (nofunc-stdhtbl
   (= :version))
  (nofunc-stdmap
   (= :version))
  (nofunc-stdset
   (= :version))
  (ordering
   (>= 3.20))
  (pp
   (>= 2.0.0))
  (ppx_expect
   (>= v0.17))
  (ppx_sexp_conv
   (>= v0.17))
  (sexplib0
   (>= v0.16))))

(package
 (name nofunc-keyed-dev)
 (synopsis "Package to regroup dev targets, documentation, and more")
 (allow_empty)
 (depends
  (ocaml
   (>= 5.2))
  (ocamlformat
   (= 0.29.0))
  (bisect_ppx
   (>= 2.8.3))
  (nofunc-hset
   (= :version))
  (nofunc-htbl
   (= :version))
  (nofunc-keyed-tests
   (= :version))
  (nofunc-map
   (= :version))
  (nofunc-set
   (= :version))
  (nofunc-stdhset
   (= :version))
  (nofunc-stdhtbl
   (= :version))
  (nofunc-stdmap
   (= :version))
  (nofunc-stdset
   (= :version))
  (sherlodoc
   (>= 0.2))))
