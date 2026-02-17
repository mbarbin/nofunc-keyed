# nofunc-keyed

[![CI Status](https://github.com/mbarbin/nofunc-keyed/workflows/ci/badge.svg)](https://github.com/mbarbin/nofunc-keyed/actions/workflows/ci.yml)
[![Coverage Status](https://coveralls.io/repos/github/mbarbin/nofunc-keyed/badge.svg?branch=main)](https://coveralls.io/github/mbarbin/nofunc-keyed?branch=main)

## Introduction

This repo implements OCaml packages for common data structures with elements used as keys:

- Map
- Set
- Hash-table
- Hash-set

The code is derived from the OCaml Stdlib, and adapted to remove the use of functors. Some may know this as a style used in the `Base` library which also relates to designs inspired by `modular-explicit`.

## Description

These data structures require functions operating on a key type, typically `hash`, `equal` or `compare`. Instead of accessing then from a functor argument, the functions that create new structures take them from a first-class module passed as a mandatory argument in first position.

**Functorized (stdlib):**
```ocaml
module Int_set = Stdlib.Set.Make (Int)
let (s : Int_set.t) = Int_set.of_list [ 1; 2; 3 ]
```

**Nofunc (this project)**:
```ocaml
module Set = Nofunc_set.Set
let (s : int Set.t) = Set.of_list (module Int) [ 1; 2; 3 ]
```

Note that in the `nofunc` version, the keys are type parameters.

The required functions are stored *inside the structure* for later use by the functions that need it. So for example here, `Int.compare` will be stored as a field inside that `s` value.

### Dynamic Resolution of Conflicting Key Functions

In `Nofunc`, the code raises an exception when attempting to operate on multiple values that have been created using different key functions.

```ocaml
module Set = Nofunc_set.Set

let%expect_test "different compare" =
  let module String2 = struct
    type t = string

    (* The compare function is reversed! *)
    let compare a b = String.compare b a
  end
  in
  let s1 = Set.of_list (module String) [ "a"; "b" ] in
  let s2 = Set.of_list (module String2) [ "c"; "b" ] in
  print_dyn (Set.to_dyn Dyn.string s1);
  [%expect {| set { "a"; "b" } |}];
  print_dyn (Set.to_dyn Dyn.string s2);
  [%expect {| set { "c"; "b" } |}];
  let () =
    match Set.union s1 s2 with
    | (_ : string Set.t) -> assert false
    | exception e -> print_string (Printexc.to_string e)
  in
  [%expect {| Invalid_argument("Sets have different compare functions.") |}];
  ()
;;
```

This exception indicates a programming error that must be fixed.

These principles generalize to the other data structures available here besides `Set`.

### Style & API

The packages come in two flavors, `std` and `non-std`.

- The packages `nofunc-std*` follow closely the naming and API from the OCaml Stdlib.
- The packages `nofunc-*` (without the `std` prefix) build on the general principles mentioned here, while making some additional opinionated decisions based on the author's personal preferences.

## Motivations

We believe that avoiding functors can be ergonomic in some cases and we think that these libraries could be useful in projects that do not depend on `Base`.

We're experimenting with the non-standard versions in personal projects.

## License

`LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception`

## Status

:construction: We're currently building this project and we'll update this section once we're done with that initial phase.

## Acknowledgements

There is very little if any original content in this repo. We re-packaged code from existing sources to make the libraries easily available for other projects.

We are very thankful to the work we've been able to build upon here. We're listing these sources below.

### OCaml Stdlib & INRIA

https://github.com/ocaml/ocaml

The implementation is based on copies of original modules from the OCaml Stdlib, with minor changes required by this project:

- Hashtbl
- Map
- Set

The license of ocaml is included [here](third-party-license/ocaml/ocaml/LICENSE).

### Base & Jane Street

https://github.com/janestreet/base

Sources of inspirations for this project include the modules and interfaces of similar keyed modules from the `Base` library.

The license of base is included [here](third-party-license/janestreet/base/LICENSE.md).

### Stdune

https://github.com/ocaml/dune

Another source of inspiration for this project was similar keyed modules from `dune`'s standard library `stdune`.

The license of dune is included [here](third-party-license/ocaml/dune/LICENSE.md).
