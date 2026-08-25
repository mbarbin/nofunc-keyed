# Initiating this Repo

This document describes how this repo was initiated.

## Introduction

The initial contents of this repository was created by following particular
steps that are mostly systematic and following guiding principles that made the
initiating process almost entirely deterministic and reproducible.

In this document we describe precisely and technically the actual steps that
were used.

## Step 1

The first step was to initiate the part of the repository that relates to the
`map` and `set` data structures.

1. Created a dune-project file with packages definitions to support the new
   packages.

2. Imported files from an OCaml distribution, from `stdlib/` at rev: `f8ea2c42144f416f4d7a5d71a0bb2c766ca8fedc`

3. Auto-fmt the code using the `conventional` profile of `ocamlformat`.

4. Silence warnings 9 and build the repo.

5. Add project license headers - initiate notices for changes to imported files.

6. Removed the functor from the set ml and mli and let ocamlformat reindent the
   contents of the files at toplevel.

   Note that starting from this stage, the tree won't build for a few commits,
   which is done intentionally in order to make the history of edits more clear.

7. This step is mostly deterministic however may appear difficult to review
   based on the raw diff. The principle is to replace any occurrence of
   `Ord.compare` by `compare` function introduced as an additional argument to
   the current function, and then propagate the argument where needed.

   To avoid using `Stdlib.compare` by accident, we shadow this function at the
   beginning of the file.

   From that commit, the tree builds again.

8. Repeat step 6 for the map module. Removed the functor and reindent. The tree
   won't build at this commit.

9. Repeat step 7 for the map module. Propagate `compare` arguments and fix
   the build.

10. The interface of `set0.mli` is initiated from a copy from `Stdlib`, and then
    we remove the functor. We make the type parametrized by the type of
    elements, and require `Ord` as modexp argument everywhere needed.

11. This step adds new code to implement `set0.ml`. We try to make the code as
    deterministic as possible. The functions operating on one tree should be
    built with a simple alias helper, while functions operations on multiple
    trees must raise when operating on incompatible inputs.

12. Repeat steps 10 and 11 for the map0 module.

## Step 2

The second step was to initiate the part of the repository that relates to the
`hashtbl` data structure.

1. Create skeleton for the `stdhtbl` package.

2. Imported `Hashtbl` module from an OCaml distribution, from `stdlib/` at rev: `f8ea2c42144f416f4d7a5d71a0bb2c766ca8fedc`

3. Auto-fmt the code using the `conventional` profile of `ocamlformat`.

4. Silence warnings 9 and build the repo. Fix odoc warnings.

5. Add project license headers - initiate notices for changes to imported files.

6. Start from the implementation of the `MakeSeeded` functor.
   a. Remove its parameters, make it simply a module instead of a functor.
   b. Add `equal` and `seeded_hash` as parameters everywhere needed instead of accessing these functions from `H`.
   c. Inline [MakeSeeded] at toplevel, reformat the code.
   d. Remove module type interfaces
   e. Remove the generic version
   f. Update the mli to match the implementation of the defunc functions.

7. The interface of `hashtbl0.mli` is initiated from `Stdlib.Hashtbl.MakeSeeded`
   but without the functor. We require a modexp argument everywhere needed.

8. This steps adds new code to implement `hashtbl0.ml`. There are no functions
   that operate on multiple tables so there is no runtime exception similar to
   the stdmap.

## Step 3

1. Create a new module `Hashset` for hash sets based on `Hashtbl` using `unit`
   as data. Adapt the interface and implementation to always have at most one
   binding per element in the set (`add` performs a `replace`).

## Step 4

The fourth step started the `non-std` flavor of the packages (`nofunc-map`,
`nofunc-set`, `nofunc-htbl`, `nofunc-hset`), mentioned in the [Style &
API](../README.md#style--api) section of the README, and began differentiating
it from the `std` flavor.

1. Create an empty skeleton for each of the four `non-std` packages: a `dune`
   file and an empty library file (license header only, no code), mirroring the
   scaffolding of their `std` counterparts.

2. Move the private `<pkg>.stdlib` sub-library out of each `std` package
   (`nofunc-stdmap/stdlib`, `nofunc-stdset/stdlib`, `nofunc-stdhtbl/stdlib`) and
   into the matching `non-std` package, at the same `stdlib/` sub-path and
   under the same `.stdlib` sub-package naming convention (e.g.
   `nofunc-map.stdlib`). This is the internal, functorless-but-explicit-compare
   building block copied from the OCaml Stdlib (see Step 1 and Step 2); moving
   it does not change its code. Each `std` package now depends on its `non-std`
   sibling's `.stdlib` sub-library instead of on a private sub-library of its
   own (e.g. `nofunc-stdmap` depends on `nofunc-map.stdlib`), and declares an
   opam dependency on that sibling package.

   `nofunc-stdhset` is not affected: it has no `stdlib` sub-library of its own,
   it builds directly on top of `nofunc-stdhtbl`.

3. Add `ordering` as a dependency of `nofunc-map.stdlib` and
   `nofunc-set.stdlib`, and require the `compare` function taken by every
   function in these `stdlib` building blocks to return `Ordering.t` instead of
   `int`. Pattern-match each call site on `Ordering.Eq` / `Ordering.Lt` /
   `Ordering.Gt`, annotating the `compare` argument as `~(compare : _ compare)`
   so the constructors resolve without qualification.

4. In `nofunc-stdmap` and `nofunc-stdset`, store two closures in the record
   built from the `Ord.compare` handed in at creation site (`empty`,
   `singleton`, `of_list`, `of_seq`): `compare_int`, the original int-returning
   closure, kept only for the physical-equality consistency check between two
   structures (`check_same_compare`); and `compare`, an `Ordering`-returning
   closure derived from it via `Ordering.of_int`, used for the actual tree
   operations. Both packages depend on `ordering` directly for this.

5. Copy the top-level `ml`/`mli` files of each `std` package verbatim into its
   `non-std` counterpart, giving each `non-std` package a clean starting point
   to iterate and diverge from: `map0`/`nofunc_stdmap` into `nofunc-map`,
   `set0`/`nofunc_stdset` into `nofunc-set`, `hashtbl0`/`nofunc_stdhtbl` into
   `nofunc-htbl`, and `hashset0`/`nofunc_stdhset` into `nofunc-hset`.

   This is a pure copy: file contents are byte-for-byte identical to their
   `std` source, module references included. As a result the tree doesn't
   build at this commit, the same intentional temporary breakage already used
   in Step 1: `map0.ml`/`set0.ml`/`hashtbl0.ml` reference their own package's
   `.stdlib` sub-library without depending on it in `dune` yet, and
   `hashset0.ml` still references `Nofunc_stdhtbl`, the `std` package, instead
   of the `non-std` one.

6. Fix up the four `non-std` packages so they build against their own sources:
   add the missing `libraries` dependencies (`.stdlib` sub-library, and
   `ordering` where `map0.ml`/`set0.ml` need it), and rewire `hashset0.ml` to
   `Nofunc_htbl.Hashtbl` instead of `Nofunc_stdhtbl.Hashtbl`, with `nofunc-hset`
   now depending on `nofunc-htbl`. The tree builds again from this commit on.

7. Small tweak: in `nofunc-map` and `nofunc-set`'s `stdlib` building blocks,
   make the two-map/set comparison function (`compare_aux`) chain entirely in
   `Ordering.t` instead of converting the key comparison to `int` at every
   recursive step via `Ordering.to_int`. It matches on the key compare's
   result with the normal `(Lt | Gt) as res -> res | Eq -> ...` chaining
   pattern, converting to `int` only once, at the very end, to satisfy the
   public `int`-returning `compare` signature.

8. Diverge `nofunc-map` and `nofunc-set` from their `std` counterparts:
   `OrderedType.compare` now must return `Ordering.t` directly instead of
   `int` - these packages are for projects that work with `Ordering.t`
   natively, not as a wrapper around `int`-returning comparators. Since the
   record's `compare` field already matches the tree's `compare` type,
   `Ord.compare` is stored and passed to the tree operations as-is, with no
   wrapping closure needed - so `compare_int` is dropped entirely, along with
   the extra allocation it required at every creation site
   (`empty`/`singleton`/`of_list`/`of_seq`). `check_same_compare` now checks
   physical equality directly on the single `compare` closure.

9. Add `test/map`, `test/set`, `test/htbl`, `test/hset`: basic test suites for
   the four `non-std` packages, mirroring the `std` suites' structure but
   scoped to the wrapper's own code. Each wrapper shares its actual
   tree/table implementation with its `std` counterpart, already exercised
   thoroughly by the existing `stdmap`/`stdset`/`stdhtbl`/`stdhset` suites, so
   these new suites only call every function of the thin wrapper itself, with
   just enough cases to hit every wrapper-level branch (physical-equality
   fast paths, the different-compare-functions error path for map/set). All
   four wrapper files (`map0.ml`, `set0.ml`, `hashtbl0.ml`, `hashset0.ml`)
   reach 100% line coverage, matching the `std` ones.

10. Switch the `non-std` interfaces to a `t`-first style with labeled
    closures: every function that operates on a container now takes it as
    its first argument, and every closure passed to be called is labeled
    `~f` (e.g. `val iter : 'a t -> f:('a -> unit) -> unit`). Functions
    operating on bindings (a key and an associated value) label the two
    `~key` and `~data` (e.g. `val add : ('a, 'b) t -> key:'a -> data:'b ->
    ('a, 'b) t`): `nofunc-map`'s
    `add`/`add_to_list`/`update`/`singleton`/`remove`/`find`/`find_opt`/`mem`/`split`/`to_seq_from`,
    and `nofunc-htbl`'s
    `add`/`find`/`find_opt`/`find_all`/`mem`/`remove`/`find_and_remove`/`replace`/`find_and_replace`.
    `nofunc-set` and `nofunc-hset`'s single element argument stays
    positional, matching Base's `Set.add : t -> 'a -> t` convention.

    `Map.iter`/`Map.fold`'s own callback additionally labels its `~key`/
    `~data` arguments, with the accumulator left last and unlabeled, to
    avoid confusion with the accumulator's position - which varies across
    fold conventions (mirroring Base's `Map.fold`:
    `f:(key:'k -> data:'v -> 'acc -> 'acc)`, as opposed to `List.fold`'s
    `f:('accum -> 'a -> 'accum)`). `Set.fold`'s callback labels its element
    `~key` for the same reason.
