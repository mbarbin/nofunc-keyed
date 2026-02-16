# Initiating this Repo

This document describes how this repo was initiated.

## Introduction

The initial contents of this repository was created by following particular
steps that are mostly systematic and following guiding principles that made the
initiating process almost entirely deterministic and reproducible.

In this document we describe precisely and technically the actual steps that
were used.

## First Step

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

## Second Step

The second step was to initiate the part of the repository that relates to the
`hashtbl` data structure.

1. Create skeleton for the `stdhtbl` package.

2. Imported `Hashtbl` module from an OCaml distribution, from `stdlib/` at rev: `f8ea2c42144f416f4d7a5d71a0bb2c766ca8fedc`

3. Auto-fmt the code using the `conventional` profile of `ocamlformat`.

4. Silence warnings 9 and build the repo. Fix odoc warnings.

5. Add project license headers - initiate notices for changes to imported files.
