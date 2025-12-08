# Initiating this Repo

This document describes how this repo was initiated.

## Introduction

The initial contents of this repository was created by following particular
steps that are mostly systematic and following guiding principles that made the
initiating process almost entirely deterministic and reproducible.

In this document we describe precisely and technically the actual steps that
were used.

## Steps

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
