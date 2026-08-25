(*_**********************************************************************************)
(*_  nofunc-keyed: Keyed data structures adapted from OCaml Stdlib but no functors  *)
(*_  SPDX-FileCopyrightText: 2025-2026 Mathieu Barbin <mathieu.barbin@gmail.com>    *)
(*_  SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception   *)
(*_**********************************************************************************)

(*_ Notice: This file was created from the OCaml Stdlib Hashtbl mli file:

  path: "stdlib/hashtbl.mli" ; rev: f8ea2c42144f416f4d7a5d71a0bb2c766ca8fedc

  The original license header was kept with the file, see below.

  List of changes:

  - While adapting to the use of sets, we replaced the mention of "bindings" by
    "elements". Also, the interface is defunctorized, so it is in essence closer
    to the [Hashtbl0] interface from this project than OCaml when both of these
    diverge.

  - Take the set as the first argument. Label closures [~f].

  - Add a "Modular explicit usage" section, with a Base-style [M] functor and
    [sexp_of_m__t] / [dyn_of_m__t] derivers requiring a [compare] function, used
    to sort elements before building the result, since iteration order is
    unspecified.
*)

(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Hash sets implemented from hash tables. *)

(** The type of hash sets of elements of type ['a]. *)
type !'a t

(** Empty a hash set. Use [reset] instead of [clear] to shrink the size of the
    bucket table to its initial size. *)
val clear : 'a t -> unit

(** Empty a hash set and shrink the size of the bucket table to its initial
    size. *)
val reset : 'a t -> unit

(** Return a copy of the given hash set. *)
val copy : 'a t -> 'a t

(** [add set elt] adds an element to the set. This uses [Hashtbl.replace]
    internally so this is idempotent. *)
val add : 'a t -> 'a -> unit

(** [mem set x] checks if [x] is present in [set]. *)
val mem : 'a t -> 'a -> bool

(** [remove set x] removes the current binding of [x] in [set]. It does nothing
    if [x] is not present in [set]. *)
val remove : 'a t -> 'a -> unit

(** [iter set ~f] applies [f] to all elements in set [set]. Each element is
    presented exactly once to [f].

    The order in which the elements are passed to [f] is unspecified.

    If the hash set was created in non-randomized mode, the order in which the
    elements are enumerated is reproducible between successive runs of the
    program, and even between minor versions of OCaml. For randomized hash
    tables, the order of enumeration is entirely random.

    The behavior is not specified if the hash set is modified by [f] during
    the iteration. *)
val iter : 'a t -> f:('a -> unit) -> unit

(** [filter_inplace set ~f] applies [f] to all elements in set [set] and decide
    whether to keep the elements depending on the result of [f]. If [f] returns
    [false], the element is discarded. If it returns [true], the elements is
    kept.

    Other comments for {!iter} apply as well. *)
val filter_inplace : 'a t -> f:('a -> bool) -> unit

(** [fold set init ~f] computes [(f eN ... (f e1 init)...)], where [e1 ... eN]
    are the elements in [set]. Each element is presented exactly once to [f].
    [key] is labeled to avoid mixing it up with the accumulator.

    The order in which the elements are passed to [f] is unspecified.

    If the hash set was created in non-randomized mode, the order in which the
    elements are enumerated is reproducible between successive runs of the
    program, and even between minor versions of OCaml. For randomized hash sets,
    the order of enumeration is entirely random.

    The behavior is not specified if the hash set is modified by [f] during
    the iteration. *)
val fold : 'a t -> 'acc -> f:(key:'a -> 'acc -> 'acc) -> 'acc

(** [length set] returns the number of bindings in [set]. It takes constant
    time. *)
val length : _ t -> int

(** [stats tbl] returns statistics about the table [tbl]: number of buckets,
    size of the biggest bucket, distribution of buckets by size. *)
val stats : _ t -> Stdlib.Hashtbl.statistics

(** Iterate on the whole set. The order in which the elements appear in the
    sequence is unspecified.

    The behavior is not specified if the hash set is modified during the
    iteration. *)
val to_seq : 'a t -> 'a Seq.t

(** Add the elements to the table, using {!add}. *)
val add_seq : 'a t -> 'a Seq.t -> unit

val create_seeded
  :  (module Hashtbl.SeededHashedType with type t = 'a)
  -> ?random:bool
  -> int
  -> 'a t

(** Build a set from the given elements. The elements are added in the same
    order they appear in the sequence, using {!add}, which means that duplicated
    elements are simply ignored (add is idempotent). *)
val of_seq_seeded
  :  (module Hashtbl.SeededHashedType with type t = 'a)
  -> ?random:bool
  -> 'a Seq.t
  -> 'a t

(** When using the [HashedType] interface, hashtbl cannot be created with
    [~random] set to [true], thus this parameter does not appear in the
    interface. *)
val create : (module Hashtbl.HashedType with type t = 'a) -> int -> 'a t

(** Same as {!of_seq_seeded} but never randomized. *)
val of_seq : (module Hashtbl.HashedType with type t = 'a) -> 'a Seq.t -> 'a t

(** {1:modular Modular explicit usage}

    The declarations below offer an alternative, module-based style for fixing
    the element type at a single first-class module, in the tradition of
    Base's [Hash_set.M(Elt).t]. They also provide ways to derive [sexp_of_t]
    and [to_dyn] for a hash set, given a first-class module for the elements.

    Since the iteration order of a hash set is unspecified, the first-class
    module supplied to {!val:sexp_of_m__t} and {!val:dyn_of_m__t} is required
    to provide a [compare] function, used to sort the elements before building
    the resulting value, so that the output is deterministic. *)

module M (T : sig
    type t
  end) : sig
  type nonrec t = T.t t
end

(** Input signature for {!val:sexp_of_m__t}. *)
module type Sexpable = sig
  type t

  val compare : t -> t -> Ordering.t
  val sexp_of_t : t -> Sexplib0.Sexp.t
end

(** [sexp_of_m__t (module Elt) set] converts [set] to an s-expression, listing
    the elements sorted by [Elt.compare]. *)
val sexp_of_m__t : (module Sexpable with type t = 'a) -> 'a t -> Sexplib0.Sexp.t

(** Input signature for {!val:dyn_of_m__t}. *)
module type Dynable = sig
  type t

  val compare : t -> t -> Ordering.t
  val to_dyn : t -> Dyn.t
end

(** [dyn_of_m__t (module Elt) set] converts [set] to a [Dyn.t], built the same
    way as {!val:sexp_of_m__t}. *)
val dyn_of_m__t : (module Dynable with type t = 'a) -> 'a t -> Dyn.t
