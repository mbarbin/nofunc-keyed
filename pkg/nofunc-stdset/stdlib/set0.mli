(*_**********************************************************************************)
(*_  nofunc-keyed: Keyed data structures adapted from OCaml Stdlib but no functors  *)
(*_  SPDX-FileCopyrightText: 2025 Mathieu Barbin <mathieu.barbin@gmail.com>         *)
(*_  SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception   *)
(*_**********************************************************************************)

(*_ Notice: This file was copied from OCaml Stdlib:

  path: "stdlib/set.mli" ; rev: f8ea2c42144f416f4d7a5d71a0bb2c766ca8fedc

  The original license header was kept with the file, see below.

  List of changes:

  - Format file with ocamlformat

  - Remove the functor and signature. Make the type parametrized by the type of
  elements. Require [compare] everywhere needed. *)

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

(** In the context of the [nofunc] project, this file serves as an internal
    building block and is not meant to be facing the user. Every function that
    needs access to the [compare] function takes it as additional argument. *)

type 'elt compare = 'elt -> 'elt -> int
(** A total ordering function over the set elements. This is a two-argument
    function [f] such that [f e1 e2] is zero if the elements [e1] and [e2] are
    equal, [f e1 e2] is strictly negative if [e1] is smaller than [e2], and
    [f e1 e2] is strictly positive if [e1] is greater than [e2]. Example: a
    suitable ordering function is the generic structural comparison function
    {!Stdlib.compare}. *)

type 'elt t
(** The type of sets of elements of type ['elt]. *)

val empty : 'elt t
(** The empty set. *)

val add : compare:'elt compare -> 'elt -> 'elt t -> 'elt t
(** [add x s] returns a set containing all elements of [s], plus [x]. If [x] was
    already in [s], [s] is returned unchanged (the result of the function is
    then physically equal to [s]).
    @before 4.03 Physical equality was not ensured. *)

val singleton : 'elt -> 'elt t
(** [singleton x] returns the one-element set containing only [x]. *)

val remove : compare:'elt compare -> 'elt -> 'elt t -> 'elt t
(** [remove x s] returns a set containing all elements of [s], except [x]. If
    [x] was not in [s], [s] is returned unchanged (the result of the function is
    then physically equal to [s]).
    @before 4.03 Physical equality was not ensured. *)

val union : compare:'elt compare -> 'elt t -> 'elt t -> 'elt t
(** Set union. *)

val inter : compare:'elt compare -> 'elt t -> 'elt t -> 'elt t
(** Set intersection. *)

val disjoint : compare:'elt compare -> 'elt t -> 'elt t -> bool
(** Test if two sets are disjoint.
    @since 4.08 *)

val diff : compare:'elt compare -> 'elt t -> 'elt t -> 'elt t
(** Set difference: [diff s1 s2] contains the elements of [s1] that are not in
    [s2]. *)

val cardinal : 'elt t -> int
(** Return the number of elements of a set. *)

(** {1:elements Elements} *)

val elements : 'elt t -> 'elt list
(** Return the list of all elements of the given set. The returned list is
    sorted in increasing order with respect to the ordering [Ord.compare], where
    [Ord] is the argument given to {!Set.Make}. *)

val min_elt : 'elt t -> 'elt
(** Return the smallest element of the given set (with respect to the
    [Ord.compare] ordering), or raise [Not_found] if the set is empty. *)

val min_elt_opt : 'elt t -> 'elt option
(** Return the smallest element of the given set (with respect to the
    [Ord.compare] ordering), or [None] if the set is empty.
    @since 4.05 *)

val max_elt : 'elt t -> 'elt
(** Same as {!min_elt}, but returns the largest element of the given set. *)

val max_elt_opt : 'elt t -> 'elt option
(** Same as {!min_elt_opt}, but returns the largest element of the given set.
    @since 4.05 *)

val choose : 'elt t -> 'elt
(** Return one element of the given set, or raise [Not_found] if the set is
    empty. Which element is chosen is unspecified, but equal elements will be
    chosen for equal sets. *)

val choose_opt : 'elt t -> 'elt option
(** Return one element of the given set, or [None] if the set is empty. Which
    element is chosen is unspecified, but equal elements will be chosen for
    equal sets.
    @since 4.05 *)

(** {1:searching Searching} *)

val find : compare:'elt compare -> 'elt -> 'elt t -> 'elt
(** [find x s] returns the element of [s] equal to [x] (according to
    [Ord.compare]), or raise [Not_found] if no such element exists.
    @since 4.01 *)

val find_opt : compare:'elt compare -> 'elt -> 'elt t -> 'elt option
(** [find_opt x s] returns the element of [s] equal to [x] (according to
    [Ord.compare]), or [None] if no such element exists.
    @since 4.05 *)

val find_first : ('elt -> bool) -> 'elt t -> 'elt
(** [find_first f s], where [f] is a monotonically increasing function, returns
    the lowest element [e] of [s] such that [f e], or raises [Not_found] if no
    such element exists.

    For example, [find_first (fun e -> Ord.compare e x >= 0) s] will return the
    first element [e] of [s] where [Ord.compare e x >= 0] (intuitively:
    [e >= x]), or raise [Not_found] if [x] is greater than any element of [s].

    @since 4.05 *)

val find_first_opt : ('elt -> bool) -> 'elt t -> 'elt option
(** [find_first_opt f s], where [f] is a monotonically increasing function,
    returns an option containing the lowest element [e] of [s] such that [f e],
    or [None] if no such element exists.
    @since 4.05 *)

val find_last : ('elt -> bool) -> 'elt t -> 'elt
(** [find_last f s], where [f] is a monotonically decreasing function, returns
    the highest element [e] of [s] such that [f e], or raises [Not_found] if no
    such element exists.
    @since 4.05 *)

val find_last_opt : ('elt -> bool) -> 'elt t -> 'elt option
(** [find_last_opt f s], where [f] is a monotonically decreasing function,
    returns an option containing the highest element [e] of [s] such that [f e],
    or [None] if no such element exists.
    @since 4.05 *)

(** {1:traversing Traversing} *)

val iter : ('elt -> unit) -> 'elt t -> unit
(** [iter f s] applies [f] in turn to all elements of [s]. The elements of [s]
    are presented to [f] in increasing order with respect to the ordering over
    the type of the elements. *)

val fold : ('elt -> 'acc -> 'acc) -> 'elt t -> 'acc -> 'acc
(** [fold f s init] computes [(f xN ... (f x2 (f x1 init))...)], where
    [x1 ... xN] are the elements of [s], in increasing order. *)

(** {1:transforming Transforming} *)

val map : compare:'elt compare -> ('elt -> 'elt) -> 'elt t -> 'elt t
(** [map f s] is the set whose elements are [f a0],[f a1]... [f aN], where
    [a0],[a1]...[aN] are the elements of [s].

    The elements are passed to [f] in increasing order with respect to the
    ordering over the type of the elements.

    If no element of [s] is changed by [f], [s] is returned unchanged. (If each
    output of [f] is physically equal to its input, the returned set is
    physically equal to [s].)
    @since 4.04 *)

val filter : ('elt -> bool) -> 'elt t -> 'elt t
(** [filter f s] returns the set of all elements in [s] that satisfy predicate
    [f]. If [f] satisfies every element in [s], [s] is returned unchanged (the
    result of the function is then physically equal to [s]).
    @before 4.03 Physical equality was not ensured.*)

val filter_map :
  compare:'elt compare -> ('elt -> 'elt option) -> 'elt t -> 'elt t
(** [filter_map f s] returns the set of all [v] such that [f x = Some v] for
    some element [x] of [s].

    For example,
    {[
      filter_map (fun n -> if n mod 2 = 0 then Some (n / 2) else None) s
    ]}
    is the set of halves of the even elements of [s].

    If no element of [s] is changed or dropped by [f] (if [f x = Some x] for
    each element [x]), then [s] is returned unchanged: the result of the
    function is then physically equal to [s].

    @since 4.11 *)

val partition : ('elt -> bool) -> 'elt t -> 'elt t * 'elt t
(** [partition f s] returns a pair of sets [(s1, s2)], where [s1] is the set of
    all the elements of [s] that satisfy the predicate [f], and [s2] is the set
    of all the elements of [s] that do not satisfy [f]. *)

val split : compare:'elt compare -> 'elt -> 'elt t -> 'elt t * bool * 'elt t
(** [split x s] returns a triple [(l, present, r)], where [l] is the set of
    elements of [s] that are strictly less than [x]; [r] is the set of elements
    of [s] that are strictly greater than [x]; [present] is [false] if [s]
    contains no element equal to [x], or [true] if [s] contains an element equal
    to [x]. *)

(** {1:predicates Predicates and comparisons} *)

val is_empty : 'elt t -> bool
(** Test whether a set is empty or not. *)

val is_singleton : 'elt t -> bool
(** Test whether a set has exactly one element or not.

    @since 5.5 *)

val mem : compare:'elt compare -> 'elt -> 'elt t -> bool
(** [mem x s] tests whether [x] belongs to the set [s]. *)

val equal : compare:'elt compare -> 'elt t -> 'elt t -> bool
(** [equal s1 s2] tests whether the sets [s1] and [s2] are equal, that is,
    contain equal elements. *)

val compare : compare:'elt compare -> 'elt t -> 'elt t -> int
(** Total ordering between sets. Can be used as the ordering function for doing
    sets of sets. *)

val subset : compare:'elt compare -> 'elt t -> 'elt t -> bool
(** [subset s1 s2] tests whether the set [s1] is a subset of the set [s2]. *)

val for_all : ('elt -> bool) -> 'elt t -> bool
(** [for_all f s] checks if all elements of the set satisfy the predicate [f].
*)

val exists : ('elt -> bool) -> 'elt t -> bool
(** [exists f s] checks if at least one element of the set satisfies the
    predicate [f]. *)

(** {1:converting Converting} *)

val to_list : 'elt t -> 'elt list
(** [to_list s] is {!elements}[ s].
    @since 5.1 *)

val of_list : compare:'elt compare -> 'elt list -> 'elt t
(** [of_list l] creates a set from a list of elements. This is usually more
    efficient than folding [add] over the list, except perhaps for lists with
    many duplicated elements.
    @since 4.02 *)

val to_seq_from : compare:'elt compare -> 'elt -> 'elt t -> 'elt Seq.t
(** [to_seq_from x s] iterates on a subset of the elements of [s] in ascending
    order, from [x] or above.
    @since 4.07 *)

val to_seq : 'elt t -> 'elt Seq.t
(** Iterate on the whole set, in ascending order
    @since 4.07 *)

val to_rev_seq : 'elt t -> 'elt Seq.t
(** Iterate on the whole set, in descending order
    @since 4.12 *)

val add_seq : compare:'elt compare -> 'elt Seq.t -> 'elt t -> 'elt t
(** Add the given elements to the set, in order.
    @since 4.07 *)

val of_seq : compare:'elt compare -> 'elt Seq.t -> 'elt t
(** Build a set from the given bindings
    @since 4.07 *)
