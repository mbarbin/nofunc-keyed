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
    elements. Require [(module Ord)] as first-class module argument everywhere
    needed.

  - Document which functions raise when operating on incompatible inputs. *)

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

(** Sets over ordered types.

    This interface is adapted from OCaml's stdlib and inspired by the Base
    container.

    The compare function is taken from a first-class module supplied as argument
    and stored with the set to be used when needed.

    When a function operates on multiple sets, the code will raise
    [Invalid_argument] if the compare functions are not all physical equal,
    ensuring the consistency of the computed values. *)

(** Input signature for element comparison. *)
module type OrderedType = sig
  (** The type of the set elements. *)
  type t

  (** A total ordering function over the set elements. *)
  val compare : t -> t -> int
end

(** The type of sets of elements of type ['elt]. *)
type 'elt t

(** [empty (module Ord)] returns an empty set using [Ord.compare] for ordering. *)
val empty : (module OrderedType with type t = 'elt) -> 'elt t

(** [add x s] returns a set containing all elements of [s], plus [x]. If [x] was
    already in [s], [s] is returned unchanged (the result of the function is
    then physically equal to [s]). *)
val add : 'elt -> 'elt t -> 'elt t

(** [singleton (module Ord) x] returns the one-element set containing only [x],
    using [Ord.compare] for ordering. *)
val singleton : (module OrderedType with type t = 'elt) -> 'elt -> 'elt t

(** [remove x s] returns a set containing all elements of [s], except [x]. If
    [x] was not in [s], [s] is returned unchanged (the result of the function is
    then physically equal to [s]). *)
val remove : 'elt -> 'elt t -> 'elt t

(** Set union.
    @raise Invalid_argument if the sets have different compare functions. *)
val union : 'elt t -> 'elt t -> 'elt t

(** Set intersection.
    @raise Invalid_argument if the sets have different compare functions. *)
val inter : 'elt t -> 'elt t -> 'elt t

(** Test if two sets are disjoint.
    @raise Invalid_argument if the sets have different compare functions. *)
val disjoint : 'elt t -> 'elt t -> bool

(** Set difference: [diff s1 s2] contains the elements of [s1] that are not in
    [s2].
    @raise Invalid_argument if the sets have different compare functions. *)
val diff : 'elt t -> 'elt t -> 'elt t

(** Return the number of elements of a set. *)
val cardinal : _ t -> int

(** {1:elements Elements} *)

(** Return the list of all elements of the given set. The returned list is
    sorted in increasing order with respect to the ordering [Ord.compare] used
    to create the set. *)
val elements : 'elt t -> 'elt list

(** Return the smallest element of the given set (with respect to the
    [Ord.compare] ordering used to create the set), or raise [Not_found] if the
    set is empty. *)
val min_elt : 'elt t -> 'elt

(** Return the smallest element of the given set (with respect to the
    [Ord.compare] ordering used to create the set), or [None] if the set is
    empty. *)
val min_elt_opt : 'elt t -> 'elt option

(** Same as {!val:min_elt}, but returns the largest element of the given set. *)
val max_elt : 'elt t -> 'elt

(** Same as {!val:min_elt_opt}, but returns the largest element of the given
    set. *)
val max_elt_opt : 'elt t -> 'elt option

(** Return one element of the given set, or raise [Not_found] if the set is
    empty. Which element is chosen is unspecified, but equal elements will be
    chosen for equal sets. *)
val choose : 'elt t -> 'elt

(** Return one element of the given set, or [None] if the set is empty. Which
    element is chosen is unspecified, but equal elements will be chosen for
    equal sets. *)
val choose_opt : 'elt t -> 'elt option

(** {1:searching Searching} *)

(** [find x s] returns the element of [s] equal to [x] (according to
    [Ord.compare]), or raise [Not_found] if no such element exists. *)
val find : 'elt -> 'elt t -> 'elt

(** [find_opt x s] returns the element of [s] equal to [x] (according to
    [Ord.compare]), or [None] if no such element exists. *)
val find_opt : 'elt -> 'elt t -> 'elt option

(** [find_first f s], where [f] is a monotonically increasing function, returns
    the lowest element [e] of [s] such that [f e], or raises [Not_found] if no
    such element exists.

    For example, [find_first (fun e -> Ord.compare e x >= 0) s] will return the
    first element [e] of [s] where [Ord.compare e x >= 0] (intuitively: [e >= x]),
    or raise [Not_found] if [x] is greater than any element of [s]. *)
val find_first : ('elt -> bool) -> 'elt t -> 'elt

(** [find_first_opt f s], where [f] is a monotonically increasing function,
    returns an option containing the lowest element [e] of [s] such that [f e],
    or [None] if no such element exists. *)
val find_first_opt : ('elt -> bool) -> 'elt t -> 'elt option

(** [find_last f s], where [f] is a monotonically decreasing function, returns
    the highest element [e] of [s] such that [f e], or raises [Not_found] if no
    such element exists. *)
val find_last : ('elt -> bool) -> 'elt t -> 'elt

(** [find_last_opt f s], where [f] is a monotonically decreasing function,
    returns an option containing the highest element [e] of [s] such that [f e],
    or [None] if no such element exists. *)
val find_last_opt : ('elt -> bool) -> 'elt t -> 'elt option

(** {1:traversing Traversing} *)

(** [iter f s] applies [f] in turn to all elements of [s]. The elements of [s]
    are presented to [f] in increasing order with respect to the ordering over
    the type of the elements. *)
val iter : ('elt -> unit) -> 'elt t -> unit

(** [fold f s init] computes [(f xN ... (f x2 (f x1 init))...)], where
    [x1 ... xN] are the elements of [s], in increasing order. *)
val fold : ('elt -> 'acc -> 'acc) -> 'elt t -> 'acc -> 'acc

(** {1:transforming Transforming} *)

(** [map f s] is the set whose elements are [f a0],[f a1]... [f aN],
    where [a0],[a1]...[aN] are the elements of [s].

    The elements are passed to [f] in increasing order with respect to the
    ordering over the type of the elements.

    If no element of [s] is changed by [f], [s] is returned unchanged. (If each
    output of [f] is physically equal to its input, the returned set is
    physically equal to [s].) *)
val map : ('elt -> 'elt) -> 'elt t -> 'elt t

(** [filter f s] returns the set of all elements in [s] that satisfy predicate
    [f]. If [f] satisfies every element in [s], [s] is returned unchanged (the
    result of the function is then physically equal to [s]). *)
val filter : ('elt -> bool) -> 'elt t -> 'elt t

(** [filter_map f s] returns the set of all [v] such that [f x = Some v] for
    some element [x] of [s].

    For example,
    {[
      filter_map (fun n -> if n mod 2 = 0 then Some (n / 2) else None) s
    ]}
    is the set of halves of the even elements of [s].

    If no element of [s] is changed or dropped by [f] (if [f x = Some x] for
    each element [x]), then [s] is returned unchanged: the result of the
    function is then physically equal to [s]. *)
val filter_map : ('elt -> 'elt option) -> 'elt t -> 'elt t

(** [partition f s] returns a pair of sets [(s1, s2)], where [s1] is the set of
    all the elements of [s] that satisfy the predicate [f], and [s2] is the set
    of all the elements of [s] that do not satisfy [f]. *)
val partition : ('elt -> bool) -> 'elt t -> 'elt t * 'elt t

(** [split x s] returns a triple [(l, present, r)], where [l] is the set of
    elements of [s] that are strictly less than [x]; [r] is the set of elements
    of [s] that are strictly greater than [x]; [present] is [false] if [s]
    contains no element equal to [x], or [true] if [s] contains an element equal
    to [x]. *)
val split : 'elt -> 'elt t -> 'elt t * bool * 'elt t

(** {1:predicates Predicates and comparisons} *)

(** Test whether a set is empty or not. *)
val is_empty : _ t -> bool

(** Test whether a set has exactly one element or not. *)
val is_singleton : _ t -> bool

(** [mem x s] tests whether [x] belongs to the set [s]. *)
val mem : 'elt -> 'elt t -> bool

(** [equal s1 s2] tests whether the sets [s1] and [s2] are equal, that is,
    contain equal elements.
    @raise Invalid_argument if the sets have different compare functions. *)
val equal : 'elt t -> 'elt t -> bool

(** Total ordering between sets. Can be used as the ordering function for doing
    sets of sets.
    @raise Invalid_argument if the sets have different compare functions. *)
val compare : 'elt t -> 'elt t -> int

(** [subset s1 s2] tests whether the set [s1] is a subset of the set [s2].
    @raise Invalid_argument if the sets have different compare functions. *)
val subset : 'elt t -> 'elt t -> bool

(** [for_all f s] checks if all elements of the set satisfy the predicate
    [f]. *)
val for_all : ('elt -> bool) -> 'elt t -> bool

(** [exists f s] checks if at least one element of the set satisfies the
    predicate [f]. *)
val exists : ('elt -> bool) -> 'elt t -> bool

(** {1:converting Converting} *)

(** [to_list s] is {!val:elements}[ s]. *)
val to_list : 'elt t -> 'elt list

(** [of_list (module Ord) l] creates a set from a list of elements using
    [Ord.compare] ordering. This is usually more efficient than folding [add]
    over the list, except perhaps for lists with many duplicated elements. *)
val of_list : (module OrderedType with type t = 'elt) -> 'elt list -> 'elt t

(** [to_seq_from x s] iterates on a subset of the elements of [s] in ascending
    order, from [x] or above. *)
val to_seq_from : 'elt -> 'elt t -> 'elt Seq.t

(** Iterate on the whole set, in ascending order. *)
val to_seq : 'elt t -> 'elt Seq.t

(** Iterate on the whole set, in descending order. *)
val to_rev_seq : 'elt t -> 'elt Seq.t

(** Add the given elements to the set, in order. *)
val add_seq : 'elt Seq.t -> 'elt t -> 'elt t

(** Build a set from the given elements. *)
val of_seq : (module OrderedType with type t = 'elt) -> 'elt Seq.t -> 'elt t
