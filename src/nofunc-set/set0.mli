(*_**********************************************************************************)
(*_  nofunc-keyed: Keyed data structures adapted from OCaml Stdlib but no functors  *)
(*_  SPDX-FileCopyrightText: 2025-2026 Mathieu Barbin <mathieu.barbin@gmail.com>    *)
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

  - Document which functions raise when operating on incompatible inputs.

  - Require [Ord.compare] to return [Ordering.t] instead of [int].

  - Take the set as the first argument. Label closures [~f].

  - Add a "Modular explicit usage" section, with a Base-style [M] functor and
    [sexp_of_m__t] / [dyn_of_m__t] derivers.

  - Add a phantom [comparator_witness] second type parameter, Base-style, so
    that operations expecting compatible sets (such as [union]) unify it
    across their arguments, while operations that ignore it (such as [mem])
    leave it as a wildcard. *)

(*_*************************************************************************)
(*_                                                                        *)
(*_                                 OCaml                                  *)
(*_                                                                        *)
(*_             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*_                                                                        *)
(*_   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*_     en Automatique.                                                    *)
(*_                                                                        *)
(*_   All rights reserved.  This file is distributed under the terms of    *)
(*_   the GNU Lesser General Public License version 2.1, with the          *)
(*_   special exception on linking described in the file LICENSE.          *)
(*_                                                                        *)
(*_*************************************************************************)

(** Sets over ordered types.

    This interface is adapted from OCaml's stdlib and inspired by the Base
    container.

    The compare function is taken from a first-class module supplied as argument
    and stored with the set to be used when needed.

    When a function operates on multiple sets, the code will raise
    [Invalid_argument] if the compare functions are not all physical equal,
    ensuring the consistency of the computed values. The second type parameter
    of {!type:t}, ['cmp], is a phantom [comparator_witness] used to catch
    inconsistent usage at compile time too: functions that combine several
    sets (such as {!val:union}) require them to share the same ['cmp], while
    functions that don't care about it (such as {!val:mem}) leave it as a
    wildcard. *)

(** Input signature for element comparison. *)
module type OrderedType = sig
  (** The type of the set elements. *)
  type t

  (** A phantom type used to track, at the type level, which [compare]
      function a given {!type:t} was built with. There is no implementation
      requirement attached to this type: it is only used as a marker. Modules
      meant to be used as distinct comparators should each declare their own
      abstract [comparator_witness]; modules that are happy to be considered
      interchangeable (e.g. because one is a thin wrapper around another) may
      share the same one. Sharing a witness between modules whose [compare]
      functions don't actually agree is not caught by the type system: it is
      the responsibility of the comparator author not to claim an existing
      witness unless the two are genuinely interchangeable. The runtime
      physical-equality check on [compare] (see above) remains the backstop
      that catches such a mismatch, raising [Invalid_argument]. *)
  type comparator_witness

  (** A total ordering function over the set elements. *)
  val compare : t -> t -> Ordering.t
end

(** {1:sets Sets} *)

(** The type of sets of elements of type ['elt], built using a comparison
    function witnessed by ['cmp]. *)
type (!'elt, !'cmp) t

(** [empty (module Ord)] returns an empty set using [Ord.compare] for ordering. *)
val empty
  :  (module OrderedType with type t = 'elt and type comparator_witness = 'cmp)
  -> ('elt, 'cmp) t

(** [add s x] returns a set containing all elements of [s], plus [x]. If [x] was
    already in [s], [s] is returned unchanged (the result of the function is
    then physically equal to [s]). *)
val add : ('elt, 'cmp) t -> 'elt -> ('elt, 'cmp) t

(** [singleton (module Ord) x] returns the one-element set containing only [x],
    using [Ord.compare] for ordering. *)
val singleton
  :  (module OrderedType with type t = 'elt and type comparator_witness = 'cmp)
  -> 'elt
  -> ('elt, 'cmp) t

(** [remove s x] returns a set containing all elements of [s], except [x]. If
    [x] was not in [s], [s] is returned unchanged (the result of the function is
    then physically equal to [s]). *)
val remove : ('elt, 'cmp) t -> 'elt -> ('elt, 'cmp) t

(** Set union. Raise [Invalid_argument] if the sets have different compare
    functions. *)
val union : ('elt, 'cmp) t -> ('elt, 'cmp) t -> ('elt, 'cmp) t

(** Set intersection. Raise [Invalid_argument] if the sets have different
    compare functions. *)
val inter : ('elt, 'cmp) t -> ('elt, 'cmp) t -> ('elt, 'cmp) t

(** Test if two sets are disjoint. Raise [Invalid_argument] if the sets have
    different compare functions. *)
val disjoint : ('elt, 'cmp) t -> ('elt, 'cmp) t -> bool

(** Set difference: [diff s1 s2] contains the elements of [s1] that are not in
    [s2]. Raise [Invalid_argument] if the sets have different compare
    functions. *)
val diff : ('elt, 'cmp) t -> ('elt, 'cmp) t -> ('elt, 'cmp) t

(** Return the number of elements of a set. *)
val cardinal : (_, _) t -> int

(** {1:elements Elements} *)

(** Return the list of all elements of the given set. The returned list is
    sorted in increasing order with respect to the ordering [Ord.compare] used
    to create the set. *)
val elements : ('elt, _) t -> 'elt list

(** Return the smallest element of the given set (with respect to the
    [Ord.compare] ordering used to create the set), or raise [Not_found] if the
    set is empty. *)
val min_elt : ('elt, _) t -> 'elt

(** Return the smallest element of the given set (with respect to the
    [Ord.compare] ordering used to create the set), or [None] if the set is
    empty. *)
val min_elt_opt : ('elt, _) t -> 'elt option

(** Same as {!val:min_elt}, but returns the largest element of the given set. *)
val max_elt : ('elt, _) t -> 'elt

(** Same as {!val:min_elt_opt}, but returns the largest element of the given
    set. *)
val max_elt_opt : ('elt, _) t -> 'elt option

(** Return one element of the given set, or raise [Not_found] if the set is
    empty. Which element is chosen is unspecified, but equal elements will be
    chosen for equal sets. *)
val choose : ('elt, _) t -> 'elt

(** Return one element of the given set, or [None] if the set is empty. Which
    element is chosen is unspecified, but equal elements will be chosen for
    equal sets. *)
val choose_opt : ('elt, _) t -> 'elt option

(** {1:searching Searching} *)

(** [find s x] returns the element of [s] equal to [x] (according to
    [Ord.compare]), or raise [Not_found] if no such element exists. *)
val find : ('elt, _) t -> 'elt -> 'elt

(** [find_opt s x] returns the element of [s] equal to [x] (according to
    [Ord.compare]), or [None] if no such element exists. *)
val find_opt : ('elt, _) t -> 'elt -> 'elt option

(** [find_first s ~f], where [f] is a monotonically increasing function, returns
    the lowest element [e] of [s] such that [f e], or raises [Not_found] if no
    such element exists.

    For example, [find_first s ~f:(fun e -> Ord.compare e x >= 0)] will return
    the first element [e] of [s] where [Ord.compare e x >= 0] (intuitively:
    [e >= x]), or raise [Not_found] if [x] is greater than any element of [s]. *)
val find_first : ('elt, _) t -> f:('elt -> bool) -> 'elt

(** [find_first_opt s ~f], where [f] is a monotonically increasing function,
    returns an option containing the lowest element [e] of [s] such that [f e],
    or [None] if no such element exists. *)
val find_first_opt : ('elt, _) t -> f:('elt -> bool) -> 'elt option

(** [find_last s ~f], where [f] is a monotonically decreasing function, returns
    the highest element [e] of [s] such that [f e], or raises [Not_found] if no
    such element exists. *)
val find_last : ('elt, _) t -> f:('elt -> bool) -> 'elt

(** [find_last_opt s ~f], where [f] is a monotonically decreasing function,
    returns an option containing the highest element [e] of [s] such that [f e],
    or [None] if no such element exists. *)
val find_last_opt : ('elt, _) t -> f:('elt -> bool) -> 'elt option

(** {1:traversing Traversing} *)

(** [iter s ~f] applies [f] in turn to all elements of [s]. The elements of [s]
    are presented to [f] in increasing order with respect to the ordering over
    the type of the elements. *)
val iter : ('elt, _) t -> f:('elt -> unit) -> unit

(** [fold s ~init ~f] computes [(f xN ... (f x2 (f x1 init))...)], where
    [x1 ... xN] are the elements of [s], in increasing order. [elt] is labeled
    to avoid mixing it up with the accumulator. *)
val fold : ('elt, _) t -> init:'acc -> f:(elt:'elt -> 'acc -> 'acc) -> 'acc

(** {1:transforming Transforming} *)

(** [map s ~f] is the set whose elements are [f a0],[f a1]... [f aN],
    where [a0],[a1]...[aN] are the elements of [s].

    The elements are passed to [f] in increasing order with respect to the
    ordering over the type of the elements.

    If no element of [s] is changed by [f], [s] is returned unchanged. (If each
    output of [f] is physically equal to its input, the returned set is
    physically equal to [s].) *)
val map : ('elt, 'cmp) t -> f:('elt -> 'elt) -> ('elt, 'cmp) t

(** [filter s ~f] returns the set of all elements in [s] that satisfy predicate
    [f]. If [f] satisfies every element in [s], [s] is returned unchanged (the
    result of the function is then physically equal to [s]). *)
val filter : ('elt, 'cmp) t -> f:('elt -> bool) -> ('elt, 'cmp) t

(** [filter_map s ~f] returns the set of all [v] such that [f x = Some v] for
    some element [x] of [s].

    For example,
    {[
    filter_map s ~f:(fun n -> if n mod 2 = 0 then Some (n / 2) else None)
    ]}
    is the set of halves of the even elements of [s].

    If no element of [s] is changed or dropped by [f] (if [f x = Some x] for
    each element [x]), then [s] is returned unchanged: the result of the
    function is then physically equal to [s]. *)
val filter_map : ('elt, 'cmp) t -> f:('elt -> 'elt option) -> ('elt, 'cmp) t

(** [partition s ~f] returns a pair of sets [(s1, s2)], where [s1] is the set of
    all the elements of [s] that satisfy the predicate [f], and [s2] is the set
    of all the elements of [s] that do not satisfy [f]. *)
val partition : ('elt, 'cmp) t -> f:('elt -> bool) -> ('elt, 'cmp) t * ('elt, 'cmp) t

(** [split s x] returns a triple [(l, present, r)], where [l] is the set of
    elements of [s] that are strictly less than [x]; [r] is the set of elements
    of [s] that are strictly greater than [x]; [present] is [false] if [s]
    contains no element equal to [x], or [true] if [s] contains an element equal
    to [x]. *)
val split : ('elt, 'cmp) t -> 'elt -> ('elt, 'cmp) t * bool * ('elt, 'cmp) t

(** {1:predicates Predicates and comparisons} *)

(** Test whether a set is empty or not. *)
val is_empty : (_, _) t -> bool

(** Test whether a set has exactly one element or not. *)
val is_singleton : (_, _) t -> bool

(** [mem s x] tests whether [x] belongs to the set [s]. *)
val mem : ('elt, _) t -> 'elt -> bool

(** [equal s1 s2] tests whether the sets [s1] and [s2] are equal, that is,
    contain equal elements. Raise [Invalid_argument] if the sets have different
    compare functions. *)
val equal : ('elt, 'cmp) t -> ('elt, 'cmp) t -> bool

(** Total ordering between sets. Can be used as the ordering function for doing
    sets of sets. Raise [Invalid_argument] if the sets have different compare
    functions. *)
val compare : ('elt, 'cmp) t -> ('elt, 'cmp) t -> Ordering.t

(** [subset s1 s2] tests whether the set [s1] is a subset of the set [s2]. Raise
    [Invalid_argument] if the sets have different compare functions. *)
val subset : ('elt, 'cmp) t -> ('elt, 'cmp) t -> bool

(** [for_all s ~f] checks if all elements of the set satisfy the predicate
    [f]. *)
val for_all : ('elt, _) t -> f:('elt -> bool) -> bool

(** [exists s ~f] checks if at least one element of the set satisfies the
    predicate [f]. *)
val exists : ('elt, _) t -> f:('elt -> bool) -> bool

(** {1:converting Converting} *)

(** [to_list s] is {!val:elements}[ s]. *)
val to_list : ('elt, _) t -> 'elt list

(** [of_list (module Ord) l] creates a set from a list of elements using
    [Ord.compare] ordering. This is usually more efficient than folding [add]
    over the list, except perhaps for lists with many duplicated elements. *)
val of_list
  :  (module OrderedType with type t = 'elt and type comparator_witness = 'cmp)
  -> 'elt list
  -> ('elt, 'cmp) t

(** [to_seq_from s x] iterates on a subset of the elements of [s] in ascending
    order, from [x] or above. *)
val to_seq_from : ('elt, _) t -> 'elt -> 'elt Seq.t

(** Iterate on the whole set, in ascending order. *)
val to_seq : ('elt, _) t -> 'elt Seq.t

(** Iterate on the whole set, in descending order. *)
val to_rev_seq : ('elt, _) t -> 'elt Seq.t

(** Add the given elements to the set, in order. *)
val add_seq : ('elt, 'cmp) t -> 'elt Seq.t -> ('elt, 'cmp) t

(** Build a set from the given elements. *)
val of_seq
  :  (module OrderedType with type t = 'elt and type comparator_witness = 'cmp)
  -> 'elt Seq.t
  -> ('elt, 'cmp) t

(** {1:modular Modular explicit usage}

    The declarations below offer an alternative, module-based style for fixing
    the element type (and its comparator witness) at a single first-class
    module, in the tradition of Base's [Set.M(Elt).t]. They also provide ways
    to derive [sexp_of_t] and [to_dyn] for a set, given a first-class module
    for the elements. *)

module M (T : sig
    type t
    type comparator_witness
  end) : sig
  type nonrec t = (T.t, T.comparator_witness) t
end

(** Input signature for {!val:sexp_of_m__t}. *)
module type Sexpable = sig
  type t

  val sexp_of_t : t -> Sexplib0.Sexp.t
end

(** [sexp_of_m__t (module Elt) s] converts [s] to an s-expression, listing the
    elements in the order given by {!val:elements}. *)
val sexp_of_m__t : (module Sexpable with type t = 'elt) -> ('elt, _) t -> Sexplib0.Sexp.t

(** Input signature for {!val:dyn_of_m__t}. *)
module type Dynable = sig
  type t

  val to_dyn : t -> Dyn.t
end

(** [dyn_of_m__t (module Elt) s] converts [s] to a [Dyn.t], built the same way
    as {!val:sexp_of_m__t}. *)
val dyn_of_m__t : (module Dynable with type t = 'elt) -> ('elt, _) t -> Dyn.t
