(*_**********************************************************************************)
(*_  nofunc-keyed: Keyed data structures adapted from OCaml Stdlib but no functors  *)
(*_  SPDX-FileCopyrightText: 2025 Mathieu Barbin <mathieu.barbin@gmail.com>         *)
(*_  SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception   *)
(*_**********************************************************************************)

(*_ Notice: This file was copied from OCaml Stdlib:

  path: "stdlib/map.mli" ; rev: f8ea2c42144f416f4d7a5d71a0bb2c766ca8fedc

  The original license header was kept with the file, see below.

  List of changes:

  - Format file with ocamlformat

  - Remove the functor and signature. Make the type parametrized by the type of
    keys and data. Require [(module Ord)] as first-class module argument
    everywhere needed.

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

(** Association tables over ordered types.

    This interface is adapted from OCaml's stdlib and inspired by the Base
    container.

    The compare function is taken from a first-class module supplied as argument
    and stored with the map to be used when needed.

    When a function operates on multiple maps, the code will raise
    [Invalid_argument] if the compare functions are not all physical equal,
    ensuring the consistency of the computed values. *)

(** Input signature for key comparison. *)
module type OrderedType = sig
  (** The type of the map keys. *)
  type t

  (** A total ordering function over the keys. This is a two-argument function
      [f] such that [f e1 e2] is zero if the keys [e1] and [e2] are equal,
      [f e1 e2] is strictly negative if [e1] is smaller than [e2], and [f e1 e2]
      is strictly positive if [e1] is greater than [e2]. Example: a suitable
      ordering function is the generic structural comparison function
      [Stdlib.compare]. *)
  val compare : t -> t -> int
end

(** {1:maps Maps} *)

(** The type of maps from type ['key] to type ['a]. *)
type ('key, !+'a) t

(** [empty (module Ord)] returns an empty map using [Ord.compare] for key
    ordering. *)
val empty : (module OrderedType with type t = 'key) -> ('key, 'a) t

(** [add key data m] returns a map containing the same bindings as [m], plus a
    binding of [key] to [data]. If [key] was already bound in [m] to a value
    that is physically equal to [data], [m] is returned unchanged (the result of
    the function is then physically equal to [m]). Otherwise, the previous
    binding of [key] in [m] disappears. *)
val add : 'key -> 'a -> ('key, 'a) t -> ('key, 'a) t

(** [add_to_list key data m] is [m] with [key] mapped to [l] such that [l] is
    [data :: Map.find key m] if [key] was bound in [m] and [[data]] otherwise. *)
val add_to_list : 'key -> 'a -> ('key, 'a list) t -> ('key, 'a list) t

(** [update key f m] returns a map containing the same bindings as [m], except
    for the binding of [key]. Depending on the value of [y] where [y] is
    [f (find_opt key m)], the binding of [key] is added, removed or updated. If
    [y] is [None], the binding is removed if it exists; otherwise, if [y] is
    [Some z] then [key] is associated to [z] in the resulting map. If [key] was
    already bound in [m] to a value that is physically equal to [z], [m] is
    returned unchanged (the result of the function is then physically equal to
    [m]). *)
val update : 'key -> ('a option -> 'a option) -> ('key, 'a) t -> ('key, 'a) t

(** [singleton (module Ord) x y] returns the one-element map that contains a
    binding [y] for [x], using [Ord.compare] for ordering. *)
val singleton : (module OrderedType with type t = 'key) -> 'key -> 'a -> ('key, 'a) t

(** [remove x m] returns a map containing the same bindings as [m], except for
    [x] which is unbound in the returned map. If [x] was not in [m], [m] is
    returned unchanged (the result of the function is then physically equal to
    [m]). *)
val remove : 'key -> ('key, 'a) t -> ('key, 'a) t

(** [merge f m1 m2] computes a map whose keys are a subset of the keys of [m1]
    and of [m2]. The presence of each such binding, and the corresponding value,
    is determined with the function [f]. In terms of the [find_opt] operation,
    we have [find_opt x (merge f m1 m2) = f x (find_opt x m1) (find_opt x m2)]
    for any key [x], provided that [f x None None = None]. Raise
    [Invalid_argument] if the maps have different compare functions. *)
val merge
  :  ('key -> 'a option -> 'b option -> 'c option)
  -> ('key, 'a) t
  -> ('key, 'b) t
  -> ('key, 'c) t

(** [union f m1 m2] computes a map whose keys are a subset of the keys of [m1]
    and of [m2]. When the same binding is defined in both arguments, the
    function [f] is used to combine them. This is a special case of [merge]:
    [union f m1 m2] is equivalent to [merge f' m1 m2], where
    - [f' _key None None = None]
    - [f' _key (Some v) None = Some v]
    - [f' _key None (Some v) = Some v]
    - [f' key (Some v1) (Some v2) = f key v1 v2]

    Raise [Invalid_argument] if the maps have different compare functions. *)
val union
  :  ('key -> 'a -> 'a -> 'a option)
  -> ('key, 'a) t
  -> ('key, 'a) t
  -> ('key, 'a) t

(** Return the number of bindings of a map. *)
val cardinal : ('key, 'a) t -> int

(** Return the list of all bindings of the given map. The returned list is
    sorted in increasing order of keys with respect to the ordering
    [Ord.compare] used to build the map. *)
val bindings : ('key, 'a) t -> ('key * 'a) list

(** Return the binding with the smallest key in a given map (with respect to the
    [Ord.compare] ordering), or raise [Not_found] if the map is empty. *)
val min_binding : ('key, 'a) t -> 'key * 'a

(** Return the binding with the smallest key in the given map (with respect to
    the [Ord.compare] ordering), or [None] if the map is empty. *)
val min_binding_opt : ('key, 'a) t -> ('key * 'a) option

(** Same as {!min_binding}, but returns the binding with the largest key in the
    given map. *)
val max_binding : ('key, 'a) t -> 'key * 'a

(** Same as {!min_binding_opt}, but returns the binding with the largest key in
    the given map. *)
val max_binding_opt : ('key, 'a) t -> ('key * 'a) option

(** Return one binding of the given map, or raise [Not_found] if the map is
    empty. Which binding is chosen is unspecified, but equal bindings will be
    chosen for equal maps. *)
val choose : ('key, 'a) t -> 'key * 'a

(** Return one binding of the given map, or [None] if the map is empty. Which
    binding is chosen is unspecified, but equal bindings will be chosen for
    equal maps. *)
val choose_opt : ('key, 'a) t -> ('key * 'a) option

(** {1:searching Searching} *)

(** [find x m] returns the current value of [x] in [m], or raises [Not_found] if
    no binding for [x] exists. *)
val find : 'key -> ('key, 'a) t -> 'a

(** [find_opt x m] returns [Some v] if the current value of [x] in [m] is [v],
    or [None] if no binding for [x] exists. *)
val find_opt : 'key -> ('key, 'a) t -> 'a option

(** [find_first f m], where [f] is a monotonically increasing function, returns
    the binding of [m] with the lowest key [k] such that [f k], or raises
    [Not_found] if no such key exists.

    For example, [find_first (fun k -> Ord.compare k x >= 0) m] will return the
    first binding [k, v] of [m] where [Ord.compare k x >= 0]
    (intuitively: [k >= x]), or raise [Not_found] if [x] is greater than any
    element of [m]. *)
val find_first : ('key -> bool) -> ('key, 'a) t -> 'key * 'a

(** [find_first_opt f m], where [f] is a monotonically increasing function,
    returns an option containing the binding of [m] with the lowest key [k] such
    that [f k], or [None] if no such key exists. *)
val find_first_opt : ('key -> bool) -> ('key, 'a) t -> ('key * 'a) option

(** [find_last f m], where [f] is a monotonically decreasing function, returns
    the binding of [m] with the highest key [k] such that [f k], or raises
    [Not_found] if no such key exists. *)
val find_last : ('key -> bool) -> ('key, 'a) t -> 'key * 'a

(** [find_last_opt f m], where [f] is a monotonically decreasing function,
    returns an option containing the binding of [m] with the highest key [k]
    such that [f k], or [None] if no such key exists. *)
val find_last_opt : ('key -> bool) -> ('key, 'a) t -> ('key * 'a) option

(** {1:traversing Traversing} *)

(** [iter f m] applies [f] to all bindings in map [m]. [f] receives the key as
    first argument, and the associated value as second argument. The bindings
    are passed to [f] in increasing order with respect to the ordering over the
    type of the keys. *)
val iter : ('key -> 'a -> unit) -> ('key, 'a) t -> unit

(** [fold f m init] computes [(f kN dN ... (f k1 d1 init)...)], where
    [k1 ... kN] are the keys of all bindings in [m] (in increasing order), and
    [d1 ... dN] are the associated data. *)
val fold : ('key -> 'a -> 'acc -> 'acc) -> ('key, 'a) t -> 'acc -> 'acc

(** {1:transforming Transforming} *)

(** [map f m] returns a map with same domain as [m], where the associated value
    [a] of all bindings of [m] has been replaced by the result of the
    application of [f] to [a]. The bindings are passed to [f] in increasing
    order with respect to the ordering over the type of the keys. *)
val map : ('a -> 'b) -> ('key, 'a) t -> ('key, 'b) t

(** Same as {!map}, but the function receives as arguments both the key and the
    associated value for each binding of the map. *)
val mapi : ('key -> 'a -> 'b) -> ('key, 'a) t -> ('key, 'b) t

(** [filter f m] returns the map with all the bindings in [m] that satisfy
    predicate [p]. If every binding in [m] satisfies [f], [m] is returned
    unchanged (the result of the function is then physically equal to [m]) *)
val filter : ('key -> 'a -> bool) -> ('key, 'a) t -> ('key, 'a) t

(** [filter_map f m] applies the function [f] to every binding of [m], and
    builds a map from the results. For each binding [(k, v)] in the input map:
    - if [f k v] is [None] then [k] is not in the result,
    - if [f k v] is [Some v'] then the binding [(k, v')] is in the output map.

    For example, the following function on maps whose values are lists
    {[
      filter_map
        (fun _k li ->
           match li with
           | [] -> None
           | _ :: tl -> Some tl)
        m
    ]}
    drops all bindings of [m] whose value is an empty list, and pops the first
    element of each value that is non-empty. *)
val filter_map : ('key -> 'a -> 'b option) -> ('key, 'a) t -> ('key, 'b) t

(** [partition f m] returns a pair of maps [(m1, m2)], where [m1] contains all
    the bindings of [m] that satisfy the predicate [f], and [m2] is the map with
    all the bindings of [m] that do not satisfy [f]. *)
val partition : ('key -> 'a -> bool) -> ('key, 'a) t -> ('key, 'a) t * ('key, 'a) t

(** [split x m] returns a triple [(l, data, r)], where [l] is the map with all
    the bindings of [m] whose key is strictly less than [x]; [r] is the map with
    all the bindings of [m] whose key is strictly greater than [x]; [data] is
    [None] if [m] contains no binding for [x], or [Some v] if [m] binds [v] to
    [x]. *)
val split : 'key -> ('key, 'a) t -> ('key, 'a) t * 'a option * ('key, 'a) t

(** {1:predicates Predicates and comparisons} *)

(** Test whether a map is empty or not. *)
val is_empty : _ t -> bool

(** Test whether a map has exactly one element or not. *)
val is_singleton : _ t -> bool

(** [mem x m] returns [true] if [m] contains a binding for [x], and [false]
    otherwise. *)
val mem : 'key -> ('key, _) t -> bool

(** [equal cmp m1 m2] tests whether the maps [m1] and [m2] are equal, that is,
    contain equal keys and associate them with equal data. [cmp] is the equality
    predicate used to compare the data associated with the keys. *)
val equal : ('a -> 'a -> bool) -> ('key, 'a) t -> ('key, 'a) t -> bool

(** Total ordering between maps. The first argument is a total ordering used to
    compare data associated with equal keys in the two maps. *)
val compare : ('a -> 'a -> int) -> ('key, 'a) t -> ('key, 'a) t -> int

(** [for_all f m] checks if all the bindings of the map satisfy the predicate
    [f]. *)
val for_all : ('key -> 'a -> bool) -> ('key, 'a) t -> bool

(** [exists f m] checks if at least one binding of the map satisfies the
    predicate [f]. *)
val exists : ('key -> 'a -> bool) -> ('key, 'a) t -> bool

(** {1:converting Converting} *)

(** [to_list m] is {!val:bindings}[m]. *)
val to_list : ('key, 'a) t -> ('key * 'a) list

(** [of_list (module Ord) bs] adds the bindings of [bs] to the empty map, in
    list order (if a key is bound twice in [bs] the last one takes over). *)
val of_list : (module OrderedType with type t = 'key) -> ('key * 'a) list -> ('key, 'a) t

(** Iterate on the whole map, in ascending order of keys. *)
val to_seq : ('key, 'a) t -> ('key * 'a) Seq.t

(** Iterate on the whole map, in descending order of keys. *)
val to_rev_seq : ('key, 'a) t -> ('key * 'a) Seq.t

(** [to_seq_from k m] iterates on a subset of the bindings of [m], in ascending
    order of keys, from key [k] or above. *)
val to_seq_from : 'key -> ('key, 'a) t -> ('key * 'a) Seq.t

(** Add the given bindings to the map, in order. *)
val add_seq : ('key * 'a) Seq.t -> ('key, 'a) t -> ('key, 'a) t

(** Build a map from the given bindings. *)
val of_seq : (module OrderedType with type t = 'key) -> ('key * 'a) Seq.t -> ('key, 'a) t
