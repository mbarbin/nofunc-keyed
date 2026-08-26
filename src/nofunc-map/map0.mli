(*_**********************************************************************************)
(*_  nofunc-keyed: Keyed data structures adapted from OCaml Stdlib but no functors  *)
(*_  SPDX-FileCopyrightText: 2025-2026 Mathieu Barbin <mathieu.barbin@gmail.com>    *)
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

  - Document which functions raise when operating on incompatible inputs.

  - Require [Ord.compare] to return [Ordering.t] instead of [int].

  - Take the map as the first argument. Label closures [~f], and the key and
    data of a binding [~key] and [~data].

  - Add a "Modular explicit usage" section, with a Base-style [M] functor and
    [sexp_of_m__t] / [dyn_of_m__t] derivers.

  - Add a phantom [comparator_witness] third type parameter, Base-style, so
    that operations expecting compatible maps (such as [union]) unify it
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

(** Association tables over ordered types.

    This interface is adapted from OCaml's stdlib and inspired by the Base
    container.

    The compare function is taken from a first-class module supplied as argument
    and stored with the map to be used when needed.

    When a function operates on multiple maps, the code will raise
    [Invalid_argument] if the compare functions are not all physical equal,
    ensuring the consistency of the computed values. The third type parameter
    of {!type:t}, ['cmp], is a phantom [comparator_witness] used to catch
    inconsistent usage at compile time too: functions that combine several
    maps (such as {!val:union}) require them to share the same ['cmp], while
    functions that don't care about it (such as {!val:mem}) leave it as a
    wildcard. *)

(** Input signature for key comparison. *)
module type OrderedType = sig
  (** The type of the map keys. *)
  type t

  (** A phantom type used to track, at the type level, which [compare]
      function a given {!type:t} was built with. There is no implementation
      requirement attached to this type: it is only used as a marker. Modules
      meant to be used as distinct comparators should each declare their own
      abstract [comparator_witness]; modules that are happy to be considered
      interchangeable (e.g. because one is a thin wrapper around another) may
      share the same one. *)
  type comparator_witness

  (** A total ordering function over the keys. This is a two-argument function
      [f] such that [f e1 e2] is [Ordering.Eq] if the keys [e1] and [e2] are
      equal, [f e1 e2] is [Ordering.Lt] if [e1] is smaller than [e2], and
      [f e1 e2] is [Ordering.Gt] if [e1] is greater than [e2]. *)
  val compare : t -> t -> Ordering.t
end

(** {1:maps Maps} *)

(** The type of maps from type ['key] to type ['a], built using a comparison
    function witnessed by ['cmp]. *)
type (!'key, !+'a, 'cmp) t

(** [empty (module Ord)] returns an empty map using [Ord.compare] for key
    ordering. *)
val empty
  :  (module OrderedType with type t = 'key and type comparator_witness = 'cmp)
  -> ('key, 'a, 'cmp) t

(** [add m ~key ~data] returns a map containing the same bindings as [m], plus a
    binding of [key] to [data]. If [key] was already bound in [m] to a value
    that is physically equal to [data], [m] is returned unchanged (the result of
    the function is then physically equal to [m]). Otherwise, the previous
    binding of [key] in [m] disappears. *)
val add : ('key, 'a, 'cmp) t -> key:'key -> data:'a -> ('key, 'a, 'cmp) t

(** [add_to_list m ~key ~data] is [m] with [key] mapped to [l] such that [l] is
    [data :: find_exn m ~key] if [key] was bound in [m] and [[data]] otherwise. *)
val add_to_list
  :  ('key, 'a list, 'cmp) t
  -> key:'key
  -> data:'a
  -> ('key, 'a list, 'cmp) t

(** [update m key ~f] returns a map containing the same bindings as [m], except
    for the binding of [key]. Depending on the value of [y] where [y] is
    [f (find m key)], the binding of [key] is added, removed or updated. If
    [y] is [None], the binding is removed if it exists; otherwise, if [y] is
    [Some z] then [key] is associated to [z] in the resulting map. If [key] was
    already bound in [m] to a value that is physically equal to [z], [m] is
    returned unchanged (the result of the function is then physically equal to
    [m]). *)
val update
  :  ('key, 'a, 'cmp) t
  -> 'key
  -> f:('a option -> 'a option)
  -> ('key, 'a, 'cmp) t

(** [singleton (module Ord) ~key ~data] returns the one-element map that contains
    a binding [data] for [key], using [Ord.compare] for ordering. *)
val singleton
  :  (module OrderedType with type t = 'key and type comparator_witness = 'cmp)
  -> key:'key
  -> data:'a
  -> ('key, 'a, 'cmp) t

(** [remove m key] returns a map containing the same bindings as [m], except for
    [key] which is unbound in the returned map. If [key] was not in [m], [m] is
    returned unchanged (the result of the function is then physically equal to
    [m]). *)
val remove : ('key, 'a, 'cmp) t -> 'key -> ('key, 'a, 'cmp) t

(** [merge m1 m2 ~f] computes a map whose keys are a subset of the keys of [m1]
    and of [m2]. The presence of each such binding, and the corresponding value,
    is determined with the function [f]. In terms of the [find] operation,
    we have
    [find (merge m1 m2 ~f) key = f ~key ~left:(find m1 key) ~right:(find m2 key)]
    for any key [key], provided that [f ~key ~left:None ~right:None = None].
    [left] and [right] are labeled to avoid mixing up which map each value
    comes from. Raise [Invalid_argument] if the maps have different compare
    functions. *)
val merge
  :  ('key, 'a, 'cmp) t
  -> ('key, 'b, 'cmp) t
  -> f:(key:'key -> left:'a option -> right:'b option -> 'c option)
  -> ('key, 'c, 'cmp) t

(** [union m1 m2 ~f] computes a map whose keys are a subset of the keys of [m1]
    and of [m2]. When the same binding is defined in both arguments, the
    function [f] is used to combine them. This is a special case of [merge]:
    [union m1 m2 ~f] is equivalent to [merge m1 m2 ~f:f'], where
    - [f' ~key ~left:None ~right:None = None]
    - [f' ~key ~left:(Some v) ~right:None = Some v]
    - [f' ~key ~left:None ~right:(Some v) = Some v]
    - [f' ~key ~left:(Some v1) ~right:(Some v2) = f ~key ~left:v1 ~right:v2]

    [left] and [right] are labeled to avoid mixing up which map each value
    comes from, especially since both have the same type. Raise
    [Invalid_argument] if the maps have different compare functions. *)
val union
  :  ('key, 'a, 'cmp) t
  -> ('key, 'a, 'cmp) t
  -> f:(key:'key -> left:'a -> right:'a -> 'a option)
  -> ('key, 'a, 'cmp) t

(** Return the number of bindings of a map. *)
val cardinal : (_, _, _) t -> int

(** Return the list of all bindings of the given map. The returned list is
    sorted in increasing order of keys with respect to the ordering
    [Ord.compare] used to build the map. *)
val bindings : ('key, 'a, _) t -> ('key * 'a) list

(** Return the binding with the smallest key in a given map (with respect to the
    [Ord.compare] ordering), or raise [Not_found] if the map is empty. *)
val min_binding : ('key, 'a, _) t -> 'key * 'a

(** Return the binding with the smallest key in the given map (with respect to
    the [Ord.compare] ordering), or [None] if the map is empty. *)
val min_binding_opt : ('key, 'a, _) t -> ('key * 'a) option

(** Same as {!min_binding}, but returns the binding with the largest key in the
    given map. *)
val max_binding : ('key, 'a, _) t -> 'key * 'a

(** Same as {!min_binding_opt}, but returns the binding with the largest key in
    the given map. *)
val max_binding_opt : ('key, 'a, _) t -> ('key * 'a) option

(** Return one binding of the given map, or raise [Not_found] if the map is
    empty. Which binding is chosen is unspecified, but equal bindings will be
    chosen for equal maps. *)
val choose : ('key, 'a, _) t -> 'key * 'a

(** Return one binding of the given map, or [None] if the map is empty. Which
    binding is chosen is unspecified, but equal bindings will be chosen for
    equal maps. *)
val choose_opt : ('key, 'a, _) t -> ('key * 'a) option

(** {1:searching Searching} *)

(** [find m key] returns [Some v] if the current value of [key] in [m] is
    [v], or [None] if no binding for [key] exists. *)
val find : ('key, 'a, _) t -> 'key -> 'a option

(** [find_exn m key] returns the current value of [key] in [m], or raises
    [Not_found] if no binding for [key] exists. *)
val find_exn : ('key, 'a, _) t -> 'key -> 'a

(** [find_first m ~f], where [f] is a monotonically increasing function, returns
    the binding of [m] with the lowest key [k] such that [f k], or raises
    [Not_found] if no such key exists.

    For example, [find_first m ~f:(fun k -> Ord.compare k x >= 0)] will return
    the first binding [k, v] of [m] where [Ord.compare k x >= 0]
    (intuitively: [k >= x]), or raise [Not_found] if [x] is greater than any
    element of [m]. *)
val find_first : ('key, 'a, _) t -> f:('key -> bool) -> 'key * 'a

(** [find_first_opt m ~f], where [f] is a monotonically increasing function,
    returns an option containing the binding of [m] with the lowest key [k] such
    that [f k], or [None] if no such key exists. *)
val find_first_opt : ('key, 'a, _) t -> f:('key -> bool) -> ('key * 'a) option

(** [find_last m ~f], where [f] is a monotonically decreasing function, returns
    the binding of [m] with the highest key [k] such that [f k], or raises
    [Not_found] if no such key exists. *)
val find_last : ('key, 'a, _) t -> f:('key -> bool) -> 'key * 'a

(** [find_last_opt m ~f], where [f] is a monotonically decreasing function,
    returns an option containing the binding of [m] with the highest key [k]
    such that [f k], or [None] if no such key exists. *)
val find_last_opt : ('key, 'a, _) t -> f:('key -> bool) -> ('key * 'a) option

(** {1:traversing Traversing} *)

(** [iter m ~f] applies [f] to all bindings in map [m]. [key] and [data] are
    labeled to avoid mixing them up. The bindings are passed to [f] in
    increasing order with respect to the ordering over the type of the keys. *)
val iter : ('key, 'a, _) t -> f:(key:'key -> data:'a -> unit) -> unit

(** [fold m ~init ~f] computes [(f kN dN ... (f k1 d1 init)...)], where
    [k1 ... kN] are the keys of all bindings in [m] (in increasing order), and
    [d1 ... dN] are the associated data. [key] and [data] are labeled to avoid
    mixing them up with the accumulator. *)
val fold : ('key, 'a, _) t -> init:'acc -> f:(key:'key -> data:'a -> 'acc -> 'acc) -> 'acc

(** {1:transforming Transforming} *)

(** [map m ~f] returns a map with same domain as [m], where the associated value
    [a] of all bindings of [m] has been replaced by the result of the
    application of [f] to [a]. The bindings are passed to [f] in increasing
    order with respect to the ordering over the type of the keys. *)
val map : ('key, 'a, 'cmp) t -> f:('a -> 'b) -> ('key, 'b, 'cmp) t

(** Same as {!map}, but the function receives as arguments both the key and
    the associated value for each binding of the map. [key] and [data] are
    labeled to avoid mixing them up. *)
val mapi : ('key, 'a, 'cmp) t -> f:(key:'key -> data:'a -> 'b) -> ('key, 'b, 'cmp) t

(** [filter m ~f] returns the map with all the bindings in [m] that satisfy
    predicate [p]. If every binding in [m] satisfies [f], [m] is returned
    unchanged (the result of the function is then physically equal to [m]).
    [key] and [data] are labeled to avoid mixing them up. *)
val filter : ('key, 'a, 'cmp) t -> f:(key:'key -> data:'a -> bool) -> ('key, 'a, 'cmp) t

(** [filter_map m ~f] applies the function [f] to every binding of [m], and
    builds a map from the results. For each binding [(k, v)] in the input map:
    - if [f ~key:k ~data:v] is [None] then [k] is not in the result,
    - if [f ~key:k ~data:v] is [Some v'] then the binding [(k, v')] is in the
      output map.

    For example, the following function on maps whose values are lists
    {[
    filter_map m ~f:(fun ~key:_ ~data:li ->
      match li with
      | [] -> None
      | _ :: tl -> Some tl)
    ]}
    drops all bindings of [m] whose value is an empty list, and pops the first
    element of each value that is non-empty. [key] and [data] are labeled to
    avoid mixing them up. *)
val filter_map
  :  ('key, 'a, 'cmp) t
  -> f:(key:'key -> data:'a -> 'b option)
  -> ('key, 'b, 'cmp) t

(** [partition m ~f] returns a pair of maps [(m1, m2)], where [m1] contains all
    the bindings of [m] that satisfy the predicate [f], and [m2] is the map with
    all the bindings of [m] that do not satisfy [f]. [key] and [data] are
    labeled to avoid mixing them up. *)
val partition
  :  ('key, 'a, 'cmp) t
  -> f:(key:'key -> data:'a -> bool)
  -> ('key, 'a, 'cmp) t * ('key, 'a, 'cmp) t

(** [split m key] returns a triple [(l, data, r)], where [l] is the map with all
    the bindings of [m] whose key is strictly less than [key]; [r] is the map
    with all the bindings of [m] whose key is strictly greater than [key];
    [data] is [None] if [m] contains no binding for [key], or [Some v] if [m]
    binds [v] to [key]. *)
val split
  :  ('key, 'a, 'cmp) t
  -> 'key
  -> ('key, 'a, 'cmp) t * 'a option * ('key, 'a, 'cmp) t

(** {1:predicates Predicates and comparisons} *)

(** Test whether a map is empty or not. *)
val is_empty : (_, _, _) t -> bool

(** Test whether a map has exactly one element or not. *)
val is_singleton : (_, _, _) t -> bool

(** [mem m key] returns [true] if [m] contains a binding for [key], and [false]
    otherwise. *)
val mem : ('key, _, _) t -> 'key -> bool

(** [equal cmp m1 m2] tests whether the maps [m1] and [m2] are equal, that is,
    contain equal keys and associate them with equal data. [cmp] is the
    equality predicate used to compare the data associated with the keys.
    Raise [Invalid_argument] if the maps have different compare functions. *)
val equal : ('a -> 'a -> bool) -> ('key, 'a, 'cmp) t -> ('key, 'a, 'cmp) t -> bool

(** [compare cmp m1 m2] is a total ordering between maps. [cmp] is a total
    ordering used to compare data associated with equal keys in the two
    maps. Raise [Invalid_argument] if the maps have different compare
    functions. *)
val compare
  :  ('a -> 'a -> Ordering.t)
  -> ('key, 'a, 'cmp) t
  -> ('key, 'a, 'cmp) t
  -> Ordering.t

(** [for_all m ~f] checks if all the bindings of the map satisfy the predicate
    [f]. [key] and [data] are labeled to avoid mixing them up. *)
val for_all : ('key, 'a, _) t -> f:(key:'key -> data:'a -> bool) -> bool

(** [exists m ~f] checks if at least one binding of the map satisfies the
    predicate [f]. [key] and [data] are labeled to avoid mixing them up. *)
val exists : ('key, 'a, _) t -> f:(key:'key -> data:'a -> bool) -> bool

(** {1:converting Converting} *)

(** [to_list m] is {!val:bindings}[ m]. *)
val to_list : ('key, 'a, _) t -> ('key * 'a) list

(** [of_list (module Ord) bs] adds the bindings of [bs] to the empty map, in
    list order (if a key is bound twice in [bs] the last one takes over). *)
val of_list
  :  (module OrderedType with type t = 'key and type comparator_witness = 'cmp)
  -> ('key * 'a) list
  -> ('key, 'a, 'cmp) t

(** Iterate on the whole map, in ascending order of keys. *)
val to_seq : ('key, 'a, _) t -> ('key * 'a) Seq.t

(** Iterate on the whole map, in descending order of keys. *)
val to_rev_seq : ('key, 'a, _) t -> ('key * 'a) Seq.t

(** [to_seq_from m key] iterates on a subset of the bindings of [m], in
    ascending order of keys, from [key] or above. *)
val to_seq_from : ('key, 'a, _) t -> 'key -> ('key * 'a) Seq.t

(** Add the given bindings to the map, in order. *)
val add_seq : ('key, 'a, 'cmp) t -> ('key * 'a) Seq.t -> ('key, 'a, 'cmp) t

(** Build a map from the given bindings. *)
val of_seq
  :  (module OrderedType with type t = 'key and type comparator_witness = 'cmp)
  -> ('key * 'a) Seq.t
  -> ('key, 'a, 'cmp) t

(** {1:modular Modular explicit usage}

    The declarations below offer an alternative, module-based style for fixing
    the key type (and its comparator witness) at a single first-class module,
    in the tradition of Base's [Map.M(Key).t]. They also provide ways to
    derive [sexp_of_t] and [to_dyn] for a map, given a first-class module for
    the keys and a converter for the data. *)

module M (T : sig
    type t
    type comparator_witness
  end) : sig
  type nonrec 'a t = (T.t, 'a, T.comparator_witness) t
end

(** Input signature for {!val:sexp_of_m__t}. *)
module type Sexpable = sig
  type t

  val sexp_of_t : t -> Sexplib0.Sexp.t
end

(** [sexp_of_m__t (module Key) sexp_of_data m] converts [m] to an s-expression,
    representing each binding as a two-element list of the key and data
    s-expressions, in the order given by {!val:bindings}. *)
val sexp_of_m__t
  :  (module Sexpable with type t = 'key)
  -> ('a -> Sexplib0.Sexp.t)
  -> ('key, 'a, _) t
  -> Sexplib0.Sexp.t

(** Input signature for {!val:dyn_of_m__t}. *)
module type Dynable = sig
  type t

  val to_dyn : t -> Dyn.t
end

(** [dyn_of_m__t (module Key) data_to_dyn m] converts [m] to a [Dyn.t], built
    the same way as {!val:sexp_of_m__t}. *)
val dyn_of_m__t
  :  (module Dynable with type t = 'key)
  -> ('a -> Dyn.t)
  -> ('key, 'a, _) t
  -> Dyn.t
