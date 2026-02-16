(*_**********************************************************************************)
(*_  nofunc-keyed: Keyed data structures adapted from OCaml Stdlib but no functors  *)
(*_  SPDX-FileCopyrightText: 2025 Mathieu Barbin <mathieu.barbin@gmail.com>         *)
(*_  SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception   *)
(*_**********************************************************************************)

(*_ Notice: This file was copied from OCaml Stdlib:

  path: "stdlib/hashtbl.mli" ; rev: f8ea2c42144f416f4d7a5d71a0bb2c766ca8fedc

  The original license header was kept with the file, see below.

  List of changes:

  - format file with ocamlformat

  - Remove the functor and signature. Make the type parametrized by the type of
    keys and data. Require [(module HashedType)] (resp. SeededType) as first-class
    module argument everywhere needed. *)

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

(** Hash tables.

    Hash tables are hashed association tables, with in-place modification.
    Because most operations on a hash table modify their input, they're more
    commonly used in imperative code. The lookup of the value associated with a
    key (see {!find}, {!find_opt}) is normally very fast, often faster than the
    equivalent lookup in [Map].

    {b Warning} a hash table is only as good as the hash function. A bad hash
    function will turn the table into a degenerate association list, with linear
    time lookup instead of constant time lookup. *)

(** {b Unsynchronized accesses} *)

[@@@warning "-53"]

[@@@alert
  unsynchronized_access "Unsynchronized accesses to hash tables are a programming error."]

[@@@warning "+53"]

(** Unsynchronized accesses to a hash table may lead to an invalid hash table
    state. Thus, concurrent accesses to a hash tables must be synchronized (for
    instance with a [Mutex.t]). *)

module type HashedType = Stdlib.Hashtbl.HashedType
module type SeededHashedType = Stdlib.Hashtbl.SeededHashedType

(** The type of hash tables from type ['a] to type ['b]. *)
type (!'a, !'b) t

(** Empty a hash table. Use [reset] instead of [clear] to shrink the
    size of the bucket table to its initial size. *)
val clear : ('a, 'b) t -> unit

(** Empty a hash table and shrink the size of the bucket table
    to its initial size. *)
val reset : ('a, 'b) t -> unit

(** Return a copy of the given hashtable. *)
val copy : ('a, 'b) t -> ('a, 'b) t

(** [add tbl key data] adds a binding of [key] to [data]
    in table [tbl].

    {b Warning}: Previous bindings for [key] are not removed, but simply
    hidden. That is, after performing {!remove}[ tbl key],
    the previous binding for [key], if any, is restored.
    (Same behavior as with association lists.)

    If you desire the classic behavior of replacing elements,
    see {!replace}. *)
val add : ('a, 'b) t -> 'a -> 'b -> unit

(** [find tbl x] returns the current binding of [x] in [tbl], or raises
    [Not_found] if no such binding exists. *)
val find : ('a, 'b) t -> 'a -> 'b

(** [find_opt tbl x] returns the current binding of [x] in [tbl], or [None] if
    no such binding exists. *)
val find_opt : ('a, 'b) t -> 'a -> 'b option

(** [find_all tbl x] returns the list of all data associated with [x] in [tbl].
    The current binding is returned first, then the previous bindings, in
    reverse order of introduction in the table. *)
val find_all : ('a, 'b) t -> 'a -> 'b list

(** [mem tbl x] checks if [x] is bound in [tbl]. *)
val mem : ('a, 'b) t -> 'a -> bool

(** [remove tbl x] removes the current binding of [x] in [tbl], restoring the
    previous binding if it exists. It does nothing if [x] is not bound in
    [tbl]. *)
val remove : ('a, 'b) t -> 'a -> unit

(** Same as {!remove} but returns the previous binding, if any. *)
val find_and_remove : ('a, 'b) t -> 'a -> 'b option

(** [replace tbl key data] replaces the current binding of [key] in [tbl] by a
    binding of [key] to [data]. If [key] is unbound in [tbl], a binding of [key]
    to [data] is added to [tbl].

    This is functionally equivalent to {!remove}[ tbl key] followed by
    {!add}[ tbl key data]. *)
val replace : ('a, 'b) t -> 'a -> 'b -> unit

(** Same as {!replace} but returns the previous binding, if any. *)
val find_and_replace : ('a, 'b) t -> 'a -> 'b -> 'b option

(** [iter f tbl] applies [f] to all bindings in table [tbl]. [f] receives the
    key as first argument, and the associated value as second argument. Each
    binding is presented exactly once to [f].

    The order in which the bindings are passed to [f] is unspecified. However,
    if the table contains several bindings for the same key, they are passed to
    [f] in reverse order of introduction, that is, the most recent binding is
    passed first.

    If the hash table was created in non-randomized mode, the order in which the
    bindings are enumerated is reproducible between successive runs of the
    program, and even between minor versions of OCaml. For randomized hash
    tables, the order of enumeration is entirely random.

    The behavior is not specified if the hash table is modified by [f] during
    the iteration. *)
val iter : ('a -> 'b -> unit) -> ('a, 'b) t -> unit

(** [filter_map_inplace f tbl] applies [f] to all bindings in table [tbl] and
    update each binding depending on the result of [f]. If [f] returns [None],
    the binding is discarded. If it returns [Some new_val], the binding is
    update to associate the key to [new_val].

    Other comments for {!iter} apply as well. *)
val filter_map_inplace : ('a -> 'b -> 'b option) -> ('a, 'b) t -> unit

(** [fold f tbl init] computes [(f kN dN ... (f k1 d1 init)...)], where
    [k1 ... kN] are the keys of all bindings in [tbl], and [d1 ... dN] are the
    associated values. Each binding is presented exactly once to [f].

    The order in which the bindings are passed to [f] is unspecified. However,
    if the table contains several bindings for the same key, they are passed to
    [f] in reverse order of introduction, that is, the most recent binding is
    passed first.

    If the hash table was created in non-randomized mode, the order in which the
    bindings are enumerated is reproducible between successive runs of the
    program, and even between minor versions of OCaml. For randomized hash
    tables, the order of enumeration is entirely random.

    The behavior is not specified if the hash table is modified by [f] during
    the iteration. *)
val fold : ('a -> 'b -> 'acc -> 'acc) -> ('a, 'b) t -> 'acc -> 'acc

(** [length tbl] returns the number of bindings in [tbl]. It takes constant
    time. Multiple bindings are counted once each, so [length] gives the number
    of times [iter] calls its first argument. *)
val length : ('a, 'b) t -> int

(** [stats tbl] returns statistics about the table [tbl]: number of buckets,
    size of the biggest bucket, distribution of buckets by size. *)
val stats : ('a, 'b) t -> Stdlib.Hashtbl.statistics

(** Iterate on the whole table. The order in which the bindings appear in the
    sequence is unspecified. However, if the table contains several bindings for
    the same key, they appear in reversed order of introduction, that is, the
    most recent binding appears first.

    The behavior is not specified if the hash table is modified during the
    iteration. *)
val to_seq : ('a, 'b) t -> ('a * 'b) Seq.t

(** Same as [Seq.map fst (to_seq m)]. *)
val to_seq_keys : ('a, _) t -> 'a Seq.t

(** Same as [Seq.map snd (to_seq m)]. *)
val to_seq_values : (_, 'b) t -> 'b Seq.t

(** Add the given bindings to the table, using {!add}. *)
val add_seq : ('a, 'b) t -> ('a * 'b) Seq.t -> unit

(** Add the given bindings to the table, using {!replace}. *)
val replace_seq : ('a, 'b) t -> ('a * 'b) Seq.t -> unit

(** [create_seeded (module Key) n] creates a new, empty hash table, with initial
    size greater or equal to the suggested size [n]. For best results, [n]
    should be on the order of the expected number of elements that will be in
    the table. The table grows as needed, so [n] is just an initial guess. If
    [n] is very small or negative then it is disregarded and a small default
    size is used.

    The optional [~random] parameter (a boolean) controls whether the internal
    organization of the hash table is randomized at each execution of
    [Hashtbl.create] or deterministic over all executions.

    A hash table that is created with [~random] set to [false] uses a fixed hash
    function ([hash]) to distribute keys among buckets. As a consequence,
    collisions between keys happen deterministically. In Web-facing applications
    or other security-sensitive applications, the deterministic collision
    patterns can be exploited by a malicious user to create a denial-of-service
    attack: the attacker sends input crafted to create many collisions in the
    table, slowing the application down.

    A hash table that is created with [~random] set to [true] uses the seeded
    hash function [seeded_hash] with a seed that is randomly chosen at hash
    table creation time. In effect, the hash function used is randomly selected
    among [2^{30}] different hash functions. All these hash functions have
    different collision patterns, rendering ineffective the denial-of-service
    attack described above. However, because of randomization, enumerating all
    elements of the hash table using {!fold} or {!iter} is no longer
    deterministic: elements are enumerated in different orders at different runs
    of the program.

    If no [~random] parameter is given, hash tables are created in non-random
    mode by default. This default can be changed either programmatically by
    calling [Stdlib.Hashtbl.randomize] or by setting the [R] flag in the
    [OCAMLRUNPARAM] environment variable. *)
val create_seeded
  :  (module SeededHashedType with type t = 'key)
  -> ?random:bool
  -> int
  -> ('key, 'a) t

(** Build a table from the given bindings. The bindings are added in the same
    order they appear in the sequence, using {!replace_seq}, which means that if
    two pairs have the same key, only the latest one will appear in the table. *)
val of_seq_seeded
  :  (module SeededHashedType with type t = 'a)
  -> ?random:bool
  -> ('a * 'b) Seq.t
  -> ('a, 'b) t

(** When using the [HashedType] interface, hashtbl cannot be created with
    [~random] set to [true], thus this parameter does not appear in the
    interface. *)
val create : (module HashedType with type t = 'key) -> int -> ('key, 'a) t

(** Same as {!of_seq_seeded} but never randomized. *)
val of_seq : (module HashedType with type t = 'a) -> ('a * 'b) Seq.t -> ('a, 'b) t
