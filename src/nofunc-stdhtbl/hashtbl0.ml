(***********************************************************************************)
(*  nofunc-keyed: Keyed data structures adapted from OCaml Stdlib but no functors  *)
(*  SPDX-FileCopyrightText: 2025 Mathieu Barbin <mathieu.barbin@gmail.com>         *)
(*  SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception   *)
(***********************************************************************************)

module Tbl = Nofunc_stdhtbl_stdlib.Hashtbl0

module type HashedType = Stdlib.Hashtbl.HashedType
module type SeededHashedType = Stdlib.Hashtbl.SeededHashedType

type (!'key, !'data) t =
  { equal : 'key -> 'key -> bool
  ; seeded_hash : int -> 'key -> int
  ; tbl : ('key, 'data) Tbl.t
  }

let clear t = Tbl.clear t.tbl
let reset t = Tbl.reset t.tbl
let copy t = { equal = t.equal; seeded_hash = t.seeded_hash; tbl = Tbl.copy t.tbl }
let add t key data = Tbl.add ~seeded_hash:t.seeded_hash t.tbl key data
let find t key = Tbl.find ~equal:t.equal ~seeded_hash:t.seeded_hash t.tbl key
let find_opt t key = Tbl.find_opt ~equal:t.equal ~seeded_hash:t.seeded_hash t.tbl key
let find_all t key = Tbl.find_all ~equal:t.equal ~seeded_hash:t.seeded_hash t.tbl key
let mem t key = Tbl.mem ~equal:t.equal ~seeded_hash:t.seeded_hash t.tbl key
let remove t key = Tbl.remove ~equal:t.equal ~seeded_hash:t.seeded_hash t.tbl key

let find_and_remove t key =
  Tbl.find_and_remove ~equal:t.equal ~seeded_hash:t.seeded_hash t.tbl key
;;

let replace t key data =
  Tbl.replace ~equal:t.equal ~seeded_hash:t.seeded_hash t.tbl key data
;;

let find_and_replace t key data =
  Tbl.find_and_replace ~equal:t.equal ~seeded_hash:t.seeded_hash t.tbl key data
;;

let iter f t = Tbl.iter f t.tbl
let filter_map_inplace f t = Tbl.filter_map_inplace f t.tbl
let fold f t init = Tbl.fold f t.tbl init
let length t = Tbl.length t.tbl
let stats t = Tbl.stats t.tbl
let to_seq t = Tbl.to_seq t.tbl
let to_seq_keys t = Tbl.to_seq_keys t.tbl
let to_seq_values t = Tbl.to_seq_values t.tbl
let add_seq t seq = Tbl.add_seq ~seeded_hash:t.seeded_hash t.tbl seq

let replace_seq t seq =
  Tbl.replace_seq ~equal:t.equal ~seeded_hash:t.seeded_hash t.tbl seq
;;

let create (type key) (module Key : HashedType with type t = key) len =
  let seeded_hash _ t = Key.hash t in
  { equal = Key.equal; seeded_hash; tbl = Tbl.create len }
;;

let of_seq (type key) (module Key : HashedType with type t = key) seq =
  let seeded_hash _ t = Key.hash t in
  { equal = Key.equal; seeded_hash; tbl = Tbl.of_seq ~equal:Key.equal ~seeded_hash seq }
;;

let create_seeded (type key) (module Key : SeededHashedType with type t = key) ?random len
  =
  { equal = Key.equal; seeded_hash = Key.seeded_hash; tbl = Tbl.create ?random len }
;;

let of_seq_seeded (type key) (module Key : SeededHashedType with type t = key) ?random seq
  =
  { equal = Key.equal
  ; seeded_hash = Key.seeded_hash
  ; tbl = Tbl.of_seq ~equal:Key.equal ~seeded_hash:Key.seeded_hash ?random seq
  }
;;
