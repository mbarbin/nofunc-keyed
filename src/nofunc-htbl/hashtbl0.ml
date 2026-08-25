(***********************************************************************************)
(*  nofunc-keyed: Keyed data structures adapted from OCaml Stdlib but no functors  *)
(*  SPDX-FileCopyrightText: 2025-2026 Mathieu Barbin <mathieu.barbin@gmail.com>    *)
(*  SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception   *)
(***********************************************************************************)

module Tbl = Nofunc_htbl_stdlib.Hashtbl0

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
let shadow t ~key ~data = Tbl.add ~seeded_hash:t.seeded_hash t.tbl key data
let find t key = Tbl.find_opt ~equal:t.equal ~seeded_hash:t.seeded_hash t.tbl key
let find_exn t key = Tbl.find ~equal:t.equal ~seeded_hash:t.seeded_hash t.tbl key
let find_all t key = Tbl.find_all ~equal:t.equal ~seeded_hash:t.seeded_hash t.tbl key
let mem t key = Tbl.mem ~equal:t.equal ~seeded_hash:t.seeded_hash t.tbl key
let remove t key = Tbl.remove ~equal:t.equal ~seeded_hash:t.seeded_hash t.tbl key

let find_and_remove t key =
  Tbl.find_and_remove ~equal:t.equal ~seeded_hash:t.seeded_hash t.tbl key
;;

let remove_all t key = Tbl.remove_all ~equal:t.equal ~seeded_hash:t.seeded_hash t.tbl key

let set t ~key ~data =
  Tbl.replace ~equal:t.equal ~seeded_hash:t.seeded_hash t.tbl key data
;;

let find_and_replace t ~key ~data =
  Tbl.find_and_replace ~equal:t.equal ~seeded_hash:t.seeded_hash t.tbl key data
;;

let iter t ~f = Tbl.iter (fun key data -> f ~key ~data) t.tbl
let filter_map_inplace t ~f = Tbl.filter_map_inplace (fun key data -> f ~key ~data) t.tbl
let fold t ~init ~f = Tbl.fold (fun key data acc -> f ~key ~data acc) t.tbl init
let length t = Tbl.length t.tbl
let stats t = Tbl.stats t.tbl
let to_seq t = Tbl.to_seq t.tbl
let to_seq_keys t = Tbl.to_seq_keys t.tbl
let to_seq_values t = Tbl.to_seq_values t.tbl
let shadow_seq t seq = Tbl.add_seq ~seeded_hash:t.seeded_hash t.tbl seq
let set_seq t seq = Tbl.replace_seq ~equal:t.equal ~seeded_hash:t.seeded_hash t.tbl seq

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

module M (T : sig
    type t
  end) =
struct
  type nonrec 'a t = (T.t, 'a) t
end

module type Sexpable = sig
  type t

  val compare : t -> t -> Ordering.t
  val sexp_of_t : t -> Sexplib0.Sexp.t
end

let sexp_of_m__t (type key) (module T : Sexpable with type t = key) sexp_of_data t =
  Sexplib0.Sexp.List
    (to_seq t
     |> List.of_seq
     |> List.sort (fun (key1, _) (key2, _) -> Ordering.to_int (T.compare key1 key2))
     |> List.map (fun (key, data) ->
       Sexplib0.Sexp.List [ T.sexp_of_t key; sexp_of_data data ]))
;;

module type Dynable = sig
  type t

  val compare : t -> t -> Ordering.t
  val to_dyn : t -> Dyn.t
end

let dyn_of_m__t (type key) (module T : Dynable with type t = key) data_to_dyn t =
  Dyn.Map
    (to_seq t
     |> List.of_seq
     |> List.sort (fun (key1, _) (key2, _) -> Ordering.to_int (T.compare key1 key2))
     |> List.map (fun (key, data) -> T.to_dyn key, data_to_dyn data))
;;

let replace = set
let replace_seq = set_seq
let add = shadow
let add_seq = shadow_seq
