(***********************************************************************************)
(*  nofunc-keyed: Keyed data structures adapted from OCaml Stdlib but no functors  *)
(*  SPDX-FileCopyrightText: 2025-2026 Mathieu Barbin <mathieu.barbin@gmail.com>    *)
(*  SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception   *)
(***********************************************************************************)

module Hashtbl = Nofunc_htbl.Hashtbl

type !'a t = ('a, unit) Hashtbl.t

let clear = Hashtbl.clear
let reset = Hashtbl.reset
let copy = Hashtbl.copy
let add t key = Hashtbl.set t ~key ~data:()
let mem = Hashtbl.mem
let remove = Hashtbl.remove
let iter t ~f = Hashtbl.iter t ~f:(fun ~key ~data:() -> f key)
let some_unit = Some ()

let filter_inplace t ~f =
  Hashtbl.filter_map_inplace t ~f:(fun ~key ~data:() -> if f key then some_unit else None)
;;

let fold t ~init ~f = Hashtbl.fold t ~init ~f:(fun ~key:elt ~data:() acc -> f ~elt acc)
let length = Hashtbl.length
let stats = Hashtbl.stats
let to_seq = Hashtbl.to_seq_keys
let add_seq t seq = Seq.iter (fun key -> add t key) seq
let create_seeded = Hashtbl.create_seeded
let create = Hashtbl.create

let of_seq_seeded mkey ?random seq =
  let t = create_seeded mkey ?random 16 in
  Seq.iter (fun key -> add t key) seq;
  t
;;

let of_seq mkey seq =
  let t = create mkey 16 in
  Seq.iter (fun key -> add t key) seq;
  t
;;

module M (T : sig
    type t
  end) =
struct
  type nonrec t = T.t t
end

module type Sexpable = sig
  type t

  val compare : t -> t -> Ordering.t
  val sexp_of_t : t -> Sexplib0.Sexp.t
end

let sexp_of_m__t (type elt) (module T : Sexpable with type t = elt) t =
  Sexplib0.Sexp.List
    (to_seq t
     |> List.of_seq
     |> List.sort (fun elt1 elt2 -> Ordering.to_int (T.compare elt1 elt2))
     |> List.map T.sexp_of_t)
;;

module type Dynable = sig
  type t

  val compare : t -> t -> Ordering.t
  val to_dyn : t -> Dyn.t
end

let dyn_of_m__t (type elt) (module T : Dynable with type t = elt) t =
  Dyn.Set
    (to_seq t
     |> List.of_seq
     |> List.sort (fun elt1 elt2 -> Ordering.to_int (T.compare elt1 elt2))
     |> List.map T.to_dyn)
;;
