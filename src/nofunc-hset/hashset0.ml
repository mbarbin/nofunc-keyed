(***********************************************************************************)
(*  nofunc-keyed: Keyed data structures adapted from OCaml Stdlib but no functors  *)
(*  SPDX-FileCopyrightText: 2025 Mathieu Barbin <mathieu.barbin@gmail.com>         *)
(*  SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception   *)
(***********************************************************************************)

module Hashtbl = Nofunc_stdhtbl.Hashtbl

type !'a t = ('a, unit) Hashtbl.t

let clear = Hashtbl.clear
let reset = Hashtbl.reset
let copy = Hashtbl.copy
let add t key = Hashtbl.replace t key ()
let mem = Hashtbl.mem
let remove = Hashtbl.remove
let iter f t = Hashtbl.iter (fun x () -> f x) t
let some_unit = Some ()

let filter_inplace f t =
  Hashtbl.filter_map_inplace (fun x () -> if f x then some_unit else None) t
;;

let fold f t init = Hashtbl.fold (fun a () acc -> f a acc) t init
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
