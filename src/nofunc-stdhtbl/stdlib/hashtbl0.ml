(***********************************************************************************)
(*  nofunc-keyed: Keyed data structures adapted from OCaml Stdlib but no functors  *)
(*  SPDX-FileCopyrightText: 2025 Mathieu Barbin <mathieu.barbin@gmail.com>         *)
(*  SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception   *)
(***********************************************************************************)

(*_ Notice: This file was copied from OCaml Stdlib:

  path: "stdlib/map.ml" ; rev: f8ea2c42144f416f4d7a5d71a0bb2c766ca8fedc

  The original license header was kept with the file, see below.

  List of changes:

  - format with ocamlformat

  - disable warning 9 via directive *)

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

[@@@warning "-9"]

(* Hash tables *)

(* To avoid using [Stdlib.equal] by accident, we shadow the function [equal] in
   this file. The variable [equal] may only be used when brought to scope as a
   function argument. *)
let equal = `shadow_stdlib_equal
let _ = equal

type 'key equal = 'key -> 'key -> bool
type 'key hash = 'key -> int
type 'key seeded_hash = int -> 'key -> int

(* We do dynamic hashing, and resize the table and rehash the elements when the
   load factor becomes too high. *)

type ('a, 'b) t = {
  mutable size : int; (* number of entries *)
  mutable data : ('a, 'b) bucketlist array; (* the buckets *)
  seed : int; (* for randomization *)
  mutable initial_size : int; (* initial array size *)
}

and ('a, 'b) bucketlist =
  | Empty
  | Cons of {
      mutable key : 'a;
      mutable data : 'b;
      mutable next : ('a, 'b) bucketlist;
    }

(* The sign of initial_size encodes the fact that a traversal is ongoing or not.

   This disables the efficient in place implementation of resizing. *)

let ongoing_traversal h =
  Obj.size (Obj.repr h) < 4 (* compatibility with old hash tables *)
  || h.initial_size < 0

let flip_ongoing_traversal h = h.initial_size <- -h.initial_size

(* To pick random seeds if requested *)

let randomized_default =
  let params =
    try Sys.getenv "OCAMLRUNPARAM"
    with Not_found -> ( try Sys.getenv "CAMLRUNPARAM" with Not_found -> "")
  in
  String.contains params 'R'

let randomized = Atomic.make randomized_default
let randomize () = Atomic.set randomized true
let is_randomized () = Atomic.get randomized
let prng_key = Domain.DLS.new_key Random.State.make_self_init

(* Functions which appear before the functorial interface must either be
   independent of the hash function or take it as a parameter (see #2202 and
   code below the functor definitions. *)

(* Creating a fresh, empty table *)

let rec power_2_above x n =
  if x >= n then x
  else if x * 2 > Sys.max_array_length then x
  else power_2_above (x * 2) n

let create ?(random = Atomic.get randomized) initial_size =
  let s = power_2_above 16 initial_size in
  let seed =
    if random then Random.State.bits (Domain.DLS.get prng_key) else 0
  in
  { initial_size = s; size = 0; seed; data = Array.make s Empty }

let clear h =
  if h.size > 0 then begin
    h.size <- 0;
    Array.fill h.data 0 (Array.length h.data) Empty
  end

let reset h =
  let len = Array.length h.data in
  if
    Obj.size (Obj.repr h) < 4 (* compatibility with old hash tables *)
    || len = abs h.initial_size
  then clear h
  else begin
    h.size <- 0;
    h.data <- Array.make (abs h.initial_size) Empty
  end

let copy_bucketlist = function
  | Empty -> Empty
  | Cons { key; data; next } ->
      let rec loop prec = function
        | Empty -> ()
        | Cons { key; data; next } ->
            let r = Cons { key; data; next } in
            begin match prec with
            | Empty -> assert false
            | Cons prec -> prec.next <- r
            end;
            loop r next
      in
      let r = Cons { key; data; next } in
      loop r next;
      r

let copy h = { h with data = Array.map copy_bucketlist h.data }
let length h = h.size

let insert_all_buckets indexfun inplace odata ndata =
  let nsize = Array.length ndata in
  let ndata_tail = Array.make nsize Empty in
  let rec insert_bucket = function
    | Empty -> ()
    | Cons { key; data; next } as cell ->
        let cell = if inplace then cell else Cons { key; data; next = Empty } in
        let nidx = indexfun key in
        begin match ndata_tail.(nidx) with
        | Empty -> ndata.(nidx) <- cell
        | Cons tail -> tail.next <- cell
        end;
        ndata_tail.(nidx) <- cell;
        insert_bucket next
  in
  for i = 0 to Array.length odata - 1 do
    insert_bucket odata.(i)
  done;
  if inplace then
    for i = 0 to nsize - 1 do
      match ndata_tail.(i) with Empty -> () | Cons tail -> tail.next <- Empty
    done

let resize indexfun h =
  let odata = h.data in
  let osize = Array.length odata in
  let nsize = osize * 2 in
  if nsize < Sys.max_array_length then begin
    let ndata = Array.make nsize Empty in
    let inplace = not (ongoing_traversal h) in
    h.data <- ndata;
    (* so that indexfun sees the new bucket count *)
    insert_all_buckets (indexfun h) inplace odata ndata
  end

let iter f h =
  let rec do_bucket = function
    | Empty -> ()
    | Cons { key; data; next } ->
        f key data;
        do_bucket next
  in
  let old_trav = ongoing_traversal h in
  if not old_trav then flip_ongoing_traversal h;
  try
    let d = h.data in
    for i = 0 to Array.length d - 1 do
      do_bucket d.(i)
    done;
    if not old_trav then flip_ongoing_traversal h
  with exn when not old_trav ->
    flip_ongoing_traversal h;
    raise exn

let rec filter_map_inplace_bucket f h i prec = function
  | Empty -> begin
      match prec with Empty -> h.data.(i) <- Empty | Cons c -> c.next <- Empty
    end
  | Cons ({ key; data; next } as c) as slot -> begin
      match f key data with
      | None ->
          h.size <- h.size - 1;
          filter_map_inplace_bucket f h i prec next
      | Some data ->
          begin match prec with
          | Empty -> h.data.(i) <- slot
          | Cons c -> c.next <- slot
          end;
          c.data <- data;
          filter_map_inplace_bucket f h i slot next
    end

let filter_map_inplace f h =
  let d = h.data in
  let old_trav = ongoing_traversal h in
  if not old_trav then flip_ongoing_traversal h;
  try
    for i = 0 to Array.length d - 1 do
      filter_map_inplace_bucket f h i Empty h.data.(i)
    done;
    if not old_trav then flip_ongoing_traversal h
  with exn when not old_trav ->
    flip_ongoing_traversal h;
    raise exn

let fold f h init =
  let rec do_bucket b accu =
    match b with
    | Empty -> accu
    | Cons { key; data; next } -> do_bucket next (f key data accu)
  in
  let old_trav = ongoing_traversal h in
  if not old_trav then flip_ongoing_traversal h;
  try
    let d = h.data in
    let accu = ref init in
    for i = 0 to Array.length d - 1 do
      accu := do_bucket d.(i) !accu
    done;
    if not old_trav then flip_ongoing_traversal h;
    !accu
  with exn when not old_trav ->
    flip_ongoing_traversal h;
    raise exn

type statistics = {
  num_bindings : int;
  num_buckets : int;
  max_bucket_length : int;
  bucket_histogram : int array;
}

let rec bucket_length accu = function
  | Empty -> accu
  | Cons { next } -> bucket_length (accu + 1) next

let stats h =
  let mbl =
    Array.fold_left (fun m b -> Int.max m (bucket_length 0 b)) 0 h.data
  in
  let histo = Array.make (mbl + 1) 0 in
  Array.iter
    (fun b ->
      let l = bucket_length 0 b in
      histo.(l) <- histo.(l) + 1)
    h.data;
  {
    num_bindings = h.size;
    num_buckets = Array.length h.data;
    max_bucket_length = mbl;
    bucket_histogram = histo;
  }

(* {1 Iterators} *)

let to_seq tbl =
  (* capture current array, so that even if the table is resized we keep
     iterating on the same array *)
  let tbl_data = tbl.data in
  (* state: index * next bucket to traverse *)
  let rec aux i buck () =
    match buck with
    | Empty ->
        if i = Array.length tbl_data then Seq.Nil
        else aux (i + 1) tbl_data.(i) ()
    | Cons { key; data; next } -> Seq.Cons ((key, data), aux i next)
  in
  aux 0 Empty

let to_seq_keys m = Seq.map fst (to_seq m)
let to_seq_values m = Seq.map snd (to_seq m)

let key_index ~seeded_hash h key =
  seeded_hash h.seed key land (Array.length h.data - 1)

let add ~seeded_hash h key data =
  let i = key_index ~seeded_hash h key in
  let bucket = Cons { key; data; next = h.data.(i) } in
  h.data.(i) <- bucket;
  h.size <- h.size + 1;
  if h.size > Array.length h.data lsl 1 then resize (key_index ~seeded_hash) h

let rec remove_bucket ~equal h i key prec bucket =
  match bucket with
  | Empty -> bucket
  | Cons { key = k; next; _ } ->
      if equal k key then begin
        h.size <- h.size - 1;
        begin match prec with
        | Empty -> h.data.(i) <- next
        | Cons c -> c.next <- next
        end;
        bucket
      end
      else remove_bucket ~equal h i key bucket next

let find_and_remove ~equal ~seeded_hash h key =
  let i = key_index ~seeded_hash h key in
  let bucket = remove_bucket ~equal h i key Empty h.data.(i) in
  match bucket with Empty -> None | Cons { data; _ } -> Some data

let remove ~equal ~seeded_hash h key =
  let i = key_index ~seeded_hash h key in
  ignore (remove_bucket ~equal h i key Empty h.data.(i))

let rec find_rec ~equal key = function
  | Empty -> raise Not_found
  | Cons { key = k; data; next } ->
      if equal key k then data else find_rec ~equal key next

let find ~equal ~seeded_hash h key =
  match h.data.(key_index ~seeded_hash h key) with
  | Empty -> raise Not_found
  | Cons { key = k1; data = d1; next = next1 } -> (
      if equal key k1 then d1
      else
        match next1 with
        | Empty -> raise Not_found
        | Cons { key = k2; data = d2; next = next2 } -> (
            if equal key k2 then d2
            else
              match next2 with
              | Empty -> raise Not_found
              | Cons { key = k3; data = d3; next = next3 } ->
                  if equal key k3 then d3 else find_rec ~equal key next3))

let rec find_rec_opt ~equal key = function
  | Empty -> None
  | Cons { key = k; data; next } ->
      if equal key k then Some data else find_rec_opt ~equal key next

let find_opt ~equal ~seeded_hash h key =
  match h.data.(key_index ~seeded_hash h key) with
  | Empty -> None
  | Cons { key = k1; data = d1; next = next1 } -> (
      if equal key k1 then Some d1
      else
        match next1 with
        | Empty -> None
        | Cons { key = k2; data = d2; next = next2 } -> (
            if equal key k2 then Some d2
            else
              match next2 with
              | Empty -> None
              | Cons { key = k3; data = d3; next = next3 } ->
                  if equal key k3 then Some d3
                  else find_rec_opt ~equal key next3))

let find_all ~equal ~seeded_hash h key =
  let[@tail_mod_cons] rec find_in_bucket = function
    | Empty -> []
    | Cons { key = k; data = d; next } ->
        if equal k key then d :: find_in_bucket next else find_in_bucket next
  in
  find_in_bucket h.data.(key_index ~seeded_hash h key)

let rec retrieve_bucket ~equal key bucket =
  match bucket with
  | Empty -> bucket
  | Cons { key = k; next } ->
      if equal k key then bucket else retrieve_bucket ~equal key next

let replace_bucket ~seeded_hash h key i l data = function
  | Empty ->
      h.data.(i) <- Cons { key; data; next = l };
      h.size <- h.size + 1;
      if h.size > Array.length h.data lsl 1 then
        resize (key_index ~seeded_hash) h
  | Cons slot ->
      slot.key <- key;
      slot.data <- data

let find_and_replace ~equal ~seeded_hash h key data =
  let i = key_index ~seeded_hash h key in
  let l = h.data.(i) in
  let bucket = retrieve_bucket ~equal key l in
  let old_data =
    match bucket with Cons { data; _ } -> Some data | Empty -> None
  in
  replace_bucket ~seeded_hash h key i l data bucket;
  old_data

let replace ~equal ~seeded_hash h key data =
  let i = key_index ~seeded_hash h key in
  let l = h.data.(i) in
  let bucket = retrieve_bucket ~equal key l in
  replace_bucket ~seeded_hash h key i l data bucket

(* Iterators *)

let rec mem_in_bucket ~equal key = function
  | Empty -> false
  | Cons { key = k; next } -> equal k key || mem_in_bucket ~equal key next

let mem ~equal ~seeded_hash h key =
  mem_in_bucket ~equal key h.data.(key_index ~seeded_hash h key)

let add_seq ~seeded_hash tbl i =
  Seq.iter (fun (k, v) -> add ~seeded_hash tbl k v) i

let replace_seq ~equal ~seeded_hash tbl i =
  Seq.iter (fun (k, v) -> replace ~equal ~seeded_hash tbl k v) i

let of_seq ~equal ~seeded_hash i =
  let tbl = create 16 in
  replace_seq ~equal ~seeded_hash tbl i;
  tbl
