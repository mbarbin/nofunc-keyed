(***********************************************************************************)
(*  nofunc-keyed: Keyed data structures adapted from OCaml Stdlib but no functors  *)
(*  SPDX-FileCopyrightText: 2025 Mathieu Barbin <mathieu.barbin@gmail.com>         *)
(*  SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception   *)
(***********************************************************************************)

module type OrderedType = sig
  type t

  val compare : t -> t -> int
end

module Tree = Nofunc_stdmap_stdlib.Map0

type ('key, !+'a) t =
  { compare : 'key Tree.compare
  ; tree : ('key, 'a) Tree.t
  }

let empty (type key) (module Ord : OrderedType with type t = key) =
  { compare = Ord.compare; tree = Tree.empty }
;;

let with_tree t tree = if t.tree == tree then t else { compare = t.compare; tree }
let add key data t = with_tree t (Tree.add ~compare:t.compare key data t.tree)

let add_to_list key data t =
  with_tree t (Tree.add_to_list ~compare:t.compare key data t.tree)
;;

let update key f t = with_tree t (Tree.update ~compare:t.compare key f t.tree)

let singleton (type key) (module Ord : OrderedType with type t = key) key data =
  { compare = Ord.compare; tree = Tree.singleton key data }
;;

let remove key t = with_tree t (Tree.remove ~compare:t.compare key t.tree)

let check_same_compare t1 t2 ~fct =
  if not (t1.compare == t2.compare)
  then invalid_arg (Printf.sprintf "Map.%s: maps have different compare functions." fct)
;;

let merge f t1 t2 =
  check_same_compare t1 t2 ~fct:"merge";
  { compare = t1.compare; tree = Tree.merge ~compare:t1.compare f t1.tree t2.tree }
;;

let union f t1 t2 =
  check_same_compare t1 t2 ~fct:"union";
  let res = Tree.union ~compare:t1.compare f t1.tree t2.tree in
  if t1.tree == res
  then t1
  else if t2.tree == res
  then t2
  else { compare = t1.compare; tree = res }
;;

let cardinal t = Tree.cardinal t.tree
let bindings t = Tree.bindings t.tree
let min_binding t = Tree.min_binding t.tree
let min_binding_opt t = Tree.min_binding_opt t.tree
let max_binding t = Tree.max_binding t.tree
let max_binding_opt t = Tree.max_binding_opt t.tree
let choose t = Tree.choose t.tree
let choose_opt t = Tree.choose_opt t.tree
let find key t = Tree.find ~compare:t.compare key t.tree
let find_opt key t = Tree.find_opt ~compare:t.compare key t.tree
let find_first f t = Tree.find_first f t.tree
let find_first_opt f t = Tree.find_first_opt f t.tree
let find_last f t = Tree.find_last f t.tree
let find_last_opt f t = Tree.find_last_opt f t.tree
let iter f t = Tree.iter f t.tree
let fold f t init = Tree.fold f t.tree init
let map f t = { compare = t.compare; tree = Tree.map f t.tree }
let mapi f t = { compare = t.compare; tree = Tree.mapi f t.tree }
let filter f t = with_tree t (Tree.filter f t.tree)
let filter_map f t = { compare = t.compare; tree = Tree.filter_map f t.tree }

let partition f t =
  let t_true, t_false = Tree.partition f t.tree in
  with_tree t t_true, with_tree t t_false
;;

let split key t =
  let left, present, right = Tree.split ~compare:t.compare key t.tree in
  with_tree t left, present, with_tree t right
;;

let is_empty t = Tree.is_empty t.tree
let is_singleton t = Tree.is_singleton t.tree
let mem key t = Tree.mem ~compare:t.compare key t.tree

let equal eq t1 t2 =
  check_same_compare t1 t2 ~fct:"equal";
  Tree.equal ~compare:t1.compare eq t1.tree t2.tree
;;

let compare cmp t1 t2 =
  check_same_compare t1 t2 ~fct:"compare";
  Tree.compare ~compare:t1.compare cmp t1.tree t2.tree
;;

let for_all f t = Tree.for_all f t.tree
let exists f t = Tree.exists f t.tree
let to_list = bindings

let of_list (type key) (module Ord : OrderedType with type t = key) bs =
  { compare = Ord.compare; tree = Tree.of_list ~compare:Ord.compare bs }
;;

let to_seq t = Tree.to_seq t.tree
let to_rev_seq t = Tree.to_rev_seq t.tree
let to_seq_from key t = Tree.to_seq_from ~compare:t.compare key t.tree
let add_seq seq t = with_tree t (Tree.add_seq ~compare:t.compare seq t.tree)

let of_seq (type key) (module Ord : OrderedType with type t = key) seq =
  { compare = Ord.compare; tree = Tree.of_seq ~compare:Ord.compare seq }
;;
