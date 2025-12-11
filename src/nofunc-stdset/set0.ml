(***********************************************************************************)
(*  nofunc-keyed: Keyed data structures adapted from OCaml Stdlib but no functors  *)
(*  SPDX-FileCopyrightText: 2025 Mathieu Barbin <mathieu.barbin@gmail.com>         *)
(*  SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception   *)
(***********************************************************************************)

module type OrderedType = sig
  type t

  val compare : t -> t -> int
end

module Tree = Nofunc_stdset_stdlib.Set0

type 'elt t =
  { compare : 'elt Tree.compare
  ; tree : 'elt Tree.t
  }

let empty (type elt) (module Ord : OrderedType with type t = elt) =
  { compare = Ord.compare; tree = Tree.empty }
;;

let with_tree t tree = if t.tree == tree then t else { compare = t.compare; tree }
let add elt t = with_tree t (Tree.add ~compare:t.compare elt t.tree)

let singleton (type elt) (module Ord : OrderedType with type t = elt) elt =
  { compare = Ord.compare; tree = Tree.singleton elt }
;;

let remove elt t = with_tree t (Tree.remove ~compare:t.compare elt t.tree)

let check_same_compare t1 t2 ~fct =
  if not (t1.compare == t2.compare)
  then invalid_arg (Printf.sprintf "Set.%s: sets have different compare functions." fct)
;;

let union t1 t2 =
  check_same_compare t1 t2 ~fct:"union";
  let res = Tree.union ~compare:t1.compare t1.tree t2.tree in
  if t1.tree == res
  then t1
  else if t2.tree == res
  then t2
  else { compare = t1.compare; tree = res }
;;

let inter t1 t2 =
  check_same_compare t1 t2 ~fct:"inter";
  { compare = t1.compare; tree = Tree.inter ~compare:t1.compare t1.tree t2.tree }
;;

let disjoint t1 t2 =
  check_same_compare t1 t2 ~fct:"disjoint";
  Tree.disjoint ~compare:t1.compare t1.tree t2.tree
;;

let diff t1 t2 =
  check_same_compare t1 t2 ~fct:"diff";
  with_tree t1 (Tree.diff ~compare:t1.compare t1.tree t2.tree)
;;

let cardinal t = Tree.cardinal t.tree
let elements t = Tree.elements t.tree
let min_elt t = Tree.min_elt t.tree
let min_elt_opt t = Tree.min_elt_opt t.tree
let max_elt t = Tree.max_elt t.tree
let max_elt_opt t = Tree.max_elt_opt t.tree
let choose t = Tree.choose t.tree
let choose_opt t = Tree.choose_opt t.tree
let find elt t = Tree.find ~compare:t.compare elt t.tree
let find_opt elt t = Tree.find_opt ~compare:t.compare elt t.tree
let find_first f t = Tree.find_first f t.tree
let find_first_opt f t = Tree.find_first_opt f t.tree
let find_last f t = Tree.find_last f t.tree
let find_last_opt f t = Tree.find_last_opt f t.tree
let iter f t = Tree.iter f t.tree
let fold f t init = Tree.fold f t.tree init
let map f t = with_tree t (Tree.map ~compare:t.compare f t.tree)
let filter f t = with_tree t (Tree.filter f t.tree)
let filter_map f t = with_tree t (Tree.filter_map ~compare:t.compare f t.tree)

let partition f t =
  let t_true, t_false = Tree.partition f t.tree in
  with_tree t t_true, with_tree t t_false
;;

let split elt t =
  let left, present, right = Tree.split ~compare:t.compare elt t.tree in
  with_tree t left, present, with_tree t right
;;

let is_empty t = Tree.is_empty t.tree
let is_singleton t = Tree.is_singleton t.tree
let mem elt t = Tree.mem ~compare:t.compare elt t.tree

let equal t1 t2 =
  check_same_compare t1 t2 ~fct:"equal";
  Tree.equal ~compare:t1.compare t1.tree t2.tree
;;

let compare t1 t2 =
  check_same_compare t1 t2 ~fct:"compare";
  Tree.compare ~compare:t1.compare t1.tree t2.tree
;;

let subset t1 t2 =
  check_same_compare t1 t2 ~fct:"subset";
  Tree.subset ~compare:t1.compare t1.tree t2.tree
;;

let for_all f t = Tree.for_all f t.tree
let exists f t = Tree.exists f t.tree
let to_list t = Tree.to_list t.tree

let of_list (type elt) (module Ord : OrderedType with type t = elt) l =
  { compare = Ord.compare; tree = Tree.of_list ~compare:Ord.compare l }
;;

let to_seq_from elt t = Tree.to_seq_from ~compare:t.compare elt t.tree
let to_seq t = Tree.to_seq t.tree
let to_rev_seq t = Tree.to_rev_seq t.tree
let add_seq seq t = with_tree t (Tree.add_seq ~compare:t.compare seq t.tree)

let of_seq (type elt) (module Ord : OrderedType with type t = elt) seq =
  { compare = Ord.compare; tree = Tree.of_seq ~compare:Ord.compare seq }
;;
