(***********************************************************************************)
(*  nofunc-keyed: Keyed data structures adapted from OCaml Stdlib but no functors  *)
(*  SPDX-FileCopyrightText: 2025 Mathieu Barbin <mathieu.barbin@gmail.com>         *)
(*  SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception   *)
(***********************************************************************************)

module Set = Nofunc_stdset.Set

let print_set s = print_dyn (Set.elements s |> Dyn.list Dyn.int)

let%expect_test "empty" =
  let e = Set.empty (module Int) in
  require (Set.is_empty e);
  [%expect {||}];
  print_dyn (Set.cardinal e |> Dyn.int);
  [%expect {| 0 |}];
  ()
;;

let%expect_test "singleton" =
  let s = Set.singleton (module Int) 42 in
  require (not (Set.is_empty s));
  [%expect {||}];
  require (Set.is_singleton s);
  [%expect {||}];
  print_dyn (Set.cardinal s |> Dyn.int);
  [%expect {| 1 |}];
  print_set s;
  [%expect {| [ 42 ] |}];
  ()
;;

let%expect_test "add" =
  let s = Set.empty (module Int) in
  let s = Set.add 3 s in
  let s = Set.add 1 s in
  let s = Set.add 2 s in
  print_set s;
  [%expect {| [ 1; 2; 3 ] |}];
  (* Adding a duplicate does not change the set, and returns physically equal. *)
  let s2 = Set.add 2 s in
  require (phys_equal s s2);
  [%expect {||}];
  ()
;;

let%expect_test "remove" =
  let s = Set.of_list (module Int) [ 1; 2; 3; 4; 5 ] in
  let s = Set.remove 3 s in
  print_set s;
  [%expect {| [ 1; 2; 4; 5 ] |}];
  (* Removing a non-existent element returns physically equal set. *)
  let s2 = Set.remove 99 s in
  require (phys_equal s s2);
  [%expect {||}];
  ()
;;

let%expect_test "of_list" =
  let s = Set.of_list (module Int) [ 5; 3; 1; 4; 2; 3; 1 ] in
  print_set s;
  [%expect {| [ 1; 2; 3; 4; 5 ] |}];
  print_dyn (Set.cardinal s |> Dyn.int);
  [%expect {| 5 |}];
  ()
;;

let%expect_test "elements / to_list" =
  let s = Set.of_list (module Int) [ 3; 1; 2 ] in
  print_dyn (Set.elements s |> Dyn.list Dyn.int);
  [%expect {| [ 1; 2; 3 ] |}];
  print_dyn (Set.to_list s |> Dyn.list Dyn.int);
  [%expect {| [ 1; 2; 3 ] |}];
  ()
;;

let%expect_test "mem" =
  let s = Set.of_list (module Int) [ 1; 2; 3 ] in
  print_dyn (Set.mem 2 s |> Dyn.bool);
  [%expect {| true |}];
  print_dyn (Set.mem 4 s |> Dyn.bool);
  [%expect {| false |}];
  ()
;;

let%expect_test "find / find_opt" =
  let s = Set.of_list (module Int) [ 1; 2; 3 ] in
  print_dyn (Set.find 2 s |> Dyn.int);
  [%expect {| 2 |}];
  require_does_raise (fun () -> Set.find 4 s);
  [%expect {| Not_found |}];
  print_dyn (Set.find_opt 2 s |> Dyn.option Dyn.int);
  [%expect {| Some 2 |}];
  print_dyn (Set.find_opt 4 s |> Dyn.option Dyn.int);
  [%expect {| None |}];
  ()
;;

let%expect_test "union" =
  let s1 = Set.of_list (module Int) [ 1; 2; 3 ] in
  let s2 = Set.of_list (module Int) [ 3; 4; 5 ] in
  print_set (Set.union s1 s2);
  [%expect {| [ 1; 2; 3; 4; 5 ] |}];
  ()
;;

let%expect_test "inter" =
  let s1 = Set.of_list (module Int) [ 1; 2; 3; 4 ] in
  let s2 = Set.of_list (module Int) [ 3; 4; 5; 6 ] in
  print_set (Set.inter s1 s2);
  [%expect {| [ 3; 4 ] |}];
  ()
;;

let%expect_test "diff" =
  let s1 = Set.of_list (module Int) [ 1; 2; 3; 4 ] in
  let s2 = Set.of_list (module Int) [ 3; 4; 5; 6 ] in
  print_set (Set.diff s1 s2);
  [%expect {| [ 1; 2 ] |}];
  ()
;;

let%expect_test "disjoint" =
  let s1 = Set.of_list (module Int) [ 1; 2 ] in
  let s2 = Set.of_list (module Int) [ 3; 4 ] in
  let s3 = Set.of_list (module Int) [ 2; 3 ] in
  print_dyn (Set.disjoint s1 s2 |> Dyn.bool);
  [%expect {| true |}];
  print_dyn (Set.disjoint s1 s3 |> Dyn.bool);
  [%expect {| false |}];
  ()
;;

let%expect_test "equal / compare" =
  let s1 = Set.of_list (module Int) [ 1; 2; 3 ] in
  let s2 = Set.of_list (module Int) [ 3; 2; 1 ] in
  let s3 = Set.of_list (module Int) [ 1; 2; 4 ] in
  print_dyn (Set.equal s1 s2 |> Dyn.bool);
  [%expect {| true |}];
  print_dyn (Set.equal s1 s3 |> Dyn.bool);
  [%expect {| false |}];
  print_dyn (Set.compare s1 s2 |> Dyn.int);
  [%expect {| 0 |}];
  require (Set.compare s1 s3 <> 0);
  [%expect {||}];
  ()
;;

let%expect_test "subset" =
  let s1 = Set.of_list (module Int) [ 1; 2 ] in
  let s2 = Set.of_list (module Int) [ 1; 2; 3 ] in
  print_dyn (Set.subset s1 s2 |> Dyn.bool);
  [%expect {| true |}];
  print_dyn (Set.subset s2 s1 |> Dyn.bool);
  [%expect {| false |}];
  print_dyn (Set.subset s1 s1 |> Dyn.bool);
  [%expect {| true |}];
  ()
;;

let%expect_test "for_all / exists" =
  let s = Set.of_list (module Int) [ 2; 4; 6 ] in
  print_dyn (Set.for_all (fun x -> x mod 2 = 0) s |> Dyn.bool);
  [%expect {| true |}];
  print_dyn (Set.for_all (fun x -> x > 3) s |> Dyn.bool);
  [%expect {| false |}];
  print_dyn (Set.exists (fun x -> x > 5) s |> Dyn.bool);
  [%expect {| true |}];
  print_dyn (Set.exists (fun x -> x > 10) s |> Dyn.bool);
  [%expect {| false |}];
  ()
;;

let%expect_test "min_elt / min_elt_opt" =
  let s = Set.of_list (module Int) [ 3; 1; 2 ] in
  print_dyn (Set.min_elt s |> Dyn.int);
  [%expect {| 1 |}];
  print_dyn (Set.min_elt_opt s |> Dyn.option Dyn.int);
  [%expect {| Some 1 |}];
  let e = Set.empty (module Int) in
  require_does_raise (fun () -> Set.min_elt e);
  [%expect {| Not_found |}];
  print_dyn (Set.min_elt_opt e |> Dyn.option Dyn.int);
  [%expect {| None |}];
  ()
;;

let%expect_test "max_elt / max_elt_opt" =
  let s = Set.of_list (module Int) [ 3; 1; 2 ] in
  print_dyn (Set.max_elt s |> Dyn.int);
  [%expect {| 3 |}];
  print_dyn (Set.max_elt_opt s |> Dyn.option Dyn.int);
  [%expect {| Some 3 |}];
  let e = Set.empty (module Int) in
  require_does_raise (fun () -> Set.max_elt e);
  [%expect {| Not_found |}];
  print_dyn (Set.max_elt_opt e |> Dyn.option Dyn.int);
  [%expect {| None |}];
  ()
;;

let%expect_test "choose / choose_opt" =
  let s = Set.of_list (module Int) [ 3; 1; 2 ] in
  (* choose returns some element; for equal sets it should be deterministic. *)
  print_dyn (Set.choose s |> Dyn.int);
  [%expect {| 1 |}];
  print_dyn (Set.choose_opt s |> Dyn.option Dyn.int);
  [%expect {| Some 1 |}];
  let e = Set.empty (module Int) in
  require_does_raise (fun () -> Set.choose e);
  [%expect {| Not_found |}];
  print_dyn (Set.choose_opt e |> Dyn.option Dyn.int);
  [%expect {| None |}];
  ()
;;

let%expect_test "find_first / find_first_opt" =
  let s = Set.of_list (module Int) [ 1; 3; 5; 7; 9 ] in
  (* find_first with monotonically increasing predicate: first elt >= 4. *)
  print_dyn (Set.find_first (fun x -> x >= 4) s |> Dyn.int);
  [%expect {| 5 |}];
  print_dyn (Set.find_first_opt (fun x -> x >= 4) s |> Dyn.option Dyn.int);
  [%expect {| Some 5 |}];
  (* No element satisfies the predicate. *)
  require_does_raise (fun () -> Set.find_first (fun x -> x >= 10) s);
  [%expect {| Not_found |}];
  print_dyn (Set.find_first_opt (fun x -> x >= 10) s |> Dyn.option Dyn.int);
  [%expect {| None |}];
  ()
;;

let%expect_test "find_last / find_last_opt" =
  let s = Set.of_list (module Int) [ 1; 3; 5; 7; 9 ] in
  (* find_last with monotonically decreasing predicate: last elt <= 6. *)
  print_dyn (Set.find_last (fun x -> x <= 6) s |> Dyn.int);
  [%expect {| 5 |}];
  print_dyn (Set.find_last_opt (fun x -> x <= 6) s |> Dyn.option Dyn.int);
  [%expect {| Some 5 |}];
  require_does_raise (fun () -> Set.find_last (fun x -> x <= 0) s);
  [%expect {| Not_found |}];
  print_dyn (Set.find_last_opt (fun x -> x <= 0) s |> Dyn.option Dyn.int);
  [%expect {| None |}];
  ()
;;

let%expect_test "iter" =
  let s = Set.of_list (module Int) [ 3; 1; 2 ] in
  Set.iter (fun x -> print_dyn (Dyn.int x)) s;
  [%expect
    {|
    1
    2
    3
    |}];
  ()
;;

let%expect_test "fold" =
  let s = Set.of_list (module Int) [ 1; 2; 3; 4; 5 ] in
  let sum = Set.fold (fun x acc -> acc + x) s 0 in
  print_dyn (sum |> Dyn.int);
  [%expect {| 15 |}];
  ()
;;

let%expect_test "map" =
  let s = Set.of_list (module Int) [ 1; 2; 3 ] in
  let s2 = Set.map (fun x -> x * 10) s in
  print_set s2;
  [%expect {| [ 10; 20; 30 ] |}];
  (* map with identity returns physically equal set. *)
  let s3 = Set.map (fun x -> x) s in
  require (phys_equal s s3);
  [%expect {||}];
  ()
;;

let%expect_test "filter" =
  let s = Set.of_list (module Int) [ 1; 2; 3; 4; 5; 6 ] in
  let evens = Set.filter (fun x -> x mod 2 = 0) s in
  print_set evens;
  [%expect {| [ 2; 4; 6 ] |}];
  (* filter that keeps everything returns physically equal set. *)
  let s2 = Set.filter (fun _ -> true) s in
  require (phys_equal s s2);
  [%expect {||}];
  ()
;;

let%expect_test "filter_map" =
  let s = Set.of_list (module Int) [ 1; 2; 3; 4; 5; 6 ] in
  let halves_of_evens =
    Set.filter_map (fun x -> if x mod 2 = 0 then Some (x / 2) else None) s
  in
  print_set halves_of_evens;
  [%expect {| [ 1; 2; 3 ] |}];
  (* filter_map with Some identity returns physically equal set. *)
  let s2 = Set.filter_map (fun x -> Some x) s in
  require (phys_equal s s2);
  [%expect {||}];
  ()
;;

let%expect_test "partition" =
  let s = Set.of_list (module Int) [ 1; 2; 3; 4; 5; 6 ] in
  let evens, odds = Set.partition (fun x -> x mod 2 = 0) s in
  print_set evens;
  [%expect {| [ 2; 4; 6 ] |}];
  print_set odds;
  [%expect {| [ 1; 3; 5 ] |}];
  ()
;;

let%expect_test "split" =
  let s = Set.of_list (module Int) [ 1; 2; 3; 4; 5 ] in
  let l, present, r = Set.split 3 s in
  print_set l;
  [%expect {| [ 1; 2 ] |}];
  print_dyn (present |> Dyn.bool);
  [%expect {| true |}];
  print_set r;
  [%expect {| [ 4; 5 ] |}];
  (* Split on a missing element. *)
  let l2, present2, r2 = Set.split 6 s in
  print_set l2;
  [%expect {| [ 1; 2; 3; 4; 5 ] |}];
  print_dyn (present2 |> Dyn.bool);
  [%expect {| false |}];
  print_set r2;
  [%expect {| [] |}];
  ()
;;

let%expect_test "is_empty / is_singleton" =
  let e = Set.empty (module Int) in
  let s1 = Set.singleton (module Int) 1 in
  let s2 = Set.of_list (module Int) [ 1; 2 ] in
  print_dyn (Set.is_empty e |> Dyn.bool);
  [%expect {| true |}];
  print_dyn (Set.is_empty s1 |> Dyn.bool);
  [%expect {| false |}];
  print_dyn (Set.is_singleton e |> Dyn.bool);
  [%expect {| false |}];
  print_dyn (Set.is_singleton s1 |> Dyn.bool);
  [%expect {| true |}];
  print_dyn (Set.is_singleton s2 |> Dyn.bool);
  [%expect {| false |}];
  ()
;;

let%expect_test "to_seq / to_rev_seq" =
  let s = Set.of_list (module Int) [ 3; 1; 4; 1; 5 ] in
  print_dyn (Set.to_seq s |> List.of_seq |> Dyn.list Dyn.int);
  [%expect {| [ 1; 3; 4; 5 ] |}];
  print_dyn (Set.to_rev_seq s |> List.of_seq |> Dyn.list Dyn.int);
  [%expect {| [ 5; 4; 3; 1 ] |}];
  ()
;;

let%expect_test "to_seq_from" =
  let s = Set.of_list (module Int) [ 1; 3; 5; 7; 9 ] in
  print_dyn (Set.to_seq_from 4 s |> List.of_seq |> Dyn.list Dyn.int);
  [%expect {| [ 5; 7; 9 ] |}];
  print_dyn (Set.to_seq_from 5 s |> List.of_seq |> Dyn.list Dyn.int);
  [%expect {| [ 5; 7; 9 ] |}];
  print_dyn (Set.to_seq_from 10 s |> List.of_seq |> Dyn.list Dyn.int);
  [%expect {| [] |}];
  ()
;;

let%expect_test "add_seq" =
  let s = Set.of_list (module Int) [ 1; 2; 3 ] in
  let s2 = Set.add_seq (List.to_seq [ 4; 5; 6 ]) s in
  print_set s2;
  [%expect {| [ 1; 2; 3; 4; 5; 6 ] |}];
  ()
;;

let%expect_test "of_seq" =
  let s = Set.of_seq (module Int) (List.to_seq [ 5; 3; 1; 3; 5 ]) in
  print_set s;
  [%expect {| [ 1; 3; 5 ] |}];
  ()
;;

let%expect_test "incompatible compare raises" =
  let module Ord_rev = struct
    type t = int

    let compare t1 t2 = Int.compare t2 t1
  end
  in
  let s1 = Set.of_list (module Int) [ 1; 2 ] in
  let s2 = Set.of_list (module Ord_rev) [ 3; 4 ] in
  require_does_raise (fun () -> Set.union s1 s2);
  [%expect {| Invalid_argument("Set.union: sets have different compare functions.") |}];
  require_does_raise (fun () -> Set.inter s1 s2);
  [%expect {| Invalid_argument("Set.inter: sets have different compare functions.") |}];
  require_does_raise (fun () -> Set.diff s1 s2);
  [%expect {| Invalid_argument("Set.diff: sets have different compare functions.") |}];
  require_does_raise (fun () -> Set.disjoint s1 s2);
  [%expect {| Invalid_argument("Set.disjoint: sets have different compare functions.") |}];
  require_does_raise (fun () -> Set.equal s1 s2);
  [%expect {| Invalid_argument("Set.equal: sets have different compare functions.") |}];
  require_does_raise (fun () -> Set.compare s1 s2);
  [%expect {| Invalid_argument("Set.compare: sets have different compare functions.") |}];
  require_does_raise (fun () -> Set.subset s1 s2);
  [%expect {| Invalid_argument("Set.subset: sets have different compare functions.") |}];
  ()
;;

let%expect_test "compare - different sizes" =
  let s1 = Set.of_list (module Int) [ 1; 2 ] in
  let s2 = Set.of_list (module Int) [ 1; 2; 3 ] in
  let s3 = Set.of_list (module Int) [ 1 ] in
  (* s1 < s2 because s1 ends first. *)
  require (Set.compare s1 s2 < 0);
  [%expect {||}];
  (* s2 > s1. *)
  require (Set.compare s2 s1 > 0);
  [%expect {||}];
  (* s3 < s1. *)
  require (Set.compare s3 s1 < 0);
  [%expect {||}];
  (* Empty set < non-empty set. *)
  let e = Set.empty (module Int) in
  require (Set.compare e s1 < 0);
  [%expect {||}];
  require (Set.compare s1 e > 0);
  [%expect {||}];
  require (Set.compare e e = 0);
  [%expect {||}];
  ()
;;

let%expect_test "equal - different sizes" =
  let s1 = Set.of_list (module Int) [ 1; 2 ] in
  let s2 = Set.of_list (module Int) [ 1; 2; 3 ] in
  print_dyn (Set.equal s1 s2 |> Dyn.bool);
  [%expect {| false |}];
  print_dyn (Set.equal s2 s1 |> Dyn.bool);
  [%expect {| false |}];
  ()
;;

let%expect_test "subset - partial overlap" =
  let s1 = Set.of_list (module Int) [ 1; 3 ] in
  let s2 = Set.of_list (module Int) [ 1; 2; 3; 4 ] in
  let s3 = Set.of_list (module Int) [ 2; 3 ] in
  (* {1,3} is a subset of {1,2,3,4}. *)
  print_dyn (Set.subset s1 s2 |> Dyn.bool);
  [%expect {| true |}];
  (* {2,3} is not a subset of {1,3}. *)
  print_dyn (Set.subset s3 s1 |> Dyn.bool);
  [%expect {| false |}];
  (* {1,3} is not a subset of {2,3}. *)
  print_dyn (Set.subset s1 s3 |> Dyn.bool);
  [%expect {| false |}];
  (* Empty set is a subset of everything. *)
  let e = Set.empty (module Int) in
  print_dyn (Set.subset e s1 |> Dyn.bool);
  [%expect {| true |}];
  (* Non-empty set is not a subset of empty. *)
  print_dyn (Set.subset s1 e |> Dyn.bool);
  [%expect {| false |}];
  ()
;;

let%expect_test "map - reordering elements" =
  let s = Set.of_list (module Int) [ 1; 2; 3 ] in
  (* Map that reverses ordering. *)
  let s2 = Set.map (fun x -> 10 - x) s in
  print_set s2;
  [%expect {| [ 7; 8; 9 ] |}];
  ()
;;

let%expect_test "filter_map - removing elements" =
  let s = Set.of_list (module Int) [ 1; 2; 3; 4; 5; 6 ] in
  (* Filter_map that removes some elements and transforms others. *)
  let s2 = Set.filter_map (fun x -> if x mod 2 = 0 then Some (x / 2) else None) s in
  print_set s2;
  [%expect {| [ 1; 2; 3 ] |}];
  (* Filter_map that removes all elements. *)
  let s3 = Set.filter_map (fun _ -> None) s in
  print_set s3;
  [%expect {| [] |}];
  ()
;;

let%expect_test "inter - disjoint sets" =
  let s1 = Set.of_list (module Int) [ 1; 2; 3 ] in
  let s2 = Set.of_list (module Int) [ 4; 5; 6 ] in
  print_set (Set.inter s1 s2);
  [%expect {| [] |}];
  (* Inter with empty set. *)
  let e = Set.empty (module Int) in
  print_set (Set.inter s1 e);
  [%expect {| [] |}];
  print_set (Set.inter e s1);
  [%expect {| [] |}];
  ()
;;

let%expect_test "diff - various cases" =
  let s1 = Set.of_list (module Int) [ 1; 2; 3; 4; 5 ] in
  let s2 = Set.of_list (module Int) [ 2; 4 ] in
  print_set (Set.diff s1 s2);
  [%expect {| [ 1; 3; 5 ] |}];
  (* Diff with empty. *)
  let e = Set.empty (module Int) in
  print_set (Set.diff s1 e);
  [%expect {| [ 1; 2; 3; 4; 5 ] |}];
  print_set (Set.diff e s1);
  [%expect {| [] |}];
  (* Diff with self. *)
  print_set (Set.diff s1 s1);
  [%expect {| [] |}];
  ()
;;

let%expect_test "large set - triggers bal rotations" =
  let s = Set.of_list (module Int) (List.init 100 (fun i -> i)) in
  print_dyn (Set.cardinal s |> Dyn.int);
  [%expect {| 100 |}];
  print_dyn (Set.min_elt s |> Dyn.int);
  [%expect {| 0 |}];
  print_dyn (Set.max_elt s |> Dyn.int);
  [%expect {| 99 |}];
  (* Remove many elements to trigger rebalancing. *)
  let s2 =
    List.fold_left (fun acc i -> Set.remove (i * 2) acc) s (List.init 50 (fun i -> i))
  in
  print_dyn (Set.cardinal s2 |> Dyn.int);
  [%expect {| 50 |}];
  (* Add elements in reverse order. *)
  let s3 =
    List.fold_left
      (fun acc i -> Set.add i acc)
      (Set.empty (module Int))
      (List.init 50 (fun i -> 50 - i))
  in
  print_dyn (Set.cardinal s3 |> Dyn.int);
  [%expect {| 50 |}];
  ()
;;

let%expect_test "union - physical equality and larger" =
  let s1 = Set.of_list (module Int) [ 1; 2; 3 ] in
  let e = Set.empty (module Int) in
  (* Characterizing: union with empty returns physically equal set.
     Not documented, could change in future versions. *)
  let u1 = Set.union s1 e in
  require (phys_equal s1 u1);
  [%expect {||}];
  let u2 = Set.union e s1 in
  require (phys_equal s1 u2);
  [%expect {||}];
  (* Larger union to exercise more internal paths. *)
  let s2 = Set.of_list (module Int) [ 3; 4; 5; 6; 7 ] in
  let u3 = Set.union s1 s2 in
  print_set u3;
  [%expect {| [ 1; 2; 3; 4; 5; 6; 7 ] |}];
  (* Union with singleton + larger set. *)
  let s3 = Set.singleton (module Int) 10 in
  let u4 = Set.union s3 s2 in
  print_set u4;
  [%expect {| [ 3; 4; 5; 6; 7; 10 ] |}];
  let u5 = Set.union s2 s3 in
  print_set u5;
  [%expect {| [ 3; 4; 5; 6; 7; 10 ] |}];
  (* Characterizing: union with a singleton already in the set returns
     physically equal. Not documented, could change in future versions. *)
  let s4 = Set.singleton (module Int) 3 in
  let u6 = Set.union s1 s4 in
  require (phys_equal s1 u6);
  [%expect {||}];
  (* Reverse direction: singleton already in s2. *)
  let u7 = Set.union s4 s2 in
  require (phys_equal s2 u7);
  [%expect {||}];
  ()
;;

let%expect_test "disjoint - edge cases" =
  let e = Set.empty (module Int) in
  let s1 = Set.singleton (module Int) 1 in
  (* Empty sets are disjoint. *)
  print_dyn (Set.disjoint e e |> Dyn.bool);
  [%expect {| true |}];
  (* Empty and non-empty are disjoint. *)
  print_dyn (Set.disjoint e s1 |> Dyn.bool);
  [%expect {| true |}];
  print_dyn (Set.disjoint s1 e |> Dyn.bool);
  [%expect {| true |}];
  (* Larger overlapping sets. *)
  let s2 = Set.of_list (module Int) [ 1; 3; 5; 7; 9 ] in
  let s3 = Set.of_list (module Int) [ 2; 4; 6; 8; 10 ] in
  let s4 = Set.of_list (module Int) [ 5; 10; 15 ] in
  print_dyn (Set.disjoint s2 s3 |> Dyn.bool);
  [%expect {| true |}];
  print_dyn (Set.disjoint s2 s4 |> Dyn.bool);
  [%expect {| false |}];
  print_dyn (Set.disjoint s3 s4 |> Dyn.bool);
  [%expect {| false |}];
  ()
;;

let%expect_test "bal - descending insertion triggers right rotations" =
  (* Inserting in decreasing order exercises rebalancing. *)
  let s =
    List.fold_left
      (fun acc i -> Set.add i acc)
      (Set.empty (module Int))
      (List.rev (List.init 20 (fun i -> i)))
  in
  print_dyn (Set.cardinal s |> Dyn.int);
  [%expect {| 20 |}];
  print_set s;
  [%expect {| [ 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19 ] |}];
  ()
;;

let%expect_test "bal - zig-zag insertion triggers double rotations" =
  (* Zig-zag insertion exercises double rotations. *)
  let s = Set.empty (module Int) in
  let s = Set.add 10 s in
  let s = Set.add 3 s in
  let s = Set.add 7 s in
  let s = Set.add 5 s in
  print_set s;
  [%expect {| [ 3; 5; 7; 10 ] |}];
  (* Same pattern in the other direction. *)
  let s2 = Set.empty (module Int) in
  let s2 = Set.add 1 s2 in
  let s2 = Set.add 8 s2 in
  let s2 = Set.add 4 s2 in
  let s2 = Set.add 6 s2 in
  print_set s2;
  [%expect {| [ 1; 4; 6; 8 ] |}];
  (* Larger zig-zag to exercise deeper double rotations. *)
  let s3 =
    List.fold_left
      (fun acc i -> Set.add i acc)
      (Set.empty (module Int))
      [ 50; 20; 40; 10; 30; 60; 55; 70; 65 ]
  in
  print_dyn (Set.cardinal s3 |> Dyn.int);
  [%expect {| 9 |}];
  print_set s3;
  [%expect {| [ 10; 20; 30; 40; 50; 55; 60; 65; 70 ] |}];
  ()
;;

let%expect_test "remove - from left subtree and internal merge" =
  (* Build a balanced tree, then remove from left subtree. *)
  let s =
    List.fold_left
      (fun acc i -> Set.add i acc)
      (Set.empty (module Int))
      [ 4; 2; 6; 1; 3; 5; 7 ]
  in
  let s2 = Set.remove 1 s in
  print_set s2;
  [%expect {| [ 2; 3; 4; 5; 6; 7 ] |}];
  (* Remove a node with two children to exercise internal merge. *)
  let s3 = Set.remove 4 s in
  print_set s3;
  [%expect {| [ 1; 2; 3; 5; 6; 7 ] |}];
  let s4 = Set.remove 2 s in
  print_set s4;
  [%expect {| [ 1; 3; 4; 5; 6; 7 ] |}];
  ()
;;

let%expect_test "max_elt_opt - deeper tree" =
  let s = Set.of_list (module Int) [ 1; 2; 3; 4; 5 ] in
  print_dyn (Set.max_elt_opt s |> Dyn.option Dyn.int);
  [%expect {| Some 5 |}];
  ()
;;

let%expect_test "find_first / find_first_opt - deeper traversals" =
  let s = Set.of_list (module Int) [ 2; 4; 6; 8; 10 ] in
  (* Various predicates to exercise different traversal paths. *)
  print_dyn (Set.find_first (fun x -> x >= 5) s |> Dyn.int);
  [%expect {| 6 |}];
  print_dyn (Set.find_first_opt (fun x -> x >= 5) s |> Dyn.option Dyn.int);
  [%expect {| Some 6 |}];
  print_dyn (Set.find_first (fun x -> x >= 3) s |> Dyn.int);
  [%expect {| 4 |}];
  print_dyn (Set.find_first_opt (fun x -> x >= 3) s |> Dyn.option Dyn.int);
  [%expect {| Some 4 |}];
  (* Predicate matching all elements. *)
  print_dyn (Set.find_first (fun x -> x >= 1) s |> Dyn.int);
  [%expect {| 2 |}];
  print_dyn (Set.find_first_opt (fun x -> x >= 1) s |> Dyn.option Dyn.int);
  [%expect {| Some 2 |}];
  ()
;;

let%expect_test "find_last / find_last_opt - deeper traversals" =
  let s = Set.of_list (module Int) [ 2; 4; 6; 8; 10 ] in
  print_dyn (Set.find_last (fun x -> x <= 7) s |> Dyn.int);
  [%expect {| 6 |}];
  print_dyn (Set.find_last_opt (fun x -> x <= 7) s |> Dyn.option Dyn.int);
  [%expect {| Some 6 |}];
  ()
;;

let%expect_test "join - rebalancing paths" =
  (* Union of trees with very different sizes to exercise rebalancing. *)
  let big = Set.of_list (module Int) (List.init 30 (fun i -> i)) in
  let small = Set.of_list (module Int) [ 100; 101 ] in
  let u = Set.union big small in
  print_dyn (Set.cardinal u |> Dyn.int);
  [%expect {| 32 |}];
  let u2 = Set.union small big in
  print_dyn (Set.cardinal u2 |> Dyn.int);
  [%expect {| 32 |}];
  ()
;;

let%expect_test "concat - via filter removing root" =
  let s = Set.of_list (module Int) [ 1; 2; 3; 4; 5 ] in
  (* filter that removes some middle elements. *)
  let s2 = Set.filter (fun x -> x <> 3) s in
  print_set s2;
  [%expect {| [ 1; 2; 4; 5 ] |}];
  (* diff exercises concat when elements are present. *)
  let s3 = Set.of_list (module Int) [ 2; 3; 4 ] in
  print_set (Set.diff s s3);
  [%expect {| [ 1; 5 ] |}];
  ()
;;

let%expect_test "partition - exercises join and concat" =
  let s = Set.of_list (module Int) (List.init 20 (fun i -> i)) in
  let evens, odds = Set.partition (fun x -> x mod 2 = 0) s in
  print_dyn (Set.cardinal evens |> Dyn.int);
  [%expect {| 10 |}];
  print_dyn (Set.cardinal odds |> Dyn.int);
  [%expect {| 10 |}];
  ()
;;

let%expect_test "of_sorted_list" =
  (* Larger list to exercise of_sorted_list path. *)
  let s = Set.of_list (module Int) [ 10; 9; 8; 7; 6; 5; 4; 3; 2; 1 ] in
  print_set s;
  [%expect {| [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ] |}];
  print_dyn (Set.cardinal s |> Dyn.int);
  [%expect {| 10 |}];
  ()
;;

let%expect_test "to_seq_from - various positions" =
  let s = Set.of_list (module Int) [ 2; 4; 6; 8; 10 ] in
  (* From a key less than all elements. *)
  print_dyn (Set.to_seq_from 1 s |> List.of_seq |> Dyn.list Dyn.int);
  [%expect {| [ 2; 4; 6; 8; 10 ] |}];
  (* From an exact key. *)
  print_dyn (Set.to_seq_from 6 s |> List.of_seq |> Dyn.list Dyn.int);
  [%expect {| [ 6; 8; 10 ] |}];
  ()
;;

let%expect_test "exists - short-circuit" =
  let s = Set.of_list (module Int) [ 1; 2; 3 ] in
  print_dyn (Set.exists (fun x -> x = 3) s |> Dyn.bool);
  [%expect {| true |}];
  print_dyn (Set.exists (fun x -> x = 1) s |> Dyn.bool);
  [%expect {| true |}];
  ()
;;

let%expect_test "map - reordering triggers try_join's union path" =
  (* Map that reverses element ordering. *)
  let s = Set.of_list (module Int) [ 1; 5; 10 ] in
  let s2 = Set.map (fun x -> 11 - x) s in
  print_set s2;
  [%expect {| [ 1; 6; 10 ] |}];
  ()
;;

let%expect_test "filter_map - removing and reordering" =
  let s = Set.of_list (module Int) [ 1; 2; 3; 4; 5; 6; 7; 8 ] in
  (* Remove some, keep others. *)
  let s2 = Set.filter_map (fun x -> if x mod 3 = 0 then None else Some x) s in
  print_set s2;
  [%expect {| [ 1; 2; 4; 5; 7; 8 ] |}];
  ()
;;

let%expect_test "diff - physical equality with empty" =
  let s = Set.of_list (module Int) [ 1; 2; 3; 4; 5 ] in
  let e = Set.empty (module Int) in
  (* Characterizing: diff with empty returns physically equal set.
     Not documented, could change in future versions. *)
  let s2 = Set.diff s e in
  require (phys_equal s s2);
  [%expect {||}];
  ()
;;

let%expect_test "add_seq - physical equality" =
  let s = Set.of_list (module Int) [ 1; 2; 3 ] in
  (* Characterizing: add_seq with empty sequence returns physically equal set.
     Not documented, could change in future versions. *)
  let s2 = Set.add_seq Seq.empty s in
  require (phys_equal s s2);
  [%expect {||}];
  (* Characterizing: add_seq with all-already-present elements returns
     physically equal set. This follows from [add]'s documented physical
     equality guarantee composed through [Seq.fold_left], but [add_seq]
     itself does not document it. *)
  let s3 = Set.add_seq (List.to_seq (Set.to_list s)) s in
  require (phys_equal s s3);
  [%expect {||}];
  ()
;;
