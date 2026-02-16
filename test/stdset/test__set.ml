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
