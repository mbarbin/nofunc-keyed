(***********************************************************************************)
(*  nofunc-keyed: Keyed data structures adapted from OCaml Stdlib but no functors  *)
(*  SPDX-FileCopyrightText: 2025 Mathieu Barbin <mathieu.barbin@gmail.com>         *)
(*  SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception   *)
(***********************************************************************************)

(* [Nofunc_set.Set] shares its tree implementation with [Nofunc_stdset.Set],
   which is exercised thoroughly by the [stdset] test suite. Here we only
   need to exercise each function of the wrapper itself, using an
   [Ordering.t]-returning [compare], as required by this package. *)

module Set = Nofunc_set.Set

module Int = struct
  type t = int

  let compare (a : int) (b : int) = Ordering.of_int (Stdlib.compare a b)
end

let print_elements s = print_dyn (Set.elements s |> Dyn.list Dyn.int)

let%expect_test "empty / is_empty / is_singleton / cardinal" =
  let e = Set.empty (module Int) in
  require (Set.is_empty e);
  [%expect {||}];
  print_dyn (Set.cardinal e |> Dyn.int);
  [%expect {| 0 |}];
  ()
;;

let%expect_test "singleton / is_singleton / elements / to_list" =
  let s = Set.singleton (module Int) 1 in
  require (Set.is_singleton s);
  [%expect {||}];
  print_elements s;
  [%expect {| [ 1 ] |}];
  print_dyn (Set.to_list s |> Dyn.list Dyn.int);
  [%expect {| [ 1 ] |}];
  ()
;;

let%expect_test "add / remove" =
  let s = Set.empty (module Int) in
  let s = Set.add 1 s in
  let s = Set.add 2 s in
  (* Re-adding an existing element is a no-op returning a physically equal set. *)
  let s1 = Set.add 2 s in
  require (phys_equal s s1);
  [%expect {||}];
  let s = Set.remove 1 s in
  print_elements s;
  [%expect {| [ 2 ] |}];
  ()
;;

let%expect_test "of_list / mem / find / find_opt" =
  let s = Set.of_list (module Int) [ 3; 1; 2; 1 ] in
  print_elements s;
  [%expect {| [ 1; 2; 3 ] |}];
  print_dyn (Set.mem 2 s |> Dyn.bool);
  [%expect {| true |}];
  print_dyn (Set.mem 4 s |> Dyn.bool);
  [%expect {| false |}];
  print_dyn (Set.find 2 s |> Dyn.int);
  [%expect {| 2 |}];
  require_does_raise (fun () -> Set.find 4 s);
  [%expect {| Not_found |}];
  print_dyn (Set.find_opt 4 s |> Dyn.option Dyn.int);
  [%expect {| None |}];
  ()
;;

let%expect_test "find_first / find_first_opt / find_last / find_last_opt" =
  let s = Set.of_list (module Int) [ 1; 2; 3 ] in
  print_dyn (Set.find_first (fun x -> x >= 2) s |> Dyn.int);
  [%expect {| 2 |}];
  print_dyn (Set.find_first_opt (fun x -> x >= 99) s |> Dyn.option Dyn.int);
  [%expect {| None |}];
  print_dyn (Set.find_last (fun x -> x <= 2) s |> Dyn.int);
  [%expect {| 2 |}];
  print_dyn (Set.find_last_opt (fun x -> x <= 0) s |> Dyn.option Dyn.int);
  [%expect {| None |}];
  ()
;;

let%expect_test "min_elt / max_elt / choose" =
  let s = Set.of_list (module Int) [ 1; 2; 3 ] in
  print_dyn (Set.min_elt s |> Dyn.int);
  [%expect {| 1 |}];
  print_dyn (Set.min_elt_opt s |> Dyn.option Dyn.int);
  [%expect {| Some 1 |}];
  print_dyn (Set.max_elt s |> Dyn.int);
  [%expect {| 3 |}];
  print_dyn (Set.max_elt_opt s |> Dyn.option Dyn.int);
  [%expect {| Some 3 |}];
  print_dyn (Set.choose s |> Dyn.int);
  [%expect {| 1 |}];
  print_dyn (Set.choose_opt s |> Dyn.option Dyn.int);
  [%expect {| Some 1 |}];
  ()
;;

let%expect_test "iter / fold / for_all / exists" =
  let s = Set.of_list (module Int) [ 1; 2; 3 ] in
  let count = ref 0 in
  Set.iter (fun _ -> incr count) s;
  print_dyn (!count |> Dyn.int);
  [%expect {| 3 |}];
  let sum = Set.fold (fun x acc -> acc + x) s 0 in
  print_dyn (sum |> Dyn.int);
  [%expect {| 6 |}];
  print_dyn (Set.for_all (fun x -> x > 0) s |> Dyn.bool);
  [%expect {| true |}];
  print_dyn (Set.exists (fun x -> x > 2) s |> Dyn.bool);
  [%expect {| true |}];
  ()
;;

let%expect_test "map / filter / filter_map / partition" =
  let s = Set.of_list (module Int) [ 1; 2; 3 ] in
  print_dyn (Set.map (fun x -> x * 10) s |> Set.elements |> Dyn.list Dyn.int);
  [%expect {| [ 10; 20; 30 ] |}];
  print_dyn (Set.filter (fun x -> x <> 2) s |> Set.elements |> Dyn.list Dyn.int);
  [%expect {| [ 1; 3 ] |}];
  print_dyn
    (Set.filter_map (fun x -> if x = 2 then None else Some (x * 100)) s
     |> Set.elements
     |> Dyn.list Dyn.int);
  [%expect {| [ 100; 300 ] |}];
  let t, f = Set.partition (fun x -> x <= 1) s in
  print_dyn (Set.elements t |> Dyn.list Dyn.int);
  [%expect {| [ 1 ] |}];
  print_dyn (Set.elements f |> Dyn.list Dyn.int);
  [%expect {| [ 2; 3 ] |}];
  ()
;;

let%expect_test "split" =
  let s = Set.of_list (module Int) [ 1; 2; 3 ] in
  let l, present, r = Set.split 2 s in
  print_dyn (Set.elements l |> Dyn.list Dyn.int);
  [%expect {| [ 1 ] |}];
  print_dyn (present |> Dyn.bool);
  [%expect {| true |}];
  print_dyn (Set.elements r |> Dyn.list Dyn.int);
  [%expect {| [ 3 ] |}];
  ()
;;

let%expect_test "union / inter / disjoint / diff" =
  let s1 = Set.of_list (module Int) [ 1; 2 ] in
  let s2 = Set.of_list (module Int) [ 2; 3 ] in
  print_dyn (Set.union s1 s2 |> Set.elements |> Dyn.list Dyn.int);
  [%expect {| [ 1; 2; 3 ] |}];
  print_dyn (Set.inter s1 s2 |> Set.elements |> Dyn.list Dyn.int);
  [%expect {| [ 2 ] |}];
  print_dyn (Set.disjoint s1 s2 |> Dyn.bool);
  [%expect {| false |}];
  print_dyn (Set.diff s1 s2 |> Set.elements |> Dyn.list Dyn.int);
  [%expect {| [ 1 ] |}];
  (* Union with an empty set on either side returns the other set physically
     unchanged. *)
  let empty = Set.empty (module Int) in
  require (phys_equal s1 (Set.union s1 empty));
  [%expect {||}];
  require (phys_equal s1 (Set.union empty s1));
  [%expect {||}];
  ()
;;

let%expect_test "equal / compare / subset" =
  let s1 = Set.of_list (module Int) [ 1; 2 ] in
  let s2 = Set.of_list (module Int) [ 1; 2 ] in
  let s3 = Set.of_list (module Int) [ 1; 2; 3 ] in
  print_dyn (Set.equal s1 s2 |> Dyn.bool);
  [%expect {| true |}];
  print_dyn (Set.equal s1 s3 |> Dyn.bool);
  [%expect {| false |}];
  print_dyn (Set.compare s1 s2 |> Dyn.int);
  [%expect {| 0 |}];
  print_dyn (Set.subset s1 s3 |> Dyn.bool);
  [%expect {| true |}];
  ()
;;

let%expect_test "to_seq / to_rev_seq / to_seq_from / add_seq / of_seq" =
  let s = Set.of_list (module Int) [ 1; 2; 3 ] in
  print_dyn (Set.to_seq s |> List.of_seq |> Dyn.list Dyn.int);
  [%expect {| [ 1; 2; 3 ] |}];
  print_dyn (Set.to_rev_seq s |> List.of_seq |> Dyn.list Dyn.int);
  [%expect {| [ 3; 2; 1 ] |}];
  print_dyn (Set.to_seq_from 2 s |> List.of_seq |> Dyn.list Dyn.int);
  [%expect {| [ 2; 3 ] |}];
  let s2 = Set.add_seq (List.to_seq [ 4 ]) s in
  print_elements s2;
  [%expect {| [ 1; 2; 3; 4 ] |}];
  let s3 = Set.of_seq (module Int) (List.to_seq [ 5; 6 ]) in
  print_elements s3;
  [%expect {| [ 5; 6 ] |}];
  ()
;;

let%expect_test "different compare functions" =
  let module Int_rev = struct
    type t = int

    let compare a b = Ordering.reverse Int.compare a b
  end
  in
  let s1 = Set.of_list (module Int) [ 1 ] in
  let s2 = Set.of_list (module Int_rev) [ 2; 1 ] in
  require_does_raise (fun () -> Set.union s1 s2);
  [%expect {| Invalid_argument("Set.union: sets have different compare functions.") |}];
  ()
;;
