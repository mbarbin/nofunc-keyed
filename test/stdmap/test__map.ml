(***********************************************************************************)
(*  nofunc-keyed: Keyed data structures adapted from OCaml Stdlib but no functors  *)
(*  SPDX-FileCopyrightText: 2025 Mathieu Barbin <mathieu.barbin@gmail.com>         *)
(*  SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception   *)
(***********************************************************************************)

module Map = Nofunc_stdmap.Map

let print_binding (k, v) = print_dyn (Dyn.pair Dyn.int Dyn.string (k, v))
let print_bindings m = print_dyn (Map.bindings m |> Dyn.list (Dyn.pair Dyn.int Dyn.string))

let%expect_test "empty" =
  let e = Map.empty (module Int) in
  require (Map.is_empty e);
  [%expect {||}];
  print_dyn (Map.cardinal e |> Dyn.int);
  [%expect {| 0 |}];
  ()
;;

let%expect_test "singleton" =
  let m = Map.singleton (module Int) 1 "one" in
  require (not (Map.is_empty m));
  [%expect {||}];
  require (Map.is_singleton m);
  [%expect {||}];
  print_dyn (Map.cardinal m |> Dyn.int);
  [%expect {| 1 |}];
  print_bindings m;
  [%expect {| [ (1, "one") ] |}];
  ()
;;

let%expect_test "add" =
  let m = Map.empty (module Int) in
  let m = Map.add 3 "three" m in
  let m = Map.add 1 "one" m in
  let m = Map.add 2 "two" m in
  print_bindings m;
  [%expect {| [ (1, "one"); (2, "two"); (3, "three") ] |}];
  (* Adding with same key and physically equal value returns physically equal map. *)
  let m2 = Map.add 2 "two" m in
  require (phys_equal m m2);
  [%expect {||}];
  (* Adding with same key but different value replaces. *)
  let m3 = Map.add 2 "TWO" m in
  print_bindings m3;
  [%expect {| [ (1, "one"); (2, "TWO"); (3, "three") ] |}];
  ()
;;

let%expect_test "remove" =
  let m = Map.of_list (module Int) [ 1, "one"; 2, "two"; 3, "three" ] in
  let m2 = Map.remove 2 m in
  print_bindings m2;
  [%expect {| [ (1, "one"); (3, "three") ] |}];
  (* Removing a non-existent key returns physically equal map. *)
  let m3 = Map.remove 99 m in
  require (phys_equal m m3);
  [%expect {||}];
  ()
;;

let%expect_test "of_list" =
  let m = Map.of_list (module Int) [ 3, "three"; 1, "one"; 2, "two"; 1, "ONE" ] in
  print_bindings m;
  [%expect {| [ (1, "ONE"); (2, "two"); (3, "three") ] |}];
  print_dyn (Map.cardinal m |> Dyn.int);
  [%expect {| 3 |}];
  ()
;;

let%expect_test "bindings / to_list" =
  let m = Map.of_list (module Int) [ 3, "three"; 1, "one"; 2, "two" ] in
  print_dyn (Map.bindings m |> Dyn.list (Dyn.pair Dyn.int Dyn.string));
  [%expect {| [ (1, "one"); (2, "two"); (3, "three") ] |}];
  print_dyn (Map.to_list m |> Dyn.list (Dyn.pair Dyn.int Dyn.string));
  [%expect {| [ (1, "one"); (2, "two"); (3, "three") ] |}];
  ()
;;

let%expect_test "mem" =
  let m = Map.of_list (module Int) [ 1, "one"; 2, "two"; 3, "three" ] in
  print_dyn (Map.mem 2 m |> Dyn.bool);
  [%expect {| true |}];
  print_dyn (Map.mem 4 m |> Dyn.bool);
  [%expect {| false |}];
  (* Exercise mem on a larger map. *)
  let m2 =
    Map.of_list (module Int) [ 1, "one"; 2, "two"; 3, "three"; 4, "four"; 5, "five" ]
  in
  print_dyn (Map.mem 1 m2 |> Dyn.bool);
  [%expect {| true |}];
  print_dyn (Map.mem 5 m2 |> Dyn.bool);
  [%expect {| true |}];
  ()
;;

let%expect_test "find / find_opt" =
  let m = Map.of_list (module Int) [ 1, "one"; 2, "two"; 3, "three" ] in
  print_dyn (Map.find 2 m |> Dyn.string);
  [%expect {| "two" |}];
  require_does_raise (fun () -> Map.find 4 m);
  [%expect {| Not_found |}];
  print_dyn (Map.find_opt 2 m |> Dyn.option Dyn.string);
  [%expect {| Some "two" |}];
  print_dyn (Map.find_opt 4 m |> Dyn.option Dyn.string);
  [%expect {| None |}];
  (* Exercise find/find_opt on a larger map. *)
  let m2 =
    Map.of_list (module Int) [ 1, "one"; 2, "two"; 3, "three"; 4, "four"; 5, "five" ]
  in
  print_dyn (Map.find 1 m2 |> Dyn.string);
  [%expect {| "one" |}];
  print_dyn (Map.find 5 m2 |> Dyn.string);
  [%expect {| "five" |}];
  print_dyn (Map.find_opt 1 m2 |> Dyn.option Dyn.string);
  [%expect {| Some "one" |}];
  print_dyn (Map.find_opt 5 m2 |> Dyn.option Dyn.string);
  [%expect {| Some "five" |}];
  ()
;;

let%expect_test "add_to_list" =
  let m = Map.empty (module Int) in
  let m = Map.add_to_list 1 "a" m in
  let m = Map.add_to_list 1 "b" m in
  let m = Map.add_to_list 2 "c" m in
  print_dyn (Map.bindings m |> Dyn.list (Dyn.pair Dyn.int (Dyn.list Dyn.string)));
  [%expect {| [ (1, [ "b"; "a" ]); (2, [ "c" ]) ] |}];
  ()
;;

let%expect_test "update" =
  let m = Map.of_list (module Int) [ 1, "one"; 2, "two" ] in
  (* Update existing key. *)
  let m2 = Map.update 1 (fun v -> Option.map String.uppercase_ascii v) m in
  print_bindings m2;
  [%expect {| [ (1, "ONE"); (2, "two") ] |}];
  (* Remove via update. *)
  let m3 = Map.update 2 (fun _ -> None) m in
  print_bindings m3;
  [%expect {| [ (1, "one") ] |}];
  (* Add via update. *)
  let m4 = Map.update 3 (fun _ -> Some "three") m in
  print_bindings m4;
  [%expect {| [ (1, "one"); (2, "two"); (3, "three") ] |}];
  ()
;;

let%expect_test "merge" =
  let m1 = Map.of_list (module Int) [ 1, "a"; 2, "b" ] in
  let m2 = Map.of_list (module Int) [ 2, "B"; 3, "C" ] in
  let merge_fn _key v1 v2 =
    match v1, v2 with
    | Some v1, Some v2 -> Some (v1 ^ "+" ^ v2)
    | Some v, None | None, Some v -> Some v
    | None, None ->
      (* unreachable: Map.merge only calls f for keys in at least one map *)
      assert false [@coverage off]
  in
  let merged = Map.merge merge_fn m1 m2 in
  print_bindings merged;
  [%expect {| [ (1, "a"); (2, "b+B"); (3, "C") ] |}];
  ()
;;

let%expect_test "union" =
  let m1 = Map.of_list (module Int) [ 1, "a"; 2, "b" ] in
  let m2 = Map.of_list (module Int) [ 2, "B"; 3, "C" ] in
  let u = Map.union (fun _key v1 v2 -> Some (v1 ^ "+" ^ v2)) m1 m2 in
  print_bindings u;
  [%expect {| [ (1, "a"); (2, "b+B"); (3, "C") ] |}];
  ()
;;

let%expect_test "equal / compare" =
  let m1 = Map.of_list (module Int) [ 1, "one"; 2, "two" ] in
  let m2 = Map.of_list (module Int) [ 2, "two"; 1, "one" ] in
  let m3 = Map.of_list (module Int) [ 1, "one"; 2, "TWO" ] in
  print_dyn (Map.equal String.equal m1 m2 |> Dyn.bool);
  [%expect {| true |}];
  print_dyn (Map.equal String.equal m1 m3 |> Dyn.bool);
  [%expect {| false |}];
  print_dyn (Map.compare String.compare m1 m2 |> Dyn.int);
  [%expect {| 0 |}];
  require (Map.compare String.compare m1 m3 <> 0);
  [%expect {||}];
  ()
;;

let%expect_test "for_all / exists" =
  let m = Map.of_list (module Int) [ 2, "two"; 4, "four"; 6, "six" ] in
  print_dyn (Map.for_all (fun k _v -> k mod 2 = 0) m |> Dyn.bool);
  [%expect {| true |}];
  print_dyn (Map.for_all (fun k _v -> k > 3) m |> Dyn.bool);
  [%expect {| false |}];
  print_dyn (Map.exists (fun k _v -> k > 5) m |> Dyn.bool);
  [%expect {| true |}];
  print_dyn (Map.exists (fun k _v -> k > 10) m |> Dyn.bool);
  [%expect {| false |}];
  (* Exercise exists with early short-circuit. *)
  let m2 =
    Map.of_list (module Int) [ 1, "one"; 2, "two"; 3, "three"; 4, "four"; 5, "five" ]
  in
  print_dyn (Map.exists (fun (k : int) (_ : string) -> k = 1) m2 |> Dyn.bool);
  [%expect {| true |}];
  ()
;;

let%expect_test "min_binding / min_binding_opt" =
  let m = Map.of_list (module Int) [ 3, "three"; 1, "one"; 2, "two" ] in
  print_binding (Map.min_binding m);
  [%expect {| (1, "one") |}];
  print_dyn (Map.min_binding_opt m |> Dyn.option (Dyn.pair Dyn.int Dyn.string));
  [%expect {| Some (1, "one") |}];
  let e = Map.empty (module Int) in
  require_does_raise (fun () -> Map.min_binding e);
  [%expect {| Not_found |}];
  print_dyn (Map.min_binding_opt e |> Dyn.option (Dyn.pair Dyn.int Dyn.string));
  [%expect {| None |}];
  ()
;;

let%expect_test "max_binding / max_binding_opt" =
  let m = Map.of_list (module Int) [ 3, "three"; 1, "one"; 2, "two" ] in
  print_binding (Map.max_binding m);
  [%expect {| (3, "three") |}];
  print_dyn (Map.max_binding_opt m |> Dyn.option (Dyn.pair Dyn.int Dyn.string));
  [%expect {| Some (3, "three") |}];
  let e = Map.empty (module Int) in
  require_does_raise (fun () -> Map.max_binding e);
  [%expect {| Not_found |}];
  print_dyn (Map.max_binding_opt e |> Dyn.option (Dyn.pair Dyn.int Dyn.string));
  [%expect {| None |}];
  ()
;;

let%expect_test "choose / choose_opt" =
  let m = Map.of_list (module Int) [ 3, "three"; 1, "one"; 2, "two" ] in
  print_binding (Map.choose m);
  [%expect {| (1, "one") |}];
  print_dyn (Map.choose_opt m |> Dyn.option (Dyn.pair Dyn.int Dyn.string));
  [%expect {| Some (1, "one") |}];
  let e = Map.empty (module Int) in
  require_does_raise (fun () -> Map.choose e);
  [%expect {| Not_found |}];
  print_dyn (Map.choose_opt e |> Dyn.option (Dyn.pair Dyn.int Dyn.string));
  [%expect {| None |}];
  ()
;;

let%expect_test "find_first / find_first_opt" =
  let m = Map.of_list (module Int) [ 1, "one"; 3, "three"; 5, "five"; 7, "seven" ] in
  print_binding (Map.find_first (fun k -> k >= 4) m);
  [%expect {| (5, "five") |}];
  print_dyn
    (Map.find_first_opt (fun k -> k >= 4) m |> Dyn.option (Dyn.pair Dyn.int Dyn.string));
  [%expect {| Some (5, "five") |}];
  require_does_raise (fun () -> Map.find_first (fun k -> k >= 10) m);
  [%expect {| Not_found |}];
  print_dyn
    (Map.find_first_opt (fun k -> k >= 10) m |> Dyn.option (Dyn.pair Dyn.int Dyn.string));
  [%expect {| None |}];
  (* Exercise find_first with a predicate matching all elements. *)
  print_binding (Map.find_first (fun k -> k >= 1) m);
  [%expect {| (1, "one") |}];
  print_dyn
    (Map.find_first_opt (fun k -> k >= 1) m |> Dyn.option (Dyn.pair Dyn.int Dyn.string));
  [%expect {| Some (1, "one") |}];
  ()
;;

let%expect_test "find_last / find_last_opt" =
  let m = Map.of_list (module Int) [ 1, "one"; 3, "three"; 5, "five"; 7, "seven" ] in
  print_binding (Map.find_last (fun k -> k <= 6) m);
  [%expect {| (5, "five") |}];
  print_dyn
    (Map.find_last_opt (fun k -> k <= 6) m |> Dyn.option (Dyn.pair Dyn.int Dyn.string));
  [%expect {| Some (5, "five") |}];
  require_does_raise (fun () -> Map.find_last (fun k -> k <= 0) m);
  [%expect {| Not_found |}];
  print_dyn
    (Map.find_last_opt (fun k -> k <= 0) m |> Dyn.option (Dyn.pair Dyn.int Dyn.string));
  [%expect {| None |}];
  ()
;;

let%expect_test "iter" =
  let m = Map.of_list (module Int) [ 3, "three"; 1, "one"; 2, "two" ] in
  Map.iter (fun k v -> print_dyn (Dyn.pair Dyn.int Dyn.string (k, v))) m;
  [%expect
    {|
    (1, "one")
    (2, "two")
    (3, "three")
    |}];
  ()
;;

let%expect_test "fold" =
  let m = Map.of_list (module Int) [ 1, "one"; 2, "two"; 3, "three" ] in
  let result = Map.fold (fun k _v acc -> acc + k) m 0 in
  print_dyn (result |> Dyn.int);
  [%expect {| 6 |}];
  (* Fold using both key and data to verify bindings are correct. *)
  let result2 = Map.fold (fun k v acc -> acc ^ Printf.sprintf "%d:%s " k v) m "" in
  print_dyn (result2 |> Dyn.string);
  [%expect {| "1:one 2:two 3:three " |}];
  ()
;;

let%expect_test "map" =
  let m = Map.of_list (module Int) [ 1, "one"; 2, "two" ] in
  let m2 = Map.map String.uppercase_ascii m in
  print_bindings m2;
  [%expect {| [ (1, "ONE"); (2, "TWO") ] |}];
  ()
;;

let%expect_test "mapi" =
  let m = Map.of_list (module Int) [ 1, "one"; 2, "two" ] in
  let m2 = Map.mapi (fun k v -> Printf.sprintf "%d:%s" k v) m in
  print_dyn (Map.bindings m2 |> Dyn.list (Dyn.pair Dyn.int Dyn.string));
  [%expect {| [ (1, "1:one"); (2, "2:two") ] |}];
  ()
;;

let%expect_test "filter" =
  let m = Map.of_list (module Int) [ 1, "one"; 2, "two"; 3, "three"; 4, "four" ] in
  let evens = Map.filter (fun k _v -> k mod 2 = 0) m in
  print_bindings evens;
  [%expect {| [ (2, "two"); (4, "four") ] |}];
  (* filter that keeps everything returns physically equal map. *)
  let m2 = Map.filter (fun _k _v -> true) m in
  require (phys_equal m m2);
  [%expect {||}];
  ()
;;

let%expect_test "filter_map" =
  let m = Map.of_list (module Int) [ 1, "one"; 2, "two"; 3, "three"; 4, "four" ] in
  let m2 =
    Map.filter_map
      (fun k v -> if k mod 2 = 0 then Some (String.uppercase_ascii v) else None)
      m
  in
  print_bindings m2;
  [%expect {| [ (2, "TWO"); (4, "FOUR") ] |}];
  ()
;;

let%expect_test "partition" =
  let m = Map.of_list (module Int) [ 1, "one"; 2, "two"; 3, "three"; 4, "four" ] in
  let evens, odds = Map.partition (fun k _v -> k mod 2 = 0) m in
  print_bindings evens;
  [%expect {| [ (2, "two"); (4, "four") ] |}];
  print_bindings odds;
  [%expect {| [ (1, "one"); (3, "three") ] |}];
  ()
;;

let%expect_test "split" =
  let m =
    Map.of_list (module Int) [ 1, "one"; 2, "two"; 3, "three"; 4, "four"; 5, "five" ]
  in
  let l, data, r = Map.split 3 m in
  print_bindings l;
  [%expect {| [ (1, "one"); (2, "two") ] |}];
  print_dyn (data |> Dyn.option Dyn.string);
  [%expect {| Some "three" |}];
  print_bindings r;
  [%expect {| [ (4, "four"); (5, "five") ] |}];
  (* Split on a missing key. *)
  let l2, data2, r2 = Map.split 6 m in
  print_bindings l2;
  [%expect {| [ (1, "one"); (2, "two"); (3, "three"); (4, "four"); (5, "five") ] |}];
  print_dyn (data2 |> Dyn.option Dyn.string);
  [%expect {| None |}];
  print_bindings r2;
  [%expect {| [] |}];
  ()
;;

let%expect_test "is_empty / is_singleton" =
  let e = Map.empty (module Int) in
  let m1 = Map.singleton (module Int) 1 "one" in
  let m2 = Map.of_list (module Int) [ 1, "one"; 2, "two" ] in
  print_dyn (Map.is_empty e |> Dyn.bool);
  [%expect {| true |}];
  print_dyn (Map.is_empty m1 |> Dyn.bool);
  [%expect {| false |}];
  print_dyn (Map.is_singleton e |> Dyn.bool);
  [%expect {| false |}];
  print_dyn (Map.is_singleton m1 |> Dyn.bool);
  [%expect {| true |}];
  print_dyn (Map.is_singleton m2 |> Dyn.bool);
  [%expect {| false |}];
  ()
;;

let%expect_test "to_seq / to_rev_seq" =
  let m = Map.of_list (module Int) [ 3, "three"; 1, "one"; 4, "four" ] in
  print_dyn (Map.to_seq m |> List.of_seq |> Dyn.list (Dyn.pair Dyn.int Dyn.string));
  [%expect {| [ (1, "one"); (3, "three"); (4, "four") ] |}];
  print_dyn (Map.to_rev_seq m |> List.of_seq |> Dyn.list (Dyn.pair Dyn.int Dyn.string));
  [%expect {| [ (4, "four"); (3, "three"); (1, "one") ] |}];
  ()
;;

let%expect_test "to_seq_from" =
  let m =
    Map.of_list (module Int) [ 1, "one"; 3, "three"; 5, "five"; 7, "seven"; 9, "nine" ]
  in
  print_dyn (Map.to_seq_from 4 m |> List.of_seq |> Dyn.list (Dyn.pair Dyn.int Dyn.string));
  [%expect {| [ (5, "five"); (7, "seven"); (9, "nine") ] |}];
  print_dyn (Map.to_seq_from 5 m |> List.of_seq |> Dyn.list (Dyn.pair Dyn.int Dyn.string));
  [%expect {| [ (5, "five"); (7, "seven"); (9, "nine") ] |}];
  print_dyn (Map.to_seq_from 10 m |> List.of_seq |> Dyn.list (Dyn.pair Dyn.int Dyn.string));
  [%expect {| [] |}];
  ()
;;

let%expect_test "add_seq" =
  let m = Map.of_list (module Int) [ 1, "one"; 2, "two" ] in
  let m2 = Map.add_seq (List.to_seq [ 3, "three"; 4, "four" ]) m in
  print_bindings m2;
  [%expect {| [ (1, "one"); (2, "two"); (3, "three"); (4, "four") ] |}];
  ()
;;

let%expect_test "of_seq" =
  let m = Map.of_seq (module Int) (List.to_seq [ 3, "three"; 1, "one"; 1, "ONE" ]) in
  print_bindings m;
  [%expect {| [ (1, "ONE"); (3, "three") ] |}];
  ()
;;

let%expect_test "incompatible compare raises" =
  let module Ord_rev = struct
    type t = int

    let compare t1 t2 = Int.compare t2 t1
  end
  in
  let m1 = Map.of_list (module Int) [ 1, "one" ] in
  (* Use multiple elements so Ord_rev.compare is exercised during of_list. *)
  let m2 = Map.of_list (module Ord_rev) [ 2, "two"; 3, "three"; 1, "one" ] in
  (* Verify Ord_rev ordering works (reverse order). *)
  print_dyn (Map.bindings m2 |> Dyn.list (Dyn.pair Dyn.int Dyn.string));
  [%expect {| [ (3, "three"); (2, "two"); (1, "one") ] |}];
  (* The closures below are never called: the exception is raised before the
     callback is invoked, so we use [assert false] to acknowledge this. *)
  require_does_raise (fun () ->
    Map.merge
      (fun (_k : int) _v1 _v2 : _ option ->
         (* exception raised before callback *)
         (assert false [@coverage off]))
      m1
      m2);
  [%expect {| Invalid_argument("Map.merge: maps have different compare functions.") |}];
  require_does_raise (fun () ->
    Map.union
      (fun (_ : int) (_ : string) (_ : string) : _ option ->
         (* exception raised before callback *)
         (assert false [@coverage off]))
      m1
      m2);
  [%expect {| Invalid_argument("Map.union: maps have different compare functions.") |}];
  require_does_raise (fun () -> Map.equal String.equal m1 m2);
  [%expect {| Invalid_argument("Map.equal: maps have different compare functions.") |}];
  require_does_raise (fun () -> Map.compare String.compare m1 m2);
  [%expect {| Invalid_argument("Map.compare: maps have different compare functions.") |}];
  ()
;;

let%expect_test "compare - different sizes" =
  let m1 = Map.of_list (module Int) [ 1, "one"; 2, "two" ] in
  let m2 = Map.of_list (module Int) [ 1, "one"; 2, "two"; 3, "three" ] in
  let m3 = Map.of_list (module Int) [ 1, "one" ] in
  (* m1 < m2 because m1 ends first. *)
  require (Map.compare String.compare m1 m2 < 0);
  [%expect {||}];
  (* m2 > m1. *)
  require (Map.compare String.compare m2 m1 > 0);
  [%expect {||}];
  (* m3 < m1. *)
  require (Map.compare String.compare m3 m1 < 0);
  [%expect {||}];
  (* Empty map < non-empty map. *)
  let e = Map.empty (module Int) in
  require (Map.compare String.compare e m1 < 0);
  [%expect {||}];
  require (Map.compare String.compare m1 e > 0);
  [%expect {||}];
  require (Map.compare String.compare e e = 0);
  [%expect {||}];
  ()
;;

let%expect_test "equal - different sizes" =
  let m1 = Map.of_list (module Int) [ 1, "one"; 2, "two" ] in
  let m2 = Map.of_list (module Int) [ 1, "one"; 2, "two"; 3, "three" ] in
  print_dyn (Map.equal String.equal m1 m2 |> Dyn.bool);
  [%expect {| false |}];
  print_dyn (Map.equal String.equal m2 m1 |> Dyn.bool);
  [%expect {| false |}];
  ()
;;

let%expect_test "union - with empty" =
  let m1 = Map.of_list (module Int) [ 1, "one"; 2, "two"; 3, "three" ] in
  let e = Map.empty (module Int) in
  (* Union with empty doesn't invoke the callback: one side is always empty,
     so the callback for overlapping keys is never reached. *)
  let unreachable (_ : int) (_ : string) (_ : string) : _ option =
    (* no overlapping keys with empty map *)
    (assert false [@coverage off])
  in
  (* Characterizing: union with empty returns physically equal map.
     Not documented, could change in future versions. *)
  let u1 = Map.union unreachable m1 e in
  require (phys_equal m1 u1);
  [%expect {||}];
  let u2 = Map.union unreachable e m1 in
  require (phys_equal m1 u2);
  [%expect {||}];
  (* Union where all keys overlap and f returns None removes them. *)
  let m2 = Map.of_list (module Int) [ 1, "a"; 2, "b" ] in
  let u3 = Map.union (fun _k _v1 _v2 -> None) m2 m2 in
  require (Map.is_empty u3);
  [%expect {||}];
  ()
;;

let%expect_test "merge - one empty" =
  let m1 = Map.of_list (module Int) [ 1, "one"; 2, "two" ] in
  let e = Map.empty (module Int) in
  (* When merging with empty, only Some+None or None+Some paths are hit.
     The Some+Some and None+None cases are unreachable in this context. *)
  let keep_present _key v1 v2 =
    match v1, v2 with
    | Some v, None | None, Some v -> Some v
    | Some _, Some _ | None, None ->
      (* unreachable: one side is always empty *)
      assert false [@coverage off]
  in
  let merged = Map.merge keep_present m1 e in
  print_bindings merged;
  [%expect {| [ (1, "one"); (2, "two") ] |}];
  let merged2 = Map.merge keep_present e m1 in
  print_bindings merged2;
  [%expect {| [ (1, "one"); (2, "two") ] |}];
  (* Merge that removes all elements via f returning None. *)
  let merged3 = Map.merge (fun _key _v1 _v2 -> None) m1 m1 in
  require (Map.is_empty merged3);
  [%expect {||}];
  ()
;;

let%expect_test "filter_map - removing elements" =
  let m = Map.of_list (module Int) [ 1, "one"; 2, "two"; 3, "three"; 4, "four" ] in
  let m2 =
    Map.filter_map
      (fun _k v -> if String.length v > 3 then Some (String.uppercase_ascii v) else None)
      m
  in
  print_bindings m2;
  [%expect {| [ (3, "THREE"); (4, "FOUR") ] |}];
  (* Filter_map that removes all. *)
  let m3 = Map.filter_map (fun _k _v -> None) m in
  require (Map.is_empty m3);
  [%expect {||}];
  ()
;;

let%expect_test "update - remove and no-op" =
  let m = Map.of_list (module Int) [ 1, "one"; 2, "two"; 3, "three" ] in
  (* Update on non-existent key returning None does not change the map. *)
  let m2 = Map.update 99 (fun _ -> None) m in
  require (phys_equal m m2);
  [%expect {||}];
  (* Update returning same data is physically equal. *)
  let m3 = Map.update 2 (fun v -> v) m in
  require (phys_equal m m3);
  [%expect {||}];
  ()
;;

let%expect_test "large map - triggers bal rotations" =
  let m =
    List.fold_left
      (fun acc i -> Map.add i (string_of_int i) acc)
      (Map.empty (module Int))
      (List.init 100 (fun i -> i))
  in
  print_dyn (Map.cardinal m |> Dyn.int);
  [%expect {| 100 |}];
  print_binding (Map.min_binding m);
  [%expect {| (0, "0") |}];
  print_binding (Map.max_binding m);
  [%expect {| (99, "99") |}];
  (* Remove many elements to trigger rebalancing. *)
  let m2 =
    List.fold_left (fun acc i -> Map.remove (i * 2) acc) m (List.init 50 (fun i -> i))
  in
  print_dyn (Map.cardinal m2 |> Dyn.int);
  [%expect {| 50 |}];
  ()
;;

let%expect_test "bal - descending insertion triggers right rotations" =
  (* Inserting in decreasing order exercises rebalancing. *)
  let m =
    List.fold_left
      (fun acc i -> Map.add i (string_of_int i) acc)
      (Map.empty (module Int))
      (List.rev (List.init 20 (fun i -> i)))
  in
  print_dyn (Map.cardinal m |> Dyn.int);
  [%expect {| 20 |}];
  print_bindings m;
  [%expect
    {|
    [ (0, "0")
    ; (1, "1")
    ; (2, "2")
    ; (3, "3")
    ; (4, "4")
    ; (5, "5")
    ; (6, "6")
    ; (7, "7")
    ; (8, "8")
    ; (9, "9")
    ; (10, "10")
    ; (11, "11")
    ; (12, "12")
    ; (13, "13")
    ; (14, "14")
    ; (15, "15")
    ; (16, "16")
    ; (17, "17")
    ; (18, "18")
    ; (19, "19")
    ]
    |}];
  ()
;;

let%expect_test "bal - zig-zag insertion triggers double rotations" =
  (* Zig-zag insertion exercises double rotations. *)
  let m = Map.empty (module Int) in
  let m = Map.add 10 "ten" m in
  let m = Map.add 3 "three" m in
  let m = Map.add 7 "seven" m in
  let m = Map.add 5 "five" m in
  print_bindings m;
  [%expect {| [ (3, "three"); (5, "five"); (7, "seven"); (10, "ten") ] |}];
  (* Same pattern in the other direction. *)
  let m2 = Map.empty (module Int) in
  let m2 = Map.add 1 "one" m2 in
  let m2 = Map.add 8 "eight" m2 in
  let m2 = Map.add 4 "four" m2 in
  let m2 = Map.add 6 "six" m2 in
  print_bindings m2;
  [%expect {| [ (1, "one"); (4, "four"); (6, "six"); (8, "eight") ] |}];
  (* Larger zig-zag sequence to exercise deeper double rotations. *)
  let m3 =
    List.fold_left
      (fun acc (k, v) -> Map.add k v acc)
      (Map.empty (module Int))
      [ 50, "50"
      ; 20, "20"
      ; 40, "40"
      ; 10, "10"
      ; 30, "30"
      ; 60, "60"
      ; 55, "55"
      ; 70, "70"
      ; 65, "65"
      ]
  in
  print_dyn (Map.cardinal m3 |> Dyn.int);
  [%expect {| 9 |}];
  print_bindings m3;
  [%expect
    {|
    [ (10, "10")
    ; (20, "20")
    ; (30, "30")
    ; (40, "40")
    ; (50, "50")
    ; (55, "55")
    ; (60, "60")
    ; (65, "65")
    ; (70, "70")
    ]
    |}];
  ()
;;

let%expect_test "remove - from left subtree and internal merge" =
  (* Remove elements from various positions to exercise rebalancing. *)
  let m =
    List.fold_left
      (fun acc (k, v) -> Map.add k v acc)
      (Map.empty (module Int))
      [ 4, "four"; 2, "two"; 6, "six"; 1, "one"; 3, "three"; 5, "five"; 7, "seven" ]
  in
  (* Remove from left subtree. *)
  let m2 = Map.remove 1 m in
  print_bindings m2;
  [%expect
    {|
    [ (2, "two")
    ; (3, "three")
    ; (4, "four")
    ; (5, "five")
    ; (6, "six")
    ; (7, "seven")
    ]
    |}];
  (* Remove a node with two children to exercise internal merge. *)
  let m3 = Map.remove 4 m in
  print_bindings m3;
  [%expect
    {|
    [ (1, "one")
    ; (2, "two")
    ; (3, "three")
    ; (5, "five")
    ; (6, "six")
    ; (7, "seven")
    ]
    |}];
  (* Remove another inner node with two children. *)
  let m4 = Map.remove 2 m in
  print_bindings m4;
  [%expect
    {|
    [ (1, "one")
    ; (3, "three")
    ; (4, "four")
    ; (5, "five")
    ; (6, "six")
    ; (7, "seven")
    ]
    |}];
  (* Remove a node to exercise merge with one empty subtree. *)
  let m5 =
    List.fold_left
      (fun acc (k, v) -> Map.add k v acc)
      (Map.empty (module Int))
      [ 2, "two"; 4, "four"; 1, "one"; 3, "three" ]
  in
  let m6 = Map.remove 4 m5 in
  print_bindings m6;
  [%expect {| [ (1, "one"); (2, "two"); (3, "three") ] |}];
  ()
;;

let%expect_test "update - in left subtree" =
  (* Exercise update on various subtree positions. *)
  let m = Map.of_list (module Int) [ 5, "five"; 3, "three"; 7, "seven" ] in
  let m2 = Map.update 3 (fun _ -> Some "THREE") m in
  print_bindings m2;
  [%expect {| [ (3, "THREE"); (5, "five"); (7, "seven") ] |}];
  (* Update that inserts into left subtree. *)
  let m3 = Map.update 1 (fun _ -> Some "one") m in
  print_bindings m3;
  [%expect {| [ (1, "one"); (3, "three"); (5, "five"); (7, "seven") ] |}];
  (* Update that removes from left subtree. *)
  let m4 = Map.update 3 (fun _ -> None) m in
  print_bindings m4;
  [%expect {| [ (5, "five"); (7, "seven") ] |}];
  ()
;;

let%expect_test "max_binding_opt - deeper tree" =
  let m =
    Map.of_list (module Int) [ 1, "one"; 2, "two"; 3, "three"; 4, "four"; 5, "five" ]
  in
  print_dyn (Map.max_binding_opt m |> Dyn.option (Dyn.pair Dyn.int Dyn.string));
  [%expect {| Some (5, "five") |}];
  ()
;;

let%expect_test "find_first / find_first_opt - deeper traversals" =
  (* Use a deeper tree to exercise deeper traversal. *)
  let m =
    Map.of_list (module Int) [ 2, "two"; 4, "four"; 6, "six"; 8, "eight"; 10, "ten" ]
  in
  (* Various predicates to exercise different traversal paths. *)
  print_binding (Map.find_first (fun k -> k >= 5) m);
  [%expect {| (6, "six") |}];
  print_dyn
    (Map.find_first_opt (fun k -> k >= 5) m |> Dyn.option (Dyn.pair Dyn.int Dyn.string));
  [%expect {| Some (6, "six") |}];
  print_binding (Map.find_first (fun k -> k >= 3) m);
  [%expect {| (4, "four") |}];
  print_dyn
    (Map.find_first_opt (fun k -> k >= 3) m |> Dyn.option (Dyn.pair Dyn.int Dyn.string));
  [%expect {| Some (4, "four") |}];
  ()
;;

let%expect_test "find_last / find_last_opt - deeper traversals" =
  let m =
    Map.of_list (module Int) [ 2, "two"; 4, "four"; 6, "six"; 8, "eight"; 10, "ten" ]
  in
  print_binding (Map.find_last (fun k -> k <= 7) m);
  [%expect {| (6, "six") |}];
  print_dyn
    (Map.find_last_opt (fun k -> k <= 7) m |> Dyn.option (Dyn.pair Dyn.int Dyn.string));
  [%expect {| Some (6, "six") |}];
  ()
;;

let%expect_test "exists - short-circuit" =
  let m = Map.of_list (module Int) [ 1, "one"; 2, "two"; 3, "three" ] in
  (* exists returns true when predicate matches on a non-root node. *)
  print_dyn (Map.exists (fun k _v -> k = 3) m |> Dyn.bool);
  [%expect {| true |}];
  print_dyn (Map.exists (fun k _v -> k = 1) m |> Dyn.bool);
  [%expect {| true |}];
  ()
;;

let%expect_test "union - second branch (h2 > h1)" =
  (* Union with a smaller first argument. *)
  let m1 = Map.of_list (module Int) [ 5, "five" ] in
  let m2 =
    Map.of_list
      (module Int)
      [ 1, "one"; 2, "two"; 3, "three"; 4, "four"; 6, "six"; 7, "seven" ]
  in
  let u =
    Map.union
      (fun (_ : int) (_ : string) (_ : string) : _ option ->
         (* no overlapping keys: callback is unreachable *)
         (assert false [@coverage off]))
      m1
      m2
  in
  print_bindings u;
  [%expect
    {|
    [ (1, "one")
    ; (2, "two")
    ; (3, "three")
    ; (4, "four")
    ; (5, "five")
    ; (6, "six")
    ; (7, "seven")
    ]
    |}];
  (* With overlapping key. *)
  let m3 = Map.of_list (module Int) [ 3, "THREE" ] in
  let u2 = Map.union (fun _k v1 v2 -> Some (v1 ^ "+" ^ v2)) m3 m2 in
  print_bindings u2;
  [%expect
    {|
    [ (1, "one")
    ; (2, "two")
    ; (3, "THREE+three")
    ; (4, "four")
    ; (6, "six")
    ; (7, "seven")
    ]
    |}];
  ()
;;

let%expect_test "compare - key difference" =
  (* Maps with different keys. *)
  let m1 = Map.of_list (module Int) [ 1, "one"; 2, "two" ] in
  let m2 = Map.of_list (module Int) [ 1, "one"; 3, "three" ] in
  require (Map.compare String.compare m1 m2 <> 0);
  [%expect {||}];
  (* Maps with same keys but different values. *)
  let m3 = Map.of_list (module Int) [ 1, "one"; 2, "aaa" ] in
  let m4 = Map.of_list (module Int) [ 1, "one"; 2, "zzz" ] in
  require (Map.compare String.compare m3 m4 < 0);
  [%expect {||}];
  ()
;;

let%expect_test "concat - with empty" =
  let m = Map.of_list (module Int) [ 1, "one"; 2, "two" ] in
  let e = Map.empty (module Int) in
  (* Exercise concat via filter that removes elements. *)
  let m2 = Map.filter (fun k _v -> k <> 2) m in
  print_bindings m2;
  [%expect {| [ (1, "one") ] |}];
  let m3 = Map.filter (fun k _v -> k <> 1) m in
  print_bindings m3;
  [%expect {| [ (2, "two") ] |}];
  (* filter_map that removes an element. *)
  let m4 = Map.of_list (module Int) [ 1, "one"; 2, "two"; 3, "three" ] in
  let m5 = Map.filter_map (fun k v -> if k = 2 then None else Some v) m4 in
  print_bindings m5;
  [%expect {| [ (1, "one"); (3, "three") ] |}];
  ignore e;
  ()
;;

let%expect_test "join - rebalancing paths" =
  (* Union of trees with very different sizes to exercise rebalancing. *)
  let big =
    List.fold_left
      (fun acc i -> Map.add i (string_of_int i) acc)
      (Map.empty (module Int))
      (List.init 30 (fun i -> i))
  in
  let small = Map.of_list (module Int) [ 100, "100"; 101, "101" ] in
  let u =
    Map.union
      (fun (_ : int) (_ : string) (_ : string) : _ option ->
         (* no overlapping keys *)
         (assert false [@coverage off]))
      big
      small
  in
  print_dyn (Map.cardinal u |> Dyn.int);
  [%expect {| 32 |}];
  (* Reverse: small union big to trigger the other join branch. *)
  let u2 =
    Map.union
      (fun (_ : int) (_ : string) (_ : string) : _ option ->
         (* no overlapping keys *)
         (assert false [@coverage off]))
      small
      big
  in
  print_dyn (Map.cardinal u2 |> Dyn.int);
  [%expect {| 32 |}];
  ()
;;

let%expect_test "to_seq_from - various positions" =
  let m =
    Map.of_list (module Int) [ 2, "two"; 4, "four"; 6, "six"; 8, "eight"; 10, "ten" ]
  in
  (* From a key less than all elements. *)
  print_dyn (Map.to_seq_from 1 m |> List.of_seq |> Dyn.list (Dyn.pair Dyn.int Dyn.string));
  [%expect {| [ (2, "two"); (4, "four"); (6, "six"); (8, "eight"); (10, "ten") ] |}];
  (* From an exact key. *)
  print_dyn (Map.to_seq_from 6 m |> List.of_seq |> Dyn.list (Dyn.pair Dyn.int Dyn.string));
  [%expect {| [ (6, "six"); (8, "eight"); (10, "ten") ] |}];
  ()
;;

let%expect_test "add_seq - physical equality" =
  let m = Map.of_list (module Int) [ 1, "one"; 2, "two"; 3, "three" ] in
  (* Characterizing: add_seq with empty sequence returns physically equal map.
     Not documented, could change in future versions. *)
  let m2 = Map.add_seq Seq.empty m in
  require (phys_equal m m2);
  [%expect {||}];
  (* Characterizing: add_seq with all-already-present bindings where values are
     physically equal returns physically equal map. This follows from [add]'s
     documented physical equality guarantee composed through [Seq.fold_left],
     but [add_seq] itself does not document it. *)
  let m3 = Map.add_seq (List.to_seq (Map.to_list m)) m in
  require (phys_equal m m3);
  [%expect {||}];
  ()
;;
