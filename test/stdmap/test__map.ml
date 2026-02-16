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
  let m2 =
    Map.update
      1
      (function
        | Some v -> Some (String.uppercase_ascii v)
        | None -> None)
      m
  in
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
  let merged =
    Map.merge
      (fun _key v1 v2 ->
         match v1, v2 with
         | Some v1, Some v2 -> Some (v1 ^ "+" ^ v2)
         | Some v, None | None, Some v -> Some v
         | None, None -> None)
      m1
      m2
  in
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
  ()
;;

let%expect_test "subset" =
  let m1 = Map.of_list (module Int) [ 1, "one" ] in
  let m2 = Map.of_list (module Int) [ 1, "one"; 2, "two" ] in
  print_dyn (Map.is_empty m1 |> Dyn.bool);
  [%expect {| false |}];
  print_dyn (Map.is_singleton m1 |> Dyn.bool);
  [%expect {| true |}];
  print_dyn (Map.is_singleton m2 |> Dyn.bool);
  [%expect {| false |}];
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
  let m2 = Map.of_list (module Ord_rev) [ 2, "two" ] in
  require_does_raise (fun () -> Map.merge (fun _k _v1 _v2 -> None) m1 m2);
  [%expect {| Invalid_argument("Map.merge: maps have different compare functions.") |}];
  require_does_raise (fun () -> Map.union (fun _k v1 _v2 -> Some v1) m1 m2);
  [%expect {| Invalid_argument("Map.union: maps have different compare functions.") |}];
  require_does_raise (fun () -> Map.equal String.equal m1 m2);
  [%expect {| Invalid_argument("Map.equal: maps have different compare functions.") |}];
  require_does_raise (fun () -> Map.compare String.compare m1 m2);
  [%expect {| Invalid_argument("Map.compare: maps have different compare functions.") |}];
  ()
;;
