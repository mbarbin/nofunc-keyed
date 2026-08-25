(***********************************************************************************)
(*  nofunc-keyed: Keyed data structures adapted from OCaml Stdlib but no functors  *)
(*  SPDX-FileCopyrightText: 2025 Mathieu Barbin <mathieu.barbin@gmail.com>         *)
(*  SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception   *)
(***********************************************************************************)

(* [Nofunc_map.Map] shares its tree implementation with [Nofunc_stdmap.Map],
   which is exercised thoroughly by the [stdmap] test suite. Here we only
   need to exercise each function of the wrapper itself, using an
   [Ordering.t]-returning [compare], as required by this package. *)

module Map = Nofunc_map.Map

module Int = struct
  type t = int

  let compare (a : int) (b : int) = Ordering.of_int (Stdlib.compare a b)
end

let print_binding (k, v) = print_dyn (Dyn.pair Dyn.int Dyn.string (k, v))
let print_bindings m = print_dyn (Map.bindings m |> Dyn.list (Dyn.pair Dyn.int Dyn.string))

let%expect_test "empty / is_empty / is_singleton / cardinal" =
  let e = Map.empty (module Int) in
  require (Map.is_empty e);
  [%expect {||}];
  print_dyn (Map.cardinal e |> Dyn.int);
  [%expect {| 0 |}];
  ()
;;

let%expect_test "singleton / is_singleton / bindings / to_list" =
  let m = Map.singleton (module Int) 1 "one" in
  require (Map.is_singleton m);
  [%expect {||}];
  print_bindings m;
  [%expect {| [ (1, "one") ] |}];
  print_dyn (Map.to_list m |> Dyn.list (Dyn.pair Dyn.int Dyn.string));
  [%expect {| [ (1, "one") ] |}];
  ()
;;

let%expect_test "add / update / remove" =
  let m = Map.empty (module Int) in
  let m = Map.add 1 "one" m in
  let m = Map.add 2 "two" m in
  (* Re-adding the same key with a physically equal value is a no-op. *)
  let two = "two" in
  let m1 = Map.add 2 two m in
  let m2 = Map.add 2 two m1 in
  require (phys_equal m1 m2);
  [%expect {||}];
  let m = Map.update 1 (fun v -> Option.map String.uppercase_ascii v) m in
  print_binding (1, Map.find 1 m);
  [%expect {| (1, "ONE") |}];
  let m = Map.remove 2 m in
  print_dyn (Map.mem 2 m |> Dyn.bool);
  [%expect {| false |}];
  ()
;;

let%expect_test "add_to_list" =
  let m = Map.empty (module Int) in
  let m = Map.add_to_list 3 "a" m in
  let m = Map.add_to_list 3 "b" m in
  print_dyn (Map.find 3 m |> Dyn.list Dyn.string);
  [%expect {| [ "b"; "a" ] |}];
  ()
;;

let%expect_test "of_list / mem / find / find_opt" =
  let m = Map.of_list (module Int) [ 3, "three"; 1, "one"; 2, "two"; 1, "ONE" ] in
  print_bindings m;
  [%expect {| [ (1, "ONE"); (2, "two"); (3, "three") ] |}];
  print_dyn (Map.mem 2 m |> Dyn.bool);
  [%expect {| true |}];
  print_dyn (Map.mem 4 m |> Dyn.bool);
  [%expect {| false |}];
  print_dyn (Map.find 2 m |> Dyn.string);
  [%expect {| "two" |}];
  require_does_raise (fun () -> Map.find 4 m);
  [%expect {| Not_found |}];
  print_dyn (Map.find_opt 4 m |> Dyn.option Dyn.string);
  [%expect {| None |}];
  ()
;;

let%expect_test "find_first / find_first_opt / find_last / find_last_opt" =
  let m = Map.of_list (module Int) [ 1, "one"; 2, "two"; 3, "three" ] in
  print_binding (Map.find_first (fun k -> k >= 2) m);
  [%expect {| (2, "two") |}];
  print_dyn
    (Map.find_first_opt (fun k -> k >= 99) m |> Dyn.option (Dyn.pair Dyn.int Dyn.string));
  [%expect {| None |}];
  print_binding (Map.find_last (fun k -> k <= 2) m);
  [%expect {| (2, "two") |}];
  print_dyn
    (Map.find_last_opt (fun k -> k <= 0) m |> Dyn.option (Dyn.pair Dyn.int Dyn.string));
  [%expect {| None |}];
  ()
;;

let%expect_test "min_binding / max_binding / choose" =
  let m = Map.of_list (module Int) [ 1, "one"; 2, "two"; 3, "three" ] in
  print_binding (Map.min_binding m);
  [%expect {| (1, "one") |}];
  print_dyn (Map.min_binding_opt m |> Dyn.option (Dyn.pair Dyn.int Dyn.string));
  [%expect {| Some (1, "one") |}];
  print_binding (Map.max_binding m);
  [%expect {| (3, "three") |}];
  print_dyn (Map.max_binding_opt m |> Dyn.option (Dyn.pair Dyn.int Dyn.string));
  [%expect {| Some (3, "three") |}];
  print_binding (Map.choose m);
  [%expect {| (1, "one") |}];
  print_dyn (Map.choose_opt m |> Dyn.option (Dyn.pair Dyn.int Dyn.string));
  [%expect {| Some (1, "one") |}];
  ()
;;

let%expect_test "iter / fold / for_all / exists" =
  let m = Map.of_list (module Int) [ 1, "one"; 2, "two" ] in
  let count = ref 0 in
  Map.iter (fun _ _ -> incr count) m;
  print_dyn (!count |> Dyn.int);
  [%expect {| 2 |}];
  let sum = Map.fold (fun k _ acc -> acc + k) m 0 in
  print_dyn (sum |> Dyn.int);
  [%expect {| 3 |}];
  print_dyn (Map.for_all (fun k _ -> k > 0) m |> Dyn.bool);
  [%expect {| true |}];
  print_dyn (Map.exists (fun k _ -> k > 1) m |> Dyn.bool);
  [%expect {| true |}];
  ()
;;

let%expect_test "map / mapi / filter / filter_map / partition" =
  let m = Map.of_list (module Int) [ 1, "one"; 2, "two"; 3, "three" ] in
  print_dyn
    (Map.map String.length m |> Map.bindings |> Dyn.list (Dyn.pair Dyn.int Dyn.int));
  [%expect {| [ (1, 3); (2, 3); (3, 5) ] |}];
  print_dyn
    (Map.mapi (fun k v -> k, v) m
     |> Map.bindings
     |> Dyn.list (Dyn.pair Dyn.int (Dyn.pair Dyn.int Dyn.string)));
  [%expect {| [ (1, (1, "one")); (2, (2, "two")); (3, (3, "three")) ] |}];
  print_dyn
    (Map.filter (fun k _ -> k <> 2) m
     |> Map.bindings
     |> Dyn.list (Dyn.pair Dyn.int Dyn.string));
  [%expect {| [ (1, "one"); (3, "three") ] |}];
  print_dyn
    (Map.filter_map (fun k v -> if k = 2 then None else Some (String.uppercase_ascii v)) m
     |> Map.bindings
     |> Dyn.list (Dyn.pair Dyn.int Dyn.string));
  [%expect {| [ (1, "ONE"); (3, "THREE") ] |}];
  let t, f = Map.partition (fun k _ -> k <= 1) m in
  print_dyn (Map.bindings t |> Dyn.list (Dyn.pair Dyn.int Dyn.string));
  [%expect {| [ (1, "one") ] |}];
  print_dyn (Map.bindings f |> Dyn.list (Dyn.pair Dyn.int Dyn.string));
  [%expect {| [ (2, "two"); (3, "three") ] |}];
  ()
;;

let%expect_test "split" =
  let m = Map.of_list (module Int) [ 1, "one"; 2, "two"; 3, "three" ] in
  let l, present, r = Map.split 2 m in
  print_dyn (Map.bindings l |> Dyn.list (Dyn.pair Dyn.int Dyn.string));
  [%expect {| [ (1, "one") ] |}];
  print_dyn (present |> Dyn.option Dyn.string);
  [%expect {| Some "two" |}];
  print_dyn (Map.bindings r |> Dyn.list (Dyn.pair Dyn.int Dyn.string));
  [%expect {| [ (3, "three") ] |}];
  ()
;;

let%expect_test "merge / union" =
  let m1 = Map.of_list (module Int) [ 1, "one"; 2, "two" ] in
  let m2 = Map.of_list (module Int) [ 2, "TWO"; 3, "three" ] in
  let merged =
    Map.merge
      (fun _ v1 v2 ->
         match v1, v2 with
         | Some v, None | None, Some v -> Some v
         | Some v1, Some v2 -> Some (v1 ^ "/" ^ v2)
         | None, None -> None [@coverage off])
      m1
      m2
  in
  print_bindings merged;
  [%expect {| [ (1, "one"); (2, "two/TWO"); (3, "three") ] |}];
  let union = Map.union (fun _ v1 _v2 -> Some v1) m1 m2 in
  print_bindings union;
  [%expect {| [ (1, "one"); (2, "two"); (3, "three") ] |}];
  (* Union with an empty map on either side returns the other map physically
     unchanged; the combining function below is never called in that case. *)
  let empty = Map.empty (module Int) in
  require (phys_equal m1 (Map.union (fun _ v1 _ -> (Some v1 [@coverage off])) m1 empty));
  [%expect {||}];
  require (phys_equal m1 (Map.union (fun _ v1 _ -> (Some v1 [@coverage off])) empty m1));
  [%expect {||}];
  ()
;;

let%expect_test "equal / compare" =
  let m1 = Map.of_list (module Int) [ 1, "one"; 2, "two" ] in
  let m2 = Map.of_list (module Int) [ 1, "one"; 2, "two" ] in
  let m3 = Map.of_list (module Int) [ 1, "one"; 2, "TWO" ] in
  print_dyn (Map.equal String.equal m1 m2 |> Dyn.bool);
  [%expect {| true |}];
  print_dyn (Map.equal String.equal m1 m3 |> Dyn.bool);
  [%expect {| false |}];
  print_dyn (Map.compare String.compare m1 m2 |> Dyn.int);
  [%expect {| 0 |}];
  ()
;;

let%expect_test "to_seq / to_rev_seq / to_seq_from / add_seq / of_seq" =
  let m = Map.of_list (module Int) [ 1, "one"; 2, "two"; 3, "three" ] in
  print_dyn (Map.to_seq m |> List.of_seq |> Dyn.list (Dyn.pair Dyn.int Dyn.string));
  [%expect {| [ (1, "one"); (2, "two"); (3, "three") ] |}];
  print_dyn (Map.to_rev_seq m |> List.of_seq |> Dyn.list (Dyn.pair Dyn.int Dyn.string));
  [%expect {| [ (3, "three"); (2, "two"); (1, "one") ] |}];
  print_dyn (Map.to_seq_from 2 m |> List.of_seq |> Dyn.list (Dyn.pair Dyn.int Dyn.string));
  [%expect {| [ (2, "two"); (3, "three") ] |}];
  let m2 = Map.add_seq (List.to_seq [ 4, "four" ]) m in
  print_bindings m2;
  [%expect {| [ (1, "one"); (2, "two"); (3, "three"); (4, "four") ] |}];
  let m3 = Map.of_seq (module Int) (List.to_seq [ 5, "five"; 6, "six" ]) in
  print_bindings m3;
  [%expect {| [ (5, "five"); (6, "six") ] |}];
  ()
;;

let%expect_test "different compare functions" =
  let module Int_rev = struct
    type t = int

    let compare a b = Ordering.reverse Int.compare a b
  end
  in
  let m1 = Map.of_list (module Int) [ 1, "one" ] in
  let m2 = Map.of_list (module Int_rev) [ 2, "two"; 1, "one" ] in
  require_does_raise (fun () ->
    Map.merge (fun (_ : int) _ _ : _ option -> (assert false [@coverage off])) m1 m2);
  [%expect {| Invalid_argument("Map.merge: maps have different compare functions.") |}];
  ()
;;
