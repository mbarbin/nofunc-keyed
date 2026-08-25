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
  let m = Map.singleton (module Int) ~key:1 ~data:"one" in
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
  let m = Map.add m ~key:1 ~data:"one" in
  let m = Map.add m ~key:2 ~data:"two" in
  (* Re-adding the same key with a physically equal value is a no-op. *)
  let two = "two" in
  let m1 = Map.add m ~key:2 ~data:two in
  let m2 = Map.add m1 ~key:2 ~data:two in
  require (phys_equal m1 m2);
  [%expect {||}];
  let m = Map.update m ~key:1 ~f:(fun v -> Option.map String.uppercase_ascii v) in
  print_binding (1, Map.find m ~key:1);
  [%expect {| (1, "ONE") |}];
  let m = Map.remove m ~key:2 in
  print_dyn (Map.mem m ~key:2 |> Dyn.bool);
  [%expect {| false |}];
  ()
;;

let%expect_test "add_to_list" =
  let m = Map.empty (module Int) in
  let m = Map.add_to_list m ~key:3 ~data:"a" in
  let m = Map.add_to_list m ~key:3 ~data:"b" in
  print_dyn (Map.find m ~key:3 |> Dyn.list Dyn.string);
  [%expect {| [ "b"; "a" ] |}];
  ()
;;

let%expect_test "of_list / mem / find / find_opt" =
  let m = Map.of_list (module Int) [ 3, "three"; 1, "one"; 2, "two"; 1, "ONE" ] in
  print_bindings m;
  [%expect {| [ (1, "ONE"); (2, "two"); (3, "three") ] |}];
  print_dyn (Map.mem m ~key:2 |> Dyn.bool);
  [%expect {| true |}];
  print_dyn (Map.mem m ~key:4 |> Dyn.bool);
  [%expect {| false |}];
  print_dyn (Map.find m ~key:2 |> Dyn.string);
  [%expect {| "two" |}];
  require_does_raise (fun () -> Map.find m ~key:4);
  [%expect {| Not_found |}];
  print_dyn (Map.find_opt m ~key:4 |> Dyn.option Dyn.string);
  [%expect {| None |}];
  ()
;;

let%expect_test "find_first / find_first_opt / find_last / find_last_opt" =
  let m = Map.of_list (module Int) [ 1, "one"; 2, "two"; 3, "three" ] in
  print_binding (Map.find_first m ~f:(fun k -> k >= 2));
  [%expect {| (2, "two") |}];
  print_dyn
    (Map.find_first_opt m ~f:(fun k -> k >= 99)
     |> Dyn.option (Dyn.pair Dyn.int Dyn.string));
  [%expect {| None |}];
  print_binding (Map.find_last m ~f:(fun k -> k <= 2));
  [%expect {| (2, "two") |}];
  print_dyn
    (Map.find_last_opt m ~f:(fun k -> k <= 0) |> Dyn.option (Dyn.pair Dyn.int Dyn.string));
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
  Map.iter m ~f:(fun ~key:_ ~data:_ -> incr count);
  print_dyn (!count |> Dyn.int);
  [%expect {| 2 |}];
  let sum = Map.fold m 0 ~f:(fun ~key ~data:_ acc -> acc + key) in
  print_dyn (sum |> Dyn.int);
  [%expect {| 3 |}];
  print_dyn (Map.for_all m ~f:(fun k _ -> k > 0) |> Dyn.bool);
  [%expect {| true |}];
  print_dyn (Map.exists m ~f:(fun k _ -> k > 1) |> Dyn.bool);
  [%expect {| true |}];
  ()
;;

let%expect_test "map / mapi / filter / filter_map / partition" =
  let m = Map.of_list (module Int) [ 1, "one"; 2, "two"; 3, "three" ] in
  print_dyn
    (Map.map m ~f:String.length |> Map.bindings |> Dyn.list (Dyn.pair Dyn.int Dyn.int));
  [%expect {| [ (1, 3); (2, 3); (3, 5) ] |}];
  print_dyn
    (Map.mapi m ~f:(fun k v -> k, v)
     |> Map.bindings
     |> Dyn.list (Dyn.pair Dyn.int (Dyn.pair Dyn.int Dyn.string)));
  [%expect {| [ (1, (1, "one")); (2, (2, "two")); (3, (3, "three")) ] |}];
  print_dyn
    (Map.filter m ~f:(fun k _ -> k <> 2)
     |> Map.bindings
     |> Dyn.list (Dyn.pair Dyn.int Dyn.string));
  [%expect {| [ (1, "one"); (3, "three") ] |}];
  print_dyn
    (Map.filter_map m ~f:(fun k v ->
       if k = 2 then None else Some (String.uppercase_ascii v))
     |> Map.bindings
     |> Dyn.list (Dyn.pair Dyn.int Dyn.string));
  [%expect {| [ (1, "ONE"); (3, "THREE") ] |}];
  let t, f = Map.partition m ~f:(fun k _ -> k <= 1) in
  print_dyn (Map.bindings t |> Dyn.list (Dyn.pair Dyn.int Dyn.string));
  [%expect {| [ (1, "one") ] |}];
  print_dyn (Map.bindings f |> Dyn.list (Dyn.pair Dyn.int Dyn.string));
  [%expect {| [ (2, "two"); (3, "three") ] |}];
  ()
;;

let%expect_test "split" =
  let m = Map.of_list (module Int) [ 1, "one"; 2, "two"; 3, "three" ] in
  let l, present, r = Map.split m ~key:2 in
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
    Map.merge m1 m2 ~f:(fun _ v1 v2 ->
      match v1, v2 with
      | Some v, None | None, Some v -> Some v
      | Some v1, Some v2 -> Some (v1 ^ "/" ^ v2)
      | None, None -> None [@coverage off])
  in
  print_bindings merged;
  [%expect {| [ (1, "one"); (2, "two/TWO"); (3, "three") ] |}];
  let union = Map.union m1 m2 ~f:(fun _ v1 _v2 -> Some v1) in
  print_bindings union;
  [%expect {| [ (1, "one"); (2, "two"); (3, "three") ] |}];
  (* Union with an empty map on either side returns the other map physically
     unchanged; the combining function below is never called in that case. *)
  let empty = Map.empty (module Int) in
  require
    (phys_equal m1 (Map.union m1 empty ~f:(fun _ v1 _ -> (Some v1 [@coverage off]))));
  [%expect {||}];
  require
    (phys_equal m1 (Map.union empty m1 ~f:(fun _ v1 _ -> (Some v1 [@coverage off]))));
  [%expect {||}];
  ()
;;

let%expect_test "equal / compare" =
  let m1 = Map.of_list (module Int) [ 1, "one"; 2, "two" ] in
  let m2 = Map.of_list (module Int) [ 1, "one"; 2, "two" ] in
  let m3 = Map.of_list (module Int) [ 1, "one"; 2, "TWO" ] in
  print_dyn (Map.equal m1 m2 ~f:String.equal |> Dyn.bool);
  [%expect {| true |}];
  print_dyn (Map.equal m1 m3 ~f:String.equal |> Dyn.bool);
  [%expect {| false |}];
  print_dyn (Map.compare m1 m2 ~f:String.compare |> Dyn.int);
  [%expect {| 0 |}];
  ()
;;

let%expect_test "to_seq / to_rev_seq / to_seq_from / add_seq / of_seq" =
  let m = Map.of_list (module Int) [ 1, "one"; 2, "two"; 3, "three" ] in
  print_dyn (Map.to_seq m |> List.of_seq |> Dyn.list (Dyn.pair Dyn.int Dyn.string));
  [%expect {| [ (1, "one"); (2, "two"); (3, "three") ] |}];
  print_dyn (Map.to_rev_seq m |> List.of_seq |> Dyn.list (Dyn.pair Dyn.int Dyn.string));
  [%expect {| [ (3, "three"); (2, "two"); (1, "one") ] |}];
  print_dyn
    (Map.to_seq_from m ~key:2 |> List.of_seq |> Dyn.list (Dyn.pair Dyn.int Dyn.string));
  [%expect {| [ (2, "two"); (3, "three") ] |}];
  let m2 = Map.add_seq m (List.to_seq [ 4, "four" ]) in
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
    Map.merge m1 m2 ~f:(fun (_ : int) _ _ : _ option -> (assert false [@coverage off])));
  [%expect {| Invalid_argument("Map.merge: maps have different compare functions.") |}];
  ()
;;
