(***********************************************************************************)
(*  nofunc-keyed: Keyed data structures adapted from OCaml Stdlib but no functors  *)
(*  SPDX-FileCopyrightText: 2025 Mathieu Barbin <mathieu.barbin@gmail.com>         *)
(*  SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception   *)
(***********************************************************************************)

module Hashtbl = Nofunc_stdhtbl.Hashtbl

module Int_key = struct
  type t = int

  let equal = Int.equal
  let hash = Stdlib.Hashtbl.hash
end

let sorted_bindings tbl =
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) tbl []
  |> List.sort (fun (k1, _) (k2, _) -> Int.compare k1 k2)
;;

let print_bindings tbl =
  print_dyn (sorted_bindings tbl |> Dyn.list (Dyn.pair Dyn.int Dyn.string))
;;

let%expect_test "create / length" =
  let tbl = Hashtbl.create (module Int_key) 16 in
  print_dyn (Hashtbl.length tbl |> Dyn.int);
  [%expect {| 0 |}];
  ()
;;

let%expect_test "add / find / find_opt / mem" =
  let tbl = Hashtbl.create (module Int_key) 16 in
  Hashtbl.add tbl 1 "one";
  Hashtbl.add tbl 2 "two";
  Hashtbl.add tbl 3 "three";
  print_dyn (Hashtbl.length tbl |> Dyn.int);
  [%expect {| 3 |}];
  print_dyn (Hashtbl.find tbl 2 |> Dyn.string);
  [%expect {| "two" |}];
  require_does_raise (fun () -> Hashtbl.find tbl 4);
  [%expect {| Not_found |}];
  print_dyn (Hashtbl.find_opt tbl 2 |> Dyn.option Dyn.string);
  [%expect {| Some "two" |}];
  print_dyn (Hashtbl.find_opt tbl 4 |> Dyn.option Dyn.string);
  [%expect {| None |}];
  print_dyn (Hashtbl.mem tbl 1 |> Dyn.bool);
  [%expect {| true |}];
  print_dyn (Hashtbl.mem tbl 4 |> Dyn.bool);
  [%expect {| false |}];
  ()
;;

let%expect_test "add stacks bindings / find_all" =
  let tbl = Hashtbl.create (module Int_key) 16 in
  Hashtbl.add tbl 1 "first";
  Hashtbl.add tbl 1 "second";
  Hashtbl.add tbl 1 "third";
  print_dyn (Hashtbl.length tbl |> Dyn.int);
  [%expect {| 3 |}];
  (* find returns the most recent binding. *)
  print_dyn (Hashtbl.find tbl 1 |> Dyn.string);
  [%expect {| "third" |}];
  (* find_all returns all bindings, most recent first. *)
  print_dyn (Hashtbl.find_all tbl 1 |> Dyn.list Dyn.string);
  [%expect {| [ "third"; "second"; "first" ] |}];
  ()
;;

let%expect_test "replace" =
  let tbl = Hashtbl.create (module Int_key) 16 in
  Hashtbl.replace tbl 1 "one";
  Hashtbl.replace tbl 2 "two";
  print_bindings tbl;
  [%expect {| [ (1, "one"); (2, "two") ] |}];
  (* replace overwrites existing binding. *)
  Hashtbl.replace tbl 1 "ONE";
  print_bindings tbl;
  [%expect {| [ (1, "ONE"); (2, "two") ] |}];
  print_dyn (Hashtbl.length tbl |> Dyn.int);
  [%expect {| 2 |}];
  ()
;;

let%expect_test "remove" =
  let tbl = Hashtbl.create (module Int_key) 16 in
  Hashtbl.replace tbl 1 "one";
  Hashtbl.replace tbl 2 "two";
  Hashtbl.replace tbl 3 "three";
  Hashtbl.remove tbl 2;
  print_bindings tbl;
  [%expect {| [ (1, "one"); (3, "three") ] |}];
  (* Removing non-existent key does nothing. *)
  Hashtbl.remove tbl 99;
  print_bindings tbl;
  [%expect {| [ (1, "one"); (3, "three") ] |}];
  ()
;;

let%expect_test "remove restores previous binding" =
  let tbl = Hashtbl.create (module Int_key) 16 in
  Hashtbl.add tbl 1 "first";
  Hashtbl.add tbl 1 "second";
  print_dyn (Hashtbl.find tbl 1 |> Dyn.string);
  [%expect {| "second" |}];
  Hashtbl.remove tbl 1;
  print_dyn (Hashtbl.find tbl 1 |> Dyn.string);
  [%expect {| "first" |}];
  ()
;;

let%expect_test "find_and_remove" =
  let tbl = Hashtbl.create (module Int_key) 16 in
  Hashtbl.replace tbl 1 "one";
  Hashtbl.replace tbl 2 "two";
  print_dyn (Hashtbl.find_and_remove tbl 1 |> Dyn.option Dyn.string);
  [%expect {| Some "one" |}];
  print_dyn (Hashtbl.find_and_remove tbl 1 |> Dyn.option Dyn.string);
  [%expect {| None |}];
  print_dyn (Hashtbl.length tbl |> Dyn.int);
  [%expect {| 1 |}];
  ()
;;

let%expect_test "find_and_replace" =
  let tbl = Hashtbl.create (module Int_key) 16 in
  Hashtbl.replace tbl 1 "one";
  print_dyn (Hashtbl.find_and_replace tbl 1 "ONE" |> Dyn.option Dyn.string);
  [%expect {| Some "one" |}];
  print_dyn (Hashtbl.find tbl 1 |> Dyn.string);
  [%expect {| "ONE" |}];
  (* New key: returns None. *)
  print_dyn (Hashtbl.find_and_replace tbl 2 "two" |> Dyn.option Dyn.string);
  [%expect {| None |}];
  print_dyn (Hashtbl.find tbl 2 |> Dyn.string);
  [%expect {| "two" |}];
  ()
;;

let%expect_test "clear" =
  let tbl = Hashtbl.create (module Int_key) 16 in
  Hashtbl.replace tbl 1 "one";
  Hashtbl.replace tbl 2 "two";
  Hashtbl.clear tbl;
  print_dyn (Hashtbl.length tbl |> Dyn.int);
  [%expect {| 0 |}];
  print_dyn (Hashtbl.mem tbl 1 |> Dyn.bool);
  [%expect {| false |}];
  ()
;;

let%expect_test "reset" =
  let tbl = Hashtbl.create (module Int_key) 16 in
  Hashtbl.replace tbl 1 "one";
  Hashtbl.replace tbl 2 "two";
  Hashtbl.reset tbl;
  print_dyn (Hashtbl.length tbl |> Dyn.int);
  [%expect {| 0 |}];
  print_dyn (Hashtbl.mem tbl 1 |> Dyn.bool);
  [%expect {| false |}];
  ()
;;

let%expect_test "copy" =
  let tbl = Hashtbl.create (module Int_key) 16 in
  Hashtbl.replace tbl 1 "one";
  Hashtbl.replace tbl 2 "two";
  let tbl2 = Hashtbl.copy tbl in
  (* Modifying the copy does not affect the original. *)
  Hashtbl.replace tbl2 1 "ONE";
  Hashtbl.replace tbl2 3 "three";
  print_bindings tbl;
  [%expect {| [ (1, "one"); (2, "two") ] |}];
  print_bindings tbl2;
  [%expect {| [ (1, "ONE"); (2, "two"); (3, "three") ] |}];
  ()
;;

let%expect_test "iter" =
  let tbl = Hashtbl.create (module Int_key) 16 in
  Hashtbl.replace tbl 1 "one";
  Hashtbl.replace tbl 2 "two";
  Hashtbl.replace tbl 3 "three";
  let pairs = ref [] in
  Hashtbl.iter (fun k v -> pairs := (k, v) :: !pairs) tbl;
  let sorted = List.sort (fun (k1, _) (k2, _) -> Int.compare k1 k2) !pairs in
  print_dyn (sorted |> Dyn.list (Dyn.pair Dyn.int Dyn.string));
  [%expect {| [ (1, "one"); (2, "two"); (3, "three") ] |}];
  ()
;;

let%expect_test "fold" =
  let tbl = Hashtbl.create (module Int_key) 16 in
  Hashtbl.replace tbl 1 "one";
  Hashtbl.replace tbl 2 "two";
  Hashtbl.replace tbl 3 "three";
  let sum = Hashtbl.fold (fun k _v acc -> acc + k) tbl 0 in
  print_dyn (sum |> Dyn.int);
  [%expect {| 6 |}];
  ()
;;

let%expect_test "filter_map_inplace" =
  let tbl = Hashtbl.create (module Int_key) 16 in
  Hashtbl.replace tbl 1 "one";
  Hashtbl.replace tbl 2 "two";
  Hashtbl.replace tbl 3 "three";
  Hashtbl.replace tbl 4 "four";
  Hashtbl.filter_map_inplace
    (fun k v -> if k mod 2 = 0 then Some (String.uppercase_ascii v) else None)
    tbl;
  print_bindings tbl;
  [%expect {| [ (2, "TWO"); (4, "FOUR") ] |}];
  ()
;;

let%expect_test "to_seq" =
  let tbl = Hashtbl.create (module Int_key) 16 in
  Hashtbl.replace tbl 1 "one";
  Hashtbl.replace tbl 2 "two";
  let sorted =
    Hashtbl.to_seq tbl
    |> List.of_seq
    |> List.sort (fun (k1, _) (k2, _) -> Int.compare k1 k2)
  in
  print_dyn (sorted |> Dyn.list (Dyn.pair Dyn.int Dyn.string));
  [%expect {| [ (1, "one"); (2, "two") ] |}];
  ()
;;

let%expect_test "to_seq_keys / to_seq_values" =
  let tbl = Hashtbl.create (module Int_key) 16 in
  Hashtbl.replace tbl 3 "three";
  Hashtbl.replace tbl 1 "one";
  Hashtbl.replace tbl 2 "two";
  let keys = Hashtbl.to_seq_keys tbl |> List.of_seq |> List.sort Int.compare in
  print_dyn (keys |> Dyn.list Dyn.int);
  [%expect {| [ 1; 2; 3 ] |}];
  let values = Hashtbl.to_seq_values tbl |> List.of_seq |> List.sort String.compare in
  print_dyn (values |> Dyn.list Dyn.string);
  [%expect {| [ "one"; "three"; "two" ] |}];
  ()
;;

let%expect_test "add_seq" =
  let tbl = Hashtbl.create (module Int_key) 16 in
  Hashtbl.add_seq tbl (List.to_seq [ 1, "one"; 2, "two"; 3, "three" ]);
  print_bindings tbl;
  [%expect {| [ (1, "one"); (2, "two"); (3, "three") ] |}];
  ()
;;

let%expect_test "replace_seq" =
  let tbl = Hashtbl.create (module Int_key) 16 in
  Hashtbl.replace tbl 1 "old";
  Hashtbl.replace_seq tbl (List.to_seq [ 1, "new"; 2, "two" ]);
  print_bindings tbl;
  [%expect {| [ (1, "new"); (2, "two") ] |}];
  ()
;;

let%expect_test "of_seq" =
  let tbl =
    Hashtbl.of_seq (module Int_key) (List.to_seq [ 1, "one"; 2, "two"; 1, "ONE" ])
  in
  print_bindings tbl;
  [%expect {| [ (1, "ONE"); (2, "two") ] |}];
  ()
;;

let%expect_test "stats" =
  let tbl = Hashtbl.create (module Int_key) 16 in
  Hashtbl.replace tbl 1 "one";
  Hashtbl.replace tbl 2 "two";
  let stats = Hashtbl.stats tbl in
  print_dyn (stats.num_bindings |> Dyn.int);
  [%expect {| 2 |}];
  require (stats.num_buckets > 0);
  [%expect {||}];
  ()
;;
