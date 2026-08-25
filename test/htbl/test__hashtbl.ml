(***********************************************************************************)
(*  nofunc-keyed: Keyed data structures adapted from OCaml Stdlib but no functors  *)
(*  SPDX-FileCopyrightText: 2025-2026 Mathieu Barbin <mathieu.barbin@gmail.com>    *)
(*  SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception   *)
(***********************************************************************************)

(* [Nofunc_htbl.Hashtbl] is a thin wrapper around [Nofunc_htbl.stdlib]'s
   [Hashtbl0], which is exercised thoroughly by the [stdhtbl] test suite (the
   same building block is shared by both packages). Here we only need to
   exercise each function of the wrapper itself. *)

module Hashtbl = Nofunc_htbl.Hashtbl

let sorted_bindings tbl =
  Hashtbl.fold tbl ~init:[] ~f:(fun ~key ~data acc -> (key, data) :: acc)
  |> List.sort (fun (k1, _) (k2, _) -> Int.compare k1 k2)
;;

let print_bindings tbl =
  print_dyn (sorted_bindings tbl |> Dyn.list (Dyn.pair Dyn.int Dyn.string))
;;

let%expect_test "create / length / add / find / find_opt / find_all / mem" =
  let tbl = Hashtbl.create (module Int) 16 in
  Hashtbl.add tbl ~key:1 ~data:"one";
  Hashtbl.add tbl ~key:1 ~data:"ONE";
  print_dyn (Hashtbl.length tbl |> Dyn.int);
  [%expect {| 2 |}];
  print_dyn (Hashtbl.find tbl 1 |> Dyn.string);
  [%expect {| "ONE" |}];
  print_dyn (Hashtbl.find_opt tbl 1 |> Dyn.option Dyn.string);
  [%expect {| Some "ONE" |}];
  print_dyn (Hashtbl.find_all tbl 1 |> Dyn.list Dyn.string);
  [%expect {| [ "ONE"; "one" ] |}];
  print_dyn (Hashtbl.mem tbl 1 |> Dyn.bool);
  [%expect {| true |}];
  ()
;;

let%expect_test "replace / find_and_replace / find_and_remove / remove" =
  let tbl = Hashtbl.create (module Int) 16 in
  Hashtbl.replace tbl ~key:1 ~data:"one";
  Hashtbl.replace tbl ~key:2 ~data:"two";
  print_dyn (Hashtbl.find_and_replace tbl ~key:1 ~data:"ONE" |> Dyn.option Dyn.string);
  [%expect {| Some "one" |}];
  print_dyn (Hashtbl.find_and_remove tbl 2 |> Dyn.option Dyn.string);
  [%expect {| Some "two" |}];
  Hashtbl.remove tbl 1;
  print_dyn (Hashtbl.length tbl |> Dyn.int);
  [%expect {| 0 |}];
  ()
;;

let%expect_test "iter / fold / filter_map_inplace" =
  let tbl = Hashtbl.create (module Int) 16 in
  Hashtbl.replace tbl ~key:1 ~data:"one";
  Hashtbl.replace tbl ~key:2 ~data:"two";
  let count = ref 0 in
  Hashtbl.iter tbl ~f:(fun ~key:_ ~data:_ -> incr count);
  print_dyn (!count |> Dyn.int);
  [%expect {| 2 |}];
  let sum = Hashtbl.fold tbl ~init:0 ~f:(fun ~key ~data:_ acc -> acc + key) in
  print_dyn (sum |> Dyn.int);
  [%expect {| 3 |}];
  Hashtbl.filter_map_inplace tbl ~f:(fun ~key ~data ->
    if key = 1 then Some data else None);
  print_bindings tbl;
  [%expect {| [ (1, "one") ] |}];
  ()
;;

let%expect_test "clear / reset / copy" =
  let tbl = Hashtbl.create (module Int) 16 in
  Hashtbl.replace tbl ~key:1 ~data:"one";
  let tbl2 = Hashtbl.copy tbl in
  Hashtbl.replace tbl2 ~key:2 ~data:"two";
  print_dyn (Hashtbl.length tbl |> Dyn.int);
  [%expect {| 1 |}];
  print_dyn (Hashtbl.length tbl2 |> Dyn.int);
  [%expect {| 2 |}];
  Hashtbl.clear tbl;
  print_dyn (Hashtbl.length tbl |> Dyn.int);
  [%expect {| 0 |}];
  Hashtbl.reset tbl2;
  print_dyn (Hashtbl.length tbl2 |> Dyn.int);
  [%expect {| 0 |}];
  ()
;;

let%expect_test "stats / to_seq / to_seq_keys / to_seq_values" =
  let tbl = Hashtbl.create (module Int) 16 in
  Hashtbl.replace tbl ~key:1 ~data:"one";
  Hashtbl.replace tbl ~key:2 ~data:"two";
  let stats = Hashtbl.stats tbl in
  require (stats.num_buckets > 0);
  [%expect {||}];
  let sorted_keys s = s |> List.of_seq |> List.sort Int.compare in
  print_dyn (Hashtbl.to_seq tbl |> List.of_seq |> List.length |> Dyn.int);
  [%expect {| 2 |}];
  print_dyn (Hashtbl.to_seq_keys tbl |> sorted_keys |> Dyn.list Dyn.int);
  [%expect {| [ 1; 2 ] |}];
  print_dyn
    (Hashtbl.to_seq_values tbl
     |> List.of_seq
     |> List.sort String.compare
     |> Dyn.list Dyn.string);
  [%expect {| [ "one"; "two" ] |}];
  ()
;;

let%expect_test "add_seq / replace_seq / of_seq" =
  let tbl = Hashtbl.create (module Int) 16 in
  Hashtbl.add_seq tbl (List.to_seq [ 1, "one"; 2, "two" ]);
  print_bindings tbl;
  [%expect {| [ (1, "one"); (2, "two") ] |}];
  Hashtbl.replace_seq tbl (List.to_seq [ 1, "ONE" ]);
  print_bindings tbl;
  [%expect {| [ (1, "ONE"); (2, "two") ] |}];
  let tbl2 = Hashtbl.of_seq (module Int) (List.to_seq [ 3, "three"; 4, "four" ]) in
  print_bindings tbl2;
  [%expect {| [ (3, "three"); (4, "four") ] |}];
  ()
;;

let%expect_test "create_seeded / of_seq_seeded" =
  let tbl = Hashtbl.create_seeded (module Int) 16 in
  Hashtbl.replace tbl ~key:1 ~data:"one";
  print_bindings tbl;
  [%expect {| [ (1, "one") ] |}];
  let tbl2 = Hashtbl.of_seq_seeded (module Int) (List.to_seq [ 2, "two"; 3, "three" ]) in
  print_bindings tbl2;
  [%expect {| [ (2, "two"); (3, "three") ] |}];
  ()
;;

module Key = struct
  include Int

  let compare a b = Ordering.of_int (Int.compare a b)
  let sexp_of_t = Sexplib0.Sexp_conv.sexp_of_int
end

let%expect_test "M / sexp_of_m__t / dyn_of_m__t" =
  (* [Hashtbl.M(Key).t] fixes the key type, leaving only the data type as a
     parameter, as illustrated by this type annotation. *)
  let tbl : string Hashtbl.M(Key).t = Hashtbl.create (module Key) 16 in
  Hashtbl.replace tbl ~key:3 ~data:"three";
  Hashtbl.replace tbl ~key:1 ~data:"one";
  Hashtbl.replace tbl ~key:2 ~data:"two";
  (* Iteration order of a hash table is unspecified; [sexp_of_m__t] and
     [dyn_of_m__t] sort the bindings by [Key.compare] so that the output below
     is deterministic. *)
  print_s (Hashtbl.sexp_of_m__t (module Key) Sexplib0.Sexp_conv.sexp_of_string tbl);
  [%expect {| ((1 one) (2 two) (3 three)) |}];
  print_dyn (Hashtbl.dyn_of_m__t (module Key) Dyn.string tbl);
  [%expect {| map { 1 : "one"; 2 : "two"; 3 : "three" } |}];
  ()
;;
