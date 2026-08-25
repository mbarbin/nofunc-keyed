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

let%expect_test "create / length / is_empty / shadow / find / find_exn / find_all / mem" =
  let tbl = Hashtbl.create (module Int) 16 in
  print_dyn (Hashtbl.is_empty tbl |> Dyn.bool);
  [%expect {| true |}];
  Hashtbl.shadow tbl ~key:1 ~data:"one";
  Hashtbl.shadow tbl ~key:1 ~data:"ONE";
  print_dyn (Hashtbl.length tbl |> Dyn.int);
  [%expect {| 2 |}];
  print_dyn (Hashtbl.is_empty tbl |> Dyn.bool);
  [%expect {| false |}];
  print_dyn (Hashtbl.find_exn tbl 1 |> Dyn.string);
  [%expect {| "ONE" |}];
  print_dyn (Hashtbl.find tbl 1 |> Dyn.option Dyn.string);
  [%expect {| Some "ONE" |}];
  print_dyn (Hashtbl.find_all tbl 1 |> Dyn.list Dyn.string);
  [%expect {| [ "ONE"; "one" ] |}];
  print_dyn (Hashtbl.mem tbl 1 |> Dyn.bool);
  [%expect {| true |}];
  ()
;;

let%expect_test "shadow: multiple bindings for the same key, retrieved with find_all" =
  let tbl = Hashtbl.create (module Int) 16 in
  Hashtbl.shadow tbl ~key:1 ~data:"first";
  Hashtbl.shadow tbl ~key:1 ~data:"second";
  Hashtbl.shadow tbl ~key:1 ~data:"third";
  (* Every [shadow] adds a new binding on top of the previous one rather
     than replacing it - [length] counts each of them separately. *)
  print_dyn (Hashtbl.length tbl |> Dyn.int);
  [%expect {| 3 |}];
  (* [find_all] returns every binding for [key], most recently shadowed
     first. *)
  print_dyn (Hashtbl.find_all tbl 1 |> Dyn.list Dyn.string);
  [%expect {| [ "third"; "second"; "first" ] |}];
  (* [find] (and [find_exn]) only ever see the most recent one. *)
  print_dyn (Hashtbl.find tbl 1 |> Dyn.option Dyn.string);
  [%expect {| Some "third" |}];
  (* [remove] pops the most recent binding, uncovering the one it was
     shadowing - it does not clear every binding for [key] at once. *)
  Hashtbl.remove tbl 1;
  print_dyn (Hashtbl.find_all tbl 1 |> Dyn.list Dyn.string);
  [%expect {| [ "second"; "first" ] |}];
  Hashtbl.remove tbl 1;
  print_dyn (Hashtbl.find_all tbl 1 |> Dyn.list Dyn.string);
  [%expect {| [ "first" ] |}];
  Hashtbl.remove tbl 1;
  print_dyn (Hashtbl.find_all tbl 1 |> Dyn.list Dyn.string);
  [%expect {| [] |}];
  print_dyn (Hashtbl.mem tbl 1 |> Dyn.bool);
  [%expect {| false |}];
  ()
;;

let%expect_test "remove_all: clears every shadowed binding for a key at once" =
  let tbl = Hashtbl.create (module Int) 16 in
  Hashtbl.shadow tbl ~key:1 ~data:"first";
  Hashtbl.shadow tbl ~key:1 ~data:"second";
  Hashtbl.shadow tbl ~key:1 ~data:"third";
  Hashtbl.set tbl ~key:2 ~data:"two";
  print_dyn (Hashtbl.length tbl |> Dyn.int);
  [%expect {| 4 |}];
  (* One call to [remove_all] does the work of three [remove]s, and leaves
     other keys untouched. *)
  Hashtbl.remove_all tbl 1;
  print_dyn (Hashtbl.find_all tbl 1 |> Dyn.list Dyn.string);
  [%expect {| [] |}];
  print_dyn (Hashtbl.mem tbl 1 |> Dyn.bool);
  [%expect {| false |}];
  print_bindings tbl;
  [%expect {| [ (2, "two") ] |}];
  (* Does nothing if [key] isn't bound. *)
  Hashtbl.remove_all tbl 1;
  print_dyn (Hashtbl.length tbl |> Dyn.int);
  [%expect {| 1 |}];
  ()
;;

(* [Collider] hashes every key to the same bucket (0), regardless of table
   capacity, while still distinguishing keys by [equal]. This exercises
   [remove_all]'s bucket-splicing against colliding, interleaved keys, rather
   than against keys that (with a well-behaved hash function) would likely
   land in distinct buckets. *)
module Collider = struct
  type t = int

  let equal = Int.equal
  let hash (_ : t) = 0
end

let%expect_test
    "remove_all: only removes the matching key from a bucket shared by colliding keys"
  =
  let tbl = Hashtbl.create (module Collider) 16 in
  (* Every key below hashes to 0, so they all land in the same bucket, in
     the order they were added, most recently added first:
     [2, "f"; 1, "e"; 3, "d"; 1, "c"; 2, "b"; 1, "a"]. Key [1]'s three
     bindings are scattered through the bucket, not adjacent to one another. *)
  Hashtbl.shadow tbl ~key:1 ~data:"a";
  Hashtbl.shadow tbl ~key:2 ~data:"b";
  Hashtbl.shadow tbl ~key:1 ~data:"c";
  Hashtbl.shadow tbl ~key:3 ~data:"d";
  Hashtbl.shadow tbl ~key:1 ~data:"e";
  Hashtbl.shadow tbl ~key:2 ~data:"f";
  print_dyn (Hashtbl.length tbl |> Dyn.int);
  [%expect {| 6 |}];
  Hashtbl.remove_all tbl 1;
  (* Every binding for [1] is gone... *)
  print_dyn (Hashtbl.find_all tbl 1 |> Dyn.list Dyn.string);
  [%expect {| [] |}];
  print_dyn (Hashtbl.mem tbl 1 |> Dyn.bool);
  [%expect {| false |}];
  (* ...while the interleaved bindings for [2] and [3], which merely share
     [1]'s bucket, are all still there, in their original relative order. *)
  print_dyn (Hashtbl.find_all tbl 2 |> Dyn.list Dyn.string);
  [%expect {| [ "f"; "b" ] |}];
  print_dyn (Hashtbl.find_all tbl 3 |> Dyn.list Dyn.string);
  [%expect {| [ "d" ] |}];
  print_dyn (Hashtbl.length tbl |> Dyn.int);
  [%expect {| 3 |}];
  ()
;;

let%expect_test "set / find_and_replace / find_and_remove / remove" =
  let tbl = Hashtbl.create (module Int) 16 in
  Hashtbl.set tbl ~key:1 ~data:"one";
  Hashtbl.set tbl ~key:2 ~data:"two";
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
  Hashtbl.set tbl ~key:1 ~data:"one";
  Hashtbl.set tbl ~key:2 ~data:"two";
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
  Hashtbl.set tbl ~key:1 ~data:"one";
  let tbl2 = Hashtbl.copy tbl in
  Hashtbl.set tbl2 ~key:2 ~data:"two";
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
  Hashtbl.set tbl ~key:1 ~data:"one";
  Hashtbl.set tbl ~key:2 ~data:"two";
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

let%expect_test "shadow_seq / set_seq / of_seq" =
  let tbl = Hashtbl.create (module Int) 16 in
  Hashtbl.shadow_seq tbl (List.to_seq [ 1, "one"; 2, "two" ]);
  print_bindings tbl;
  [%expect {| [ (1, "one"); (2, "two") ] |}];
  Hashtbl.set_seq tbl (List.to_seq [ 1, "ONE" ]);
  print_bindings tbl;
  [%expect {| [ (1, "ONE"); (2, "two") ] |}];
  let tbl2 = Hashtbl.of_seq (module Int) (List.to_seq [ 3, "three"; 4, "four" ]) in
  print_bindings tbl2;
  [%expect {| [ (3, "three"); (4, "four") ] |}];
  ()
;;

let%expect_test "create_seeded / of_seq_seeded" =
  let tbl = Hashtbl.create_seeded (module Int) 16 in
  Hashtbl.set tbl ~key:1 ~data:"one";
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
  Hashtbl.set tbl ~key:3 ~data:"three";
  Hashtbl.set tbl ~key:1 ~data:"one";
  Hashtbl.set tbl ~key:2 ~data:"two";
  (* Iteration order of a hash table is unspecified; [sexp_of_m__t] and
     [dyn_of_m__t] sort the bindings by [Key.compare] so that the output below
     is deterministic. *)
  print_s (Hashtbl.sexp_of_m__t (module Key) Sexplib0.Sexp_conv.sexp_of_string tbl);
  [%expect {| ((1 one) (2 two) (3 three)) |}];
  print_dyn (Hashtbl.dyn_of_m__t (module Key) Dyn.string tbl);
  [%expect {| map { 1 : "one"; 2 : "two"; 3 : "three" } |}];
  ()
;;
