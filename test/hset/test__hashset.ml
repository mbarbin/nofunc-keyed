(***********************************************************************************)
(*  nofunc-keyed: Keyed data structures adapted from OCaml Stdlib but no functors  *)
(*  SPDX-FileCopyrightText: 2025-2026 Mathieu Barbin <mathieu.barbin@gmail.com>    *)
(*  SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception   *)
(***********************************************************************************)

(* [Nofunc_hset.Hashset] is a thin wrapper around [Nofunc_htbl.Hashtbl], which
   is exercised thoroughly by the [stdhtbl] and [htbl] test suites. Here we
   only need to exercise each function of the wrapper itself. *)

module Hashset = Nofunc_hset.Hashset

let sorted_elements set =
  Hashset.fold set ~init:[] ~f:(fun ~key acc -> key :: acc) |> List.sort Int.compare
;;

let print_elements set = print_dyn (sorted_elements set |> Dyn.list Dyn.int)

let%expect_test "create / add / mem / remove / length" =
  let set = Hashset.create (module Int) 16 in
  Hashset.add set 1;
  Hashset.add set 2;
  (* [add] is idempotent. *)
  Hashset.add set 2;
  print_dyn (Hashset.length set |> Dyn.int);
  [%expect {| 2 |}];
  print_dyn (Hashset.mem set 2 |> Dyn.bool);
  [%expect {| true |}];
  Hashset.remove set 2;
  print_dyn (Hashset.mem set 2 |> Dyn.bool);
  [%expect {| false |}];
  ()
;;

let%expect_test "iter / fold / filter_inplace" =
  let set = Hashset.create (module Int) 16 in
  Hashset.add set 1;
  Hashset.add set 2;
  Hashset.add set 3;
  let count = ref 0 in
  Hashset.iter set ~f:(fun _ -> incr count);
  print_dyn (!count |> Dyn.int);
  [%expect {| 3 |}];
  let sum = Hashset.fold set ~init:0 ~f:(fun ~key acc -> acc + key) in
  print_dyn (sum |> Dyn.int);
  [%expect {| 6 |}];
  (* Exercise both outcomes of the filtering predicate. *)
  Hashset.filter_inplace set ~f:(fun x -> x mod 2 = 0);
  print_elements set;
  [%expect {| [ 2 ] |}];
  ()
;;

let%expect_test "clear / reset / copy" =
  let set = Hashset.create (module Int) 16 in
  Hashset.add set 1;
  let set2 = Hashset.copy set in
  Hashset.add set2 2;
  print_elements set;
  [%expect {| [ 1 ] |}];
  print_elements set2;
  [%expect {| [ 1; 2 ] |}];
  Hashset.clear set;
  print_dyn (Hashset.length set |> Dyn.int);
  [%expect {| 0 |}];
  Hashset.reset set2;
  print_dyn (Hashset.length set2 |> Dyn.int);
  [%expect {| 0 |}];
  ()
;;

let%expect_test "stats / to_seq / add_seq / of_seq" =
  let set = Hashset.create (module Int) 16 in
  Hashset.add_seq set (List.to_seq [ 1; 2; 2; 3 ]);
  let stats = Hashset.stats set in
  require (stats.num_buckets > 0);
  [%expect {||}];
  print_dyn (Hashset.to_seq set |> List.of_seq |> List.length |> Dyn.int);
  [%expect {| 3 |}];
  let set2 = Hashset.of_seq (module Int) (List.to_seq [ 4; 5 ]) in
  print_elements set2;
  [%expect {| [ 4; 5 ] |}];
  ()
;;

let%expect_test "create_seeded / of_seq_seeded" =
  let set = Hashset.create_seeded (module Int) 16 in
  Hashset.add set 1;
  print_elements set;
  [%expect {| [ 1 ] |}];
  let set2 = Hashset.of_seq_seeded (module Int) (List.to_seq [ 2; 3 ]) in
  print_elements set2;
  [%expect {| [ 2; 3 ] |}];
  ()
;;

module Key = struct
  include Int

  let compare a b = Ordering.of_int (Int.compare a b)
  let sexp_of_t = Sexplib0.Sexp_conv.sexp_of_int
end

let%expect_test "M / sexp_of_m__t / dyn_of_m__t" =
  (* [Hashset.M(Key).t] fixes the element type, with no remaining type
     parameter, as illustrated by this type annotation. *)
  let set : Hashset.M(Key).t = Hashset.create (module Key) 16 in
  Hashset.add set 3;
  Hashset.add set 1;
  Hashset.add set 2;
  (* Iteration order of a hash set is unspecified; [sexp_of_m__t] and
     [dyn_of_m__t] sort the elements by [Key.compare] so that the output below
     is deterministic. *)
  print_s (Hashset.sexp_of_m__t (module Key) set);
  [%expect {| (1 2 3) |}];
  print_dyn (Hashset.dyn_of_m__t (module Key) set);
  [%expect {| set { 1; 2; 3 } |}];
  ()
;;
