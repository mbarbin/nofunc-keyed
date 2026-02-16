(***********************************************************************************)
(*  nofunc-keyed: Keyed data structures adapted from OCaml Stdlib but no functors  *)
(*  SPDX-FileCopyrightText: 2025 Mathieu Barbin <mathieu.barbin@gmail.com>         *)
(*  SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception   *)
(***********************************************************************************)

module Hashset = Nofunc_stdhset.Hashset

module Int_elt = struct
  type t = int

  let equal = Int.equal
  let hash = Stdlib.Hashtbl.hash
end

let sorted_elements set =
  Hashset.fold (fun x acc -> x :: acc) set [] |> List.sort Int.compare
;;

let print_elements set = print_dyn (sorted_elements set |> Dyn.list Dyn.int)

let%expect_test "create / length" =
  let set = Hashset.create (module Int_elt) 16 in
  print_dyn (Hashset.length set |> Dyn.int);
  [%expect {| 0 |}];
  ()
;;

let%expect_test "add / mem / length" =
  let set = Hashset.create (module Int_elt) 16 in
  Hashset.add set 1;
  Hashset.add set 2;
  Hashset.add set 3;
  print_dyn (Hashset.length set |> Dyn.int);
  [%expect {| 3 |}];
  print_dyn (Hashset.mem set 2 |> Dyn.bool);
  [%expect {| true |}];
  print_dyn (Hashset.mem set 4 |> Dyn.bool);
  [%expect {| false |}];
  print_elements set;
  [%expect {| [ 1; 2; 3 ] |}];
  ()
;;

let%expect_test "add is idempotent" =
  let set = Hashset.create (module Int_elt) 16 in
  Hashset.add set 1;
  Hashset.add set 1;
  Hashset.add set 1;
  print_dyn (Hashset.length set |> Dyn.int);
  [%expect {| 1 |}];
  ()
;;

let%expect_test "remove" =
  let set = Hashset.create (module Int_elt) 16 in
  Hashset.add set 1;
  Hashset.add set 2;
  Hashset.add set 3;
  Hashset.remove set 2;
  print_elements set;
  [%expect {| [ 1; 3 ] |}];
  (* Removing non-existent element does nothing. *)
  Hashset.remove set 99;
  print_elements set;
  [%expect {| [ 1; 3 ] |}];
  ()
;;

let%expect_test "clear" =
  let set = Hashset.create (module Int_elt) 16 in
  Hashset.add set 1;
  Hashset.add set 2;
  Hashset.clear set;
  print_dyn (Hashset.length set |> Dyn.int);
  [%expect {| 0 |}];
  print_dyn (Hashset.mem set 1 |> Dyn.bool);
  [%expect {| false |}];
  ()
;;

let%expect_test "reset" =
  let set = Hashset.create (module Int_elt) 16 in
  Hashset.add set 1;
  Hashset.add set 2;
  Hashset.reset set;
  print_dyn (Hashset.length set |> Dyn.int);
  [%expect {| 0 |}];
  print_dyn (Hashset.mem set 1 |> Dyn.bool);
  [%expect {| false |}];
  ()
;;

let%expect_test "copy" =
  let set = Hashset.create (module Int_elt) 16 in
  Hashset.add set 1;
  Hashset.add set 2;
  let set2 = Hashset.copy set in
  (* Modifying the copy does not affect the original. *)
  Hashset.add set2 3;
  Hashset.remove set2 1;
  print_elements set;
  [%expect {| [ 1; 2 ] |}];
  print_elements set2;
  [%expect {| [ 2; 3 ] |}];
  ()
;;

let%expect_test "iter" =
  let set = Hashset.create (module Int_elt) 16 in
  Hashset.add set 3;
  Hashset.add set 1;
  Hashset.add set 2;
  let elts = ref [] in
  Hashset.iter (fun x -> elts := x :: !elts) set;
  let sorted = List.sort Int.compare !elts in
  print_dyn (sorted |> Dyn.list Dyn.int);
  [%expect {| [ 1; 2; 3 ] |}];
  ()
;;

let%expect_test "fold" =
  let set = Hashset.create (module Int_elt) 16 in
  Hashset.add set 1;
  Hashset.add set 2;
  Hashset.add set 3;
  let sum = Hashset.fold (fun x acc -> acc + x) set 0 in
  print_dyn (sum |> Dyn.int);
  [%expect {| 6 |}];
  ()
;;

let%expect_test "filter_inplace" =
  let set = Hashset.create (module Int_elt) 16 in
  Hashset.add set 1;
  Hashset.add set 2;
  Hashset.add set 3;
  Hashset.add set 4;
  Hashset.add set 5;
  Hashset.add set 6;
  Hashset.filter_inplace (fun x -> x mod 2 = 0) set;
  print_elements set;
  [%expect {| [ 2; 4; 6 ] |}];
  ()
;;

let%expect_test "to_seq" =
  let set = Hashset.create (module Int_elt) 16 in
  Hashset.add set 3;
  Hashset.add set 1;
  Hashset.add set 2;
  let sorted = Hashset.to_seq set |> List.of_seq |> List.sort Int.compare in
  print_dyn (sorted |> Dyn.list Dyn.int);
  [%expect {| [ 1; 2; 3 ] |}];
  ()
;;

let%expect_test "add_seq" =
  let set = Hashset.create (module Int_elt) 16 in
  Hashset.add_seq set (List.to_seq [ 1; 2; 3; 2; 1 ]);
  print_elements set;
  [%expect {| [ 1; 2; 3 ] |}];
  ()
;;

let%expect_test "of_seq" =
  let set = Hashset.of_seq (module Int_elt) (List.to_seq [ 5; 3; 1; 3; 5 ]) in
  print_elements set;
  [%expect {| [ 1; 3; 5 ] |}];
  ()
;;

let%expect_test "stats" =
  let set = Hashset.create (module Int_elt) 16 in
  Hashset.add set 1;
  Hashset.add set 2;
  let stats = Hashset.stats set in
  print_dyn (stats.num_bindings |> Dyn.int);
  [%expect {| 2 |}];
  require (stats.num_buckets > 0);
  [%expect {||}];
  ()
;;
