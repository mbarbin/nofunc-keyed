(***********************************************************************************)
(*  nofunc-keyed: Keyed data structures adapted from OCaml Stdlib but no functors  *)
(*  SPDX-FileCopyrightText: 2025 Mathieu Barbin <mathieu.barbin@gmail.com>         *)
(*  SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception   *)
(***********************************************************************************)

module Set = Nofunc_stdset.Set

let%expect_test "empty" =
  let e = Set.empty (module Int) in
  require (Set.is_empty e);
  [%expect {||}];
  print_dyn (Set.cardinal e |> Dyn.int);
  [%expect {| 0 |}];
  ()
;;
