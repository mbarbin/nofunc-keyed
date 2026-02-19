(***********************************************************************************)
(*  nofunc-keyed: Keyed data structures adapted from OCaml Stdlib but no functors  *)
(*  SPDX-FileCopyrightText: 2025 Mathieu Barbin <mathieu.barbin@gmail.com>         *)
(*  SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception   *)
(***********************************************************************************)

let%expect_test "phys_equal" =
  let hello () = "Hello" ^ " World" in
  let h1 = hello () in
  print_dyn (phys_equal h1 h1 |> Dyn.bool);
  [%expect {| true |}];
  print_dyn (phys_equal h1 (hello ()) |> Dyn.bool);
  [%expect {| false |}];
  ()
;;

let%expect_test "require" =
  require true;
  [%expect {||}];
  require_does_raise (fun () -> require false);
  [%expect {| Failure("Required condition does not hold.") |}]
;;

let%expect_test "require_does_raise" =
  require_does_raise (fun () -> failwith "Hello Exn");
  [%expect {| Failure("Hello Exn") |}];
  ()
;;

let%expect_test "require_does_raise did not raise" =
  (match require_does_raise ignore with
   | () -> assert false
   | exception exn -> print_string (Printexc.to_string exn));
  [%expect {| ("Did not raise.", {}) |}];
  ()
;;

let%expect_test "require_equal" =
  require_equal (module Int) 42 42;
  [%expect {||}];
  ()
;;

let%expect_test "require_equal not equal" =
  (match require_equal (module Int) 0 42 with
   | () -> assert false
   | exception exn -> print_string (Printexc.to_string exn));
  [%expect {| ("Values are not equal.", { v1 = 0; v2 = 42 }) |}];
  ()
;;

let%expect_test "require_not_equal" =
  require_not_equal (module Int) 0 42;
  [%expect {||}];
  ()
;;

let%expect_test "require_not_equal equal" =
  (match require_not_equal (module Int) 0 0 with
   | () -> assert false
   | exception exn -> print_string (Printexc.to_string exn));
  [%expect {| ("Values are  equal.", { v1 = 0; v2 = 0 }) |}];
  ()
;;
