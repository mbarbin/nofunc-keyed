(***********************************************************************************)
(*  nofunc-keyed: Keyed data structures adapted from OCaml Stdlib but no functors  *)
(*  SPDX-FileCopyrightText: 2025-2026 Mathieu Barbin <mathieu.barbin@gmail.com>    *)
(*  SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception   *)
(***********************************************************************************)

(* This suite exercises the "Modular explicit usage" section exposed by
   [Map], [Set], [Hashtbl] and [Hashset] together with [ppx_sexp_conv], across
   all four libraries at once, for total coverage: it checks that
   [ppx_sexp_conv] understands the [Modname.M(Arg).t] naming convention,
   rewriting it to a call to [Modname.sexp_of_m__t (module Arg) ...], for a
   record with a field of each shape.

   There is no equivalent ppx for [Dyn.t] yet, so [dyn_of_m__t] is only
   demonstrated below by direct calls. *)

module Map = Nofunc_map.Map
module Set = Nofunc_set.Set
module Hashtbl = Nofunc_htbl.Hashtbl
module Hashset = Nofunc_hset.Hashset
open Sexplib0.Sexp_conv

module Int_key = struct
  type t = int
  type comparator_witness

  let equal = Int.equal
  let hash = Stdlib.Hashtbl.hash
  let compare (a : int) (b : int) = Ordering.of_int (Stdlib.compare a b)
  let sexp_of_t = sexp_of_int
  let to_dyn = Dyn.int
end

module String_key = struct
  type t = string
  type comparator_witness

  let equal = String.equal
  let hash = Stdlib.Hashtbl.hash
  let compare (a : string) (b : string) = Ordering.of_int (Stdlib.compare a b)
  let sexp_of_t = sexp_of_string
  let to_dyn = Dyn.string
end

(* A record with one field per container's [M(Key).t] derives [sexp_of_t] just
   fine: [@@deriving sexp_of] recognizes the [M(Arg)] functor application in
   each field's type and rewrites it into a call to [sexp_of_m__t] on the
   corresponding module ([Map], [Set], [Hashtbl] or [Hashset]). *)
type t =
  { populations : int Map.M(String_key).t
  ; ids : Set.M(Int_key).t
  ; populations_htbl : int Hashtbl.M(String_key).t
  ; ids_hset : Hashset.M(Int_key).t
  }
[@@deriving sexp_of]

let%expect_test "record with a field for each of Map / Set / Hashtbl / Hashset's M(Key).t"
  =
  let populations_htbl = Hashtbl.create (module String_key) 16 in
  Hashtbl.set populations_htbl ~key:"b" ~data:2;
  Hashtbl.set populations_htbl ~key:"a" ~data:1;
  let ids_hset = Hashset.create (module Int_key) 16 in
  Hashset.add ids_hset 3;
  Hashset.add ids_hset 1;
  Hashset.add ids_hset 2;
  let t =
    { populations = Map.of_list (module String_key) [ "b", 2; "a", 1 ]
    ; ids = Set.of_list (module Int_key) [ 3; 1; 2 ]
    ; populations_htbl
    ; ids_hset
    }
  in
  print_s (sexp_of_t t);
  [%expect
    {|
    ((populations ((a 1) (b 2))) (ids (1 2 3)) (populations_htbl ((a 1) (b 2)))
     (ids_hset (1 2 3)))
    |}];
  ()
;;

let%expect_test "[%sexp_of: _ Map.M(Key).t] / [%sexp_of: Set.M(Key).t]" =
  let m = Map.of_list (module String_key) [ "b", 2; "a", 1 ] in
  print_s ([%sexp_of: int Map.M(String_key).t] m);
  [%expect {| ((a 1) (b 2)) |}];
  let s = Set.of_list (module Int_key) [ 3; 1; 2 ] in
  print_s ([%sexp_of: Set.M(Int_key).t] s);
  [%expect {| (1 2 3) |}];
  ()
;;

let%expect_test "[%sexp_of: _ Hashtbl.M(Key).t] / [%sexp_of: Hashset.M(Key).t]" =
  let tbl = Hashtbl.create (module String_key) 16 in
  Hashtbl.set tbl ~key:"b" ~data:2;
  Hashtbl.set tbl ~key:"a" ~data:1;
  print_s ([%sexp_of: int Hashtbl.M(String_key).t] tbl);
  [%expect {| ((a 1) (b 2)) |}];
  let set = Hashset.create (module Int_key) 16 in
  Hashset.add set 3;
  Hashset.add set 1;
  Hashset.add set 2;
  print_s ([%sexp_of: Hashset.M(Int_key).t] set);
  [%expect {| (1 2 3) |}];
  ()
;;

let%expect_test "dyn_of_m__t (no ppx for Dyn.t yet, called directly)" =
  let m = Map.of_list (module String_key) [ "b", 2; "a", 1 ] in
  print_dyn (Map.dyn_of_m__t (module String_key) Dyn.int m);
  [%expect {| map { "a" : 1; "b" : 2 } |}];
  let s = Set.of_list (module Int_key) [ 3; 1; 2 ] in
  print_dyn (Set.dyn_of_m__t (module Int_key) s);
  [%expect {| set { 1; 2; 3 } |}];
  let tbl = Hashtbl.create (module String_key) 16 in
  Hashtbl.set tbl ~key:"b" ~data:2;
  Hashtbl.set tbl ~key:"a" ~data:1;
  print_dyn (Hashtbl.dyn_of_m__t (module String_key) Dyn.int tbl);
  [%expect {| map { "a" : 1; "b" : 2 } |}];
  let set = Hashset.create (module Int_key) 16 in
  Hashset.add set 3;
  Hashset.add set 1;
  Hashset.add set 2;
  print_dyn (Hashset.dyn_of_m__t (module Int_key) set);
  [%expect {| set { 1; 2; 3 } |}];
  ()
;;
