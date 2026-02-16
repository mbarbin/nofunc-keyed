(*_**********************************************************************************)
(*_  nofunc-keyed: Keyed data structures adapted from OCaml Stdlib but no functors  *)
(*_  SPDX-FileCopyrightText: 2025 Mathieu Barbin <mathieu.barbin@gmail.com>         *)
(*_  SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception   *)
(*_**********************************************************************************)

(*_ Inspired by a similar module in stdune. *)

(** A programming error that should be reported upstream *)

type t =
  { message : string
  ; data : (string * Dyn.t) list
  }

exception E of t

val raise : string -> (string * Dyn.t) list -> _
