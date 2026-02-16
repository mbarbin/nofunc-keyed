(*_**********************************************************************************)
(*_  nofunc-keyed: Keyed data structures adapted from OCaml Stdlib but no functors  *)
(*_  SPDX-FileCopyrightText: 2025 Mathieu Barbin <mathieu.barbin@gmail.com>         *)
(*_  SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception   *)
(*_**********************************************************************************)

val print_dyn : Dyn.t -> unit
val phys_equal : 'a -> 'a -> bool
val require : bool -> unit
val require_does_raise : (unit -> 'a) -> unit

module With_equal_and_dyn : sig
  module type S = sig
    type t

    val equal : t -> t -> bool
    val to_dyn : t -> Dyn.t
  end
end

val require_equal : (module With_equal_and_dyn.S with type t = 'a) -> 'a -> 'a -> unit
val require_not_equal : (module With_equal_and_dyn.S with type t = 'a) -> 'a -> 'a -> unit
