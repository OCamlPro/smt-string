(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2026 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Lesser General    *)
(*  Public License version 2.1, with the special exception on linking     *)
(*  described in the LICENSE file in the root directory.                  *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

(** An SMT-LIB character: a Unicode code point in [[0, 0x2FFFF]] (Planes 0-2).

    Following the SMT-LIB Unicode strings theory, the characters are in this
    range and include the surrogates. {!t} is a [private int] that is always a
    valid code point.

    @see <https://smt-lib.org/theories-UnicodeStrings.shtml> SMT-LIB theory *)

type t = private int

val equal : t -> t -> bool
val compare : t -> t -> int

val min_code_point : int
(** [0]. *)

val max_code_point : int
(** [0x2FFFF] (196607). *)

val is_valid_code : int -> bool
(** [is_valid_code n] is [n] in [[0, 0x2FFFF]]? *)

val of_int : int -> t option
(** [of_int n] returns [Some c] if [n] is a valid code point, [None] otherwise.
*)

val of_int_exn : int -> t
(** [of_int_exn n] returns the code point corresponding to [n] if it is valid,
    raises [Invalid_argument] otherwise. *)

val to_int : t -> int
