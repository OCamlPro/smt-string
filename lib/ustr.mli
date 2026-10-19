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

(** SMT-LIB Unicode strings: a finite sequence of {!Smt_string.Codepoint.t}
    (code points in [[0, 0x2FFFF]], Planes 0-2).

    Semantics are over the code-point sequence, no normalization, collation or
    grapheme clustering.

    @see <https://smt-lib.org/theories-UnicodeStrings.shtml> SMT-LIB theory *)

type t
(** An immutable SMT-LIB string. *)

(** {1 Construction and conversion} *)

val empty : t
val is_empty : t -> bool

val of_codepoint : Codepoint.t -> t
(** Singleton string. *)

val of_code_points : int array -> t option
(** [of_code_points a] returns [Some s], where [s] is the string made from the
    array [a] of integers representing code points, and [None] if any integer is
    out of the code point range. *)

val of_code_points_exn : int array -> t
(** [of_code_points_exn a], same as {!of_code_points} but raises
    [Invalid_argument] if an integer is out of the code point range. *)

val to_code_points : t -> int array
(** [to_code_points s] returns the array of integers representing the code
    points of [s]. *)

val of_codepoint_array : Codepoint.t array -> t
(** [of_codepoint_array a] builds a string from an array of valid code points.
*)

val to_codepoint_array : t -> Codepoint.t array
(** [to_codepoint_array s] returns the characters of [s] as an array of code
    points. *)

val get : t -> int -> Codepoint.t option
(** [get s i] returns the [i]-th character of [s], or [None] if [i] is out of
    bounds. *)
