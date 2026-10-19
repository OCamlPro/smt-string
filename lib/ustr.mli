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

val hash : t -> int

(** {1 SMT-LIB literal output} *)

val to_smtlib_literal : t -> string
(** The SMT-LIB quoted form of [s]: printable ASCII is kept as is (a double
    quote inside [s] is written twice), and every other character is written as
    a [\u] hex escape. *)

val pp : Format.formatter -> t -> unit
(** Prints using {!to_smtlib_literal}. *)

(** {1 SMT-LIB operations}

    Argument order mirrors the theory's symbols and partial functions use its
    totalizing conventions (out-of-range yields the empty string or [-1]). *)

val length : t -> int
(** [str.len] number of code points. *)

val concat : t -> t -> t
(** [str.++]. *)

val concat_list : t list -> t

val compare : t -> t -> int
(** Numeric code-point lexicographic order. *)

val equal : t -> t -> bool

val lt : t -> t -> bool
(** [str.<]. *)

val leq : t -> t -> bool
(** [str.<=]. *)

val at : t -> int -> t
(** [str.at s i] returns the singleton string containing the element at [i], or
    the empty string if [i] is out of range. *)

val substr : t -> int -> int -> t
(** [str.substr s i n]: longest substring from [i] of length at most [n],
    returns the empty string unless [0 <= i < length s] and [n > 0]. *)

val contains : t -> t -> bool
(** [str.contains s sub] does [sub] occur in [s]? ([true] if [sub] is empty). *)

val prefixof : t -> t -> bool
(** [str.prefixof p s]: is [p] a prefix of [s]? *)

val suffixof : t -> t -> bool
(** [str.suffixof q s]: is [q] a suffix of [s]? *)

val indexof : t -> t -> int -> int
(** [str.indexof s sub i]: smallest position [>= i] of [sub] in [s], or [-1]
    (also when [i < 0] or [i > length s]). Empty [sub] matches at [i]. *)

val replace : t -> t -> t -> t
(** [str.replace s sub by] replace the first occurrence of [sub] with [by] in
    [s], returns [s] if [sub] is not found, and if [sub] is empty returns
    [by ++ s]. *)

val replace_all : t -> t -> t -> t
(** [str.replace_all s sub by]: replace every non-overlapping occurrence, from
    left to right, of [sub] with [by] in [s], returns [s] if [sub] is empty. *)

val to_code : t -> int
(** [str.to_code s]: returns the code point of a singleton string [s], [-1]
    otherwise. *)

val from_code : int -> t
(** [str.from_code n] singleton for a valid [n], else empty. *)

val to_int : t -> int
(** [str.to_int s] returns the integer value of a non-empty all-digit string
    [s], [-1] otherwise. *)

val from_int : int -> t
(** [str.from_int n] returns the string corresponding to the decimal
    representation of the integer [n >= 0], otherwise returns the empty string.
*)

val is_digit : t -> bool
(** [str.is_digit s] is [s] a single ASCII digit? *)
