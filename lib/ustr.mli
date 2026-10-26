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

(** Unicode strings: immutable sequences of {!Smt_string.Codepoint.t} (code
    points in [[0, 0x2FFFF]], Planes 0-2).

    Operations work on the code-point sequence, with no normalization, collation
    or grapheme clustering. *)

type t
(** An immutable string. *)

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

(** {1 Access} *)

val length : t -> int
(** Number of code points. *)

val get : t -> int -> Codepoint.t
(** [get s i] returns the [i]-th code point of [s].

    @raise [Invalid_argument] if [i] is out of bounds. *)

val get_opt : t -> int -> Codepoint.t option
(** [get_opt s i] returns the [i]-th code point of [s], or [None] if [i] is out
    of bounds. *)

val fold_left : ('a -> Codepoint.t -> 'a) -> 'a -> t -> 'a
(** [fold_left f init s] folds [f] over the code points of [s], from left to
    right, without copying [s]. *)

val sub : t -> int -> int -> t
(** [sub s i n] returns the [n] code points of [s] starting at [i].

    @raise [Invalid_argument] if they do not all lie within [s]. *)

val find : t -> t -> int -> int
(** [find s sub i] returns the smallest index [>= i] at which [sub] occurs in
    [s]. An empty [sub] occurs at [i].

    @raise [Not_found] if [sub] does not occur in [s] at or after [i].
    @raise [Invalid_argument] if [i] is not in [[0, length s]]. *)

val find_opt : t -> t -> int -> int option
(** [find_opt s sub i] same as {!find} but returns [None] instead of raising. *)

(** {1 Combination and comparison} *)

val concat : t -> t -> t
val concat_list : t list -> t

val compare : t -> t -> int
(** Lexicographic order on code points. *)

val equal : t -> t -> bool
val hash : t -> int
