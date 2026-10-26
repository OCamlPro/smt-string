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

(** Regular languages: sets of {!Smt_string.Ustr.t}, with membership decided by
    Brzozowski derivatives.

    The constructors are exposed so that terms can be inspected (e.g. to print
    them), but values are only built with the functions below, which keep terms
    simplified. *)

type t = private
  | Empty  (** The empty language. *)
  | Eps  (** The language of the empty string only. *)
  | Chars of Codepoint.t * Codepoint.t
      (** Code points in an inclusive range, with [lo <= hi]. *)
  | Lit of Ustr.t  (** A single non-empty string. *)
  | Concat of t * t
  | Union of t * t
  | Inter of t * t
  | Star of t
  | Comp of t  (** Complement, with respect to all strings. *)

val equal : t -> t -> bool
(** Syntactic equality. *)

val compare : t -> t -> int
(** Syntactic total order. *)

(** {1 Construction} *)

val empty : t
(** The empty language. *)

val epsilon : t
(** The language of the empty string only. *)

val any_char : t
(** Any single code point. *)

val range : Codepoint.t -> Codepoint.t -> t
(** [range lo hi] single code points in [[lo, hi]], empty if [lo > hi]. *)

val of_ustr : Ustr.t -> t
(** [of_ustr s] the language of [s] only. *)

val concat : t -> t -> t
val union : t -> t -> t
val inter : t -> t -> t

val star : t -> t
(** Kleene star. *)

val comp : t -> t
(** Complement. *)

(** {1 Membership} *)

val nullable : t -> bool
(** Does the language contain the empty string? *)

val deriv : t -> Codepoint.t -> t
(** [deriv r c] the strings [s] such that [c] followed by [s] is in [r]. *)

val matches : t -> Ustr.t -> bool
(** [matches r s] is [s] in [r]? *)

(** {1 Match search} *)

val find : ?nonempty:bool -> t -> Ustr.t -> int -> int * int
(** [find r s i] returns the first (shortest) match of [r] in [s] at or after
    [i] as [(start, length)]. Empty matches are skipped if [nonempty] is [true]
    (default [false]).

    @raise Not_found if there is no such match.
    @raise Invalid_argument if [i] is not in [[0, length s]]. *)

val find_opt : ?nonempty:bool -> t -> Ustr.t -> int -> (int * int) option
(** [find_opt r s i] same as {!find} but returns [None] instead of raising. *)
