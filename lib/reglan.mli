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

(** SMT-LIB regular languages (sort [RegLan]): sets of {!Smt_string.Ustr.t}.
    Membership is decided by Brzozowski derivatives.

    @see <https://smt-lib.org/theories-UnicodeStrings.shtml> SMT-LIB theory *)

type t
(** A regular language. *)

val equal : t -> t -> bool
(** Syntactic equality. *)

val compare : t -> t -> int
(** Syntactic total order. *)

(** {1 Atoms} *)

val none : t
(** [re.none] empty language. *)

val all : t
(** [re.all] all strings. *)

val allchar : t
(** [re.allchar] any single character. *)

val epsilon : t
(** [{""}], i.e. [(str.to_re "")]. *)

val to_re : Ustr.t -> t
(** [str.to_re s] the singleton [{s}]. *)

val range : Codepoint.t -> Codepoint.t -> t
(** [re.range lo hi] single chars in [[lo, hi]], empty if [lo > hi]. *)

(** {1 Combinators} *)

val concat : t -> t -> t
(** [re.++]. *)

val concat_list : t list -> t

val union : t -> t -> t
(** [re.union]. *)

val union_list : t list -> t

val inter : t -> t -> t
(** [re.inter]. *)

val inter_list : t list -> t

val diff : t -> t -> t
(** [re.diff] strings in [r1] but not [r2]. *)

val comp : t -> t
(** [re.comp] complement. *)

val star : t -> t
(** [re.*] Kleene star. *)

val plus : t -> t
(** [re.+], i.e. [r ++ r*]. *)

val opt : t -> t
(** [re.opt], i.e. [r | ""]. *)

val power : int -> t -> t
(** [(_ re.^ n) r] [n] repetitions ([{""}] if [n <= 0]). *)

val loop : int -> int -> t -> t
(** [(_ re.loop n1 n2) r] [n1] to [n2] repetitions, empty if [n1 > n2]. *)

(** {1 Membership} *)

val nullable : t -> bool
(** Does the language contain [""]? *)

val in_re : Ustr.t -> t -> bool
(** [str.in_re s R] is [s] in [R]? *)

val matches : t -> Ustr.t -> bool
(** {!in_re} with arguments flipped. *)

(** {1 Match search} *)

val indexof_re : Ustr.t -> t -> int -> int
(** Start index of the first match of [r] in [s] at or after [i], otherwise [-1]
    (also if [i] is outside [[0, length s]]).

    Not in the SMT-LIB theory, but cvc5 has it. *)

val find : Ustr.t -> t -> int -> (int * int) option
(** First, shortest match at or after [i] as [Some (start, length)], or [None].
*)

(** {1 Replacement} *)

val replace_re : Ustr.t -> t -> Ustr.t -> Ustr.t
(** Replace the first, shortest match of [r] in [s] with [by], prepends [by] if
    [r] accepts [""], and [s] is returned unchanged if nothing matches. *)

val replace_re_all : Ustr.t -> t -> Ustr.t -> Ustr.t
(** Replace every non-overlapping, shortest, non-empty match, left to right
    (empty matches skipped). *)

(** {1 Printing} *)

val pp : Format.formatter -> t -> unit
(** Prints in SMT-LIB syntax. *)

val to_string : t -> string
