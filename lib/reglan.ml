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

(* Membership by Brzozowski derivatives. [deriv r c] is the language of
   suffixes [s] such that [c :: s] is in [r], i.e. what still has to be
   matched after reading [c].

   To test [s = c1...cn], derive one code point at a time, [deriv r c1], then
   that by [c2] ... [s] is in [r] iff the final language is [nullable]
   (contains [""], so nothing remains to match).
   The smart constructors keep these terms small. *)

type t =
  | Empty (* re.none *)
  | Eps (* {""} *)
  | Chars of Codepoint.t * Codepoint.t (* inclusive code-point range *)
  | Lit of Ustr.t (* a concrete non-empty string *)
  | Concat of t * t
  | Union of t * t
  | Inter of t * t
  | Star of t
  | Comp of t

let rank = function
  | Empty -> 0
  | Eps -> 1
  | Chars _ -> 2
  | Lit _ -> 3
  | Concat _ -> 4
  | Union _ -> 5
  | Inter _ -> 6
  | Star _ -> 7
  | Comp _ -> 8

let rec compare a b =
  match (a, b) with
  | Empty, Empty | Eps, Eps -> 0
  | Chars (l1, h1), Chars (l2, h2) ->
      let c = Codepoint.compare l1 l2 in
      if c <> 0 then c else Codepoint.compare h1 h2
  | Lit s1, Lit s2 -> Ustr.compare s1 s2
  | Concat (a1, b1), Concat (a2, b2)
  | Union (a1, b1), Union (a2, b2)
  | Inter (a1, b1), Inter (a2, b2) ->
      let c = compare a1 a2 in
      if c <> 0 then c else compare b1 b2
  | Star x, Star y | Comp x, Comp y -> compare x y
  | _ -> Int.compare (rank a) (rank b)

(* [a == b] shortcuts shared subterms before the structural walk. *)
let equal (a : t) (b : t) = a == b || compare a b = 0

(* ------------------------------------------------------------------ *)
(* Construction                                                       *)
(* ------------------------------------------------------------------ *)

let empty = Empty
let epsilon = Eps

let any_char =
  Chars
    ( Codepoint.of_int_exn Codepoint.min_code_point,
      Codepoint.of_int_exn Codepoint.max_code_point )

let range lo hi = if Codepoint.compare lo hi > 0 then Empty else Chars (lo, hi)
let of_ustr s = if Ustr.is_empty s then Eps else Lit s

let concat a b =
  match (a, b) with
  | Empty, _ | _, Empty -> Empty
  | Eps, x | x, Eps -> x
  | _ -> Concat (a, b)

let union a b =
  if equal a b then a
  else match (a, b) with Empty, x | x, Empty -> x | _ -> Union (a, b)

let inter a b =
  if equal a b then a
  else match (a, b) with Empty, _ | _, Empty -> Empty | _ -> Inter (a, b)

let star = function Empty | Eps -> Eps | Star _ as r -> r | r -> Star r
let comp = function Comp r -> r | r -> Comp r

(* ------------------------------------------------------------------ *)
(* Membership                                                         *)
(* ------------------------------------------------------------------ *)

let rec nullable = function
  | Empty | Chars _ | Lit _ -> false
  | Eps | Star _ -> true
  | Concat (a, b) | Inter (a, b) -> nullable a && nullable b
  | Union (a, b) -> nullable a || nullable b
  | Comp r -> not (nullable r)

let rec deriv r c =
  match r with
  | Empty | Eps -> Empty
  | Chars (lo, hi) ->
      if Codepoint.compare lo c <= 0 && Codepoint.compare c hi <= 0 then Eps
      else Empty
  | Lit s ->
      if Codepoint.equal (Ustr.get s 0) c then
        let len = Ustr.length s in
        if len = 1 then Eps else Lit (Ustr.sub s 1 (len - 1))
      else Empty
  | Concat (a, b) ->
      let d = concat (deriv a c) b in
      if nullable a then union d (deriv b c) else d
  | Union (a, b) -> union (deriv a c) (deriv b c)
  | Inter (a, b) -> inter (deriv a c) (deriv b c)
  | Star a -> concat (deriv a c) (star a)
  | Comp a -> comp (deriv a c)

let matches r s = nullable (Ustr.fold_left deriv r s)

(* ------------------------------------------------------------------ *)
(* Match search                                                       *)
(* ------------------------------------------------------------------ *)

(* Length of the shortest match of [r] starting at index [i] of [s], or [-1].
   With [nonempty], the empty match is not accepted. *)
let shortest_match_at ~nonempty s i r =
  let len = Ustr.length s in
  let rec aux j r =
    if nullable r && not (nonempty && j = i) then j - i
    else if j >= len then -1
    else aux (j + 1) (deriv r (Ustr.get s j))
  in
  aux i r

(* First match at or after [i]. Assumes [0 <= i <= length s]. *)
let find_from ~nonempty r s i =
  let len = Ustr.length s in
  let rec aux i =
    if i > len then None
    else
      match shortest_match_at ~nonempty s i r with
      | -1 -> aux (i + 1)
      | n -> Some (i, n)
  in
  aux i

let find_opt ?(nonempty = false) r s i =
  if i < 0 || i > Ustr.length s then None else find_from ~nonempty r s i

let find ?(nonempty = false) r s i =
  if i < 0 || i > Ustr.length s then invalid_arg "Smt_string.Reglan.find";
  match find_from ~nonempty r s i with Some m -> m | None -> raise Not_found
