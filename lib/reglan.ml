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
  | Lit of Codepoint.t array (* a concrete non-empty string *)
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

let compare_lit (s1 : Codepoint.t array) (s2 : Codepoint.t array) : int =
  let n1 = Array.length s1 in
  let n2 = Array.length s2 in
  let n = if n1 < n2 then n1 else n2 in
  let rec aux i =
    if i = n then Int.compare n1 n2
    else
      let c = Codepoint.compare s1.(i) s2.(i) in
      if c <> 0 then c else aux (i + 1)
  in
  aux 0

let rec compare a b =
  match (a, b) with
  | Empty, Empty | Eps, Eps -> 0
  | Chars (l1, h1), Chars (l2, h2) ->
      let c = Codepoint.compare l1 l2 in
      if c <> 0 then c else Codepoint.compare h1 h2
  | Lit s1, Lit s2 -> compare_lit s1 s2
  | Concat (a1, b1), Concat (a2, b2)
  | Union (a1, b1), Union (a2, b2)
  | Inter (a1, b1), Inter (a2, b2) ->
      let c = compare a1 a2 in
      if c <> 0 then c else compare b1 b2
  | Star x, Star y | Comp x, Comp y -> compare x y
  | _ -> Int.compare (rank a) (rank b)

(* [a == b] shortcuts shared subterms before the structural walk. *)
let equal (a : t) (b : t) = a == b || compare a b = 0
let none = Empty
let epsilon = Eps

let allchar =
  Chars
    ( Codepoint.of_int_exn Codepoint.min_code_point,
      Codepoint.of_int_exn Codepoint.max_code_point )

let all = Star allchar
let to_re s = if Ustr.is_empty s then Eps else Lit (Ustr.to_codepoint_array s)
let range lo hi = if Codepoint.compare lo hi > 0 then Empty else Chars (lo, hi)

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
let diff a b = inter a (comp b)
let concat_list l = List.fold_right concat l Eps
let union_list l = List.fold_left union Empty l
let inter_list = function [] -> all | r :: rest -> List.fold_left inter r rest

(* r+ = r r* *)
let plus r = concat r (star r)

(* r? = ε | r *)
let opt r = union r Eps
let rec power n r = if n <= 0 then Eps else concat r (power (n - 1) r)

(* r{n1, n2} = r^n1 (r?)^(n2-n1) *)
let loop n1 n2 r =
  if n1 > n2 || n2 < 0 then Empty
  else
    let n1 = if n1 < 0 then 0 else n1 in
    let rec opts k = if k <= 0 then Eps else concat (opt r) (opts (k - 1)) in
    concat (power n1 r) (opts (n2 - n1))

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
      if Codepoint.equal s.(0) c then
        let len = Array.length s in
        if len = 1 then Eps else Lit (Array.sub s 1 (len - 1))
      else Empty
  | Concat (a, b) ->
      let d = concat (deriv a c) b in
      if nullable a then union d (deriv b c) else d
  | Union (a, b) -> union (deriv a c) (deriv b c)
  | Inter (a, b) -> inter (deriv a c) (deriv b c)
  | Star a -> concat (deriv a c) (star a)
  | Comp a -> comp (deriv a c)

let matches r s = nullable (Array.fold_left deriv r (Ustr.to_codepoint_array s))
let in_re s r = matches r s

(* ------------------------------------------------------------------ *)
(* Match search                                                       *)
(* ------------------------------------------------------------------ *)

(* Returns the smallest index [j >= i] such that the slice from [i] up to [j-1]
   is the shortest match of [r] starting at [i], or [-1] if there is none.
   If [r] is nullable, matches empty ([j = i]). *)
let shortest_match_at arr i r =
  let len = Array.length arr in
  let rec aux j r =
    if nullable r then j
    else if j >= len then -1
    else aux (j + 1) (deriv r arr.(j))
  in
  aux i r

(* Same, but non-empty: smallest [j > i]. *)
let shortest_nonempty_match_at arr i r =
  let len = Array.length arr in
  let rec aux j r =
    if j >= len then -1
    else
      let r' = deriv r arr.(j) in
      if nullable r' then j + 1 else aux (j + 1) r'
  in
  aux i r

(* Returns the start and length of the first match of [r] in [s], starting at
   [start]. *)
let find s r start =
  let arr = Ustr.to_codepoint_array s in
  let len = Array.length arr in
  if start < 0 || start > len then None
  else
    let rec aux i =
      if i > len then None
      else
        match shortest_match_at arr i r with
        | -1 -> aux (i + 1)
        | j -> Some (i, j - i)
    in
    aux start

let indexof_re s r start =
  match find s r start with Some (i, _) -> i | None -> -1

(* ------------------------------------------------------------------ *)
(* Replacement                                                        *)
(* ------------------------------------------------------------------ *)

let replace_re s r repl =
  let arr = Ustr.to_codepoint_array s in
  let len = Array.length arr in
  let rec aux i =
    if i > len then None
    else
      (* Check index and increment, there's probably a more efficient way to do this *)
      match shortest_match_at arr i r with
      | -1 -> aux (i + 1)
      | j -> Some (i, j)
  in
  match aux 0 with
  | None -> s (* no substring matches *)
  | Some (i, j) ->
      Ustr.concat_list
        [
          Ustr.of_codepoint_array (Array.sub arr 0 i);
          repl;
          Ustr.of_codepoint_array (Array.sub arr j (len - j));
        ]

let replace_re_all s r repl =
  let arr = Ustr.to_codepoint_array s in
  let len = Array.length arr in
  let slice lo hi = Ustr.of_codepoint_array (Array.sub arr lo (hi - lo)) in
  let rec next_match k =
    if k > len then None
    else
      match shortest_nonempty_match_at arr k r with
      | -1 -> next_match (k + 1)
      | stop -> Some (k, stop)
  in
  (* For each match at [(start, stop)], concatenate the text from [pos] to
     [start] then the replacement [repl].
     And cotinue matching from [stop], once there is no match, concatenate
     the remaining text.
     Matches are non-empty, so [pos] keeps growing until the loop terminates. *)
  let rec loop pos acc =
    match next_match pos with
    | None -> List.rev (slice pos len :: acc)
    | Some (start, stop) -> loop stop (repl :: slice pos start :: acc)
  in
  Ustr.concat_list (loop 0 [])

(* ------------------------------------------------------------------ *)
(* Printing                                                           *)
(* ------------------------------------------------------------------ *)

let lit_str cps = Ustr.to_smtlib_literal (Ustr.of_codepoint_array cps)
let char_str c = Ustr.to_smtlib_literal (Ustr.of_codepoint c)

let rec pp fmt = function
  | Empty -> Format.pp_print_string fmt "re.none"
  | Eps -> Format.fprintf fmt "(str.to_re \"\")"
  | Chars (lo, hi) when Codepoint.equal lo hi ->
      Format.fprintf fmt "(str.to_re %s)" (char_str lo)
  | Chars (lo, hi)
    when (lo :> int) = Codepoint.min_code_point
         && (hi :> int) = Codepoint.max_code_point ->
      Format.pp_print_string fmt "re.allchar"
  | Chars (lo, hi) ->
      Format.fprintf fmt "(re.range %s %s)" (char_str lo) (char_str hi)
  | Lit s -> Format.fprintf fmt "(str.to_re %s)" (lit_str s)
  | Concat (a, b) -> Format.fprintf fmt "(re.++ %a %a)" pp a pp b
  | Union (a, b) -> Format.fprintf fmt "(re.union %a %a)" pp a pp b
  | Inter (a, b) -> Format.fprintf fmt "(re.inter %a %a)" pp a pp b
  | Star r -> Format.fprintf fmt "(re.* %a)" pp r
  | Comp r -> Format.fprintf fmt "(re.comp %a)" pp r

let to_string r = Format.asprintf "%a" pp r
