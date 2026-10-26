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

(* A string is an array of [Codepoint.t], the arrays are never mutated and
   external arrays are copied, so values are immutable. *)
type t = Codepoint.t array

(* ------------------------------------------------------------------ *)
(* Construction and conversion                                        *)
(* ------------------------------------------------------------------ *)

let empty : t = [||]
let is_empty s = Array.length s = 0
let of_codepoint c : t = [| c |]
let of_code_points_exn a : t = Array.map Codepoint.of_int_exn a

let of_code_points a : t option =
  try Some (of_code_points_exn a) with Invalid_argument _ -> None

let to_code_points (s : t) : int array = Array.map Codepoint.to_int s
let of_codepoint_array (a : Codepoint.t array) : t = Array.copy a
let to_codepoint_array (s : t) : Codepoint.t array = Array.copy s

(* ------------------------------------------------------------------ *)
(* Access                                                             *)
(* ------------------------------------------------------------------ *)

let length (s : t) = Array.length s

let get (s : t) i : Codepoint.t =
  if i >= 0 && i < Array.length s then s.(i)
  else invalid_arg "Smt_string.Ustr.get"

let get_opt (s : t) i : Codepoint.t option =
  if i >= 0 && i < Array.length s then Some s.(i) else None

let fold_left f init (s : t) = Array.fold_left f init s

let sub (s : t) i n : t =
  if i >= 0 && n >= 0 && i + n <= Array.length s then Array.sub s i n
  else invalid_arg "Smt_string.Ustr.sub"

(* Does [sub] occur in [s] at index [j]? Assumes the span fits. *)
let matches_at (s : t) (sub : t) j =
  let sub_len = Array.length sub in
  let rec aux k =
    k = sub_len || (Codepoint.equal s.(j + k) sub.(k) && aux (k + 1))
  in
  aux 0

let find_from (s : t) (sub : t) i =
  let last = Array.length s - Array.length sub in
  let rec loop j =
    if j > last then None
    else if matches_at s sub j then Some j
    else loop (j + 1)
  in
  loop i

let find_opt (s : t) (sub : t) i =
  if i < 0 || i > Array.length s then None else find_from s sub i

let find (s : t) (sub : t) i =
  if i < 0 || i > Array.length s then invalid_arg "Smt_string.Ustr.find";
  match find_from s sub i with Some j -> j | None -> raise Not_found

(* ------------------------------------------------------------------ *)
(* Combination and comparison                                         *)
(* ------------------------------------------------------------------ *)

let concat (a : t) (b : t) : t = Array.append a b
let concat_list (l : t list) : t = Array.concat l

let compare (a : t) (b : t) : int =
  let la = Array.length a in
  let lb = Array.length b in
  let n = if la < lb then la else lb in
  let rec aux i =
    if i = n then Int.compare la lb
    else
      let c = Codepoint.compare a.(i) b.(i) in
      if c <> 0 then c else aux (i + 1)
  in
  aux 0

let equal (a : t) (b : t) =
  a == b || (Array.length a = Array.length b && compare a b = 0)

let hash (s : t) : int =
  Array.fold_left (fun h c -> (h * 31) + Codepoint.to_int c) 0 s land max_int
