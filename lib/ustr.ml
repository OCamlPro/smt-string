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

let get (s : t) i : Codepoint.t option =
  if i >= 0 && i < Array.length s then Some s.(i) else None

let hash (s : t) : int =
  (* TODO: use a better hash function *)
  Hashtbl.hash s

(* ------------------------------------------------------------------ *)
(* SMT-LIB literal output                                              *)
(* ------------------------------------------------------------------ *)

let to_smtlib_literal (s : t) : string =
  let buf = Buffer.create (Array.length s + 2) in
  Buffer.add_char buf '"';
  Array.iter
    (fun c ->
      let cp = (c : Codepoint.t :> int) in
      if cp = 0x22 then Buffer.add_string buf "\"\"" (* doubled quote *)
      else if cp = 0x5C then Buffer.add_string buf "\\u{5c}"
        (* SMT-LIB always reads \u{...} as an escape *)
      else if cp >= 0x20 && cp <= 0x7E then Buffer.add_char buf (Char.chr cp)
      else Buffer.add_string buf (Printf.sprintf "\\u{%x}" cp))
    s;
  Buffer.add_char buf '"';
  Buffer.contents buf

let pp fmt s = Format.pp_print_string fmt (to_smtlib_literal s)

(* ------------------------------------------------------------------ *)
(* SMT-LIB operations                                                  *)
(* ------------------------------------------------------------------ *)

let length (s : t) = Array.length s
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

let equal (a : t) (b : t) = compare a b = 0
let lt a b = compare a b < 0
let leq a b = compare a b <= 0

let at (s : t) i : t =
  if i >= 0 && i < Array.length s then [| s.(i) |] else empty

let substr (s : t) i n : t =
  let len = Array.length s in
  if i < 0 || i >= len || n <= 0 then empty
  else
    let rem = len - i in
    Array.sub s i (Int.min n rem)

(* Does [sub] occur in [s] at index [j]? Assumes the span fits. *)
let matches_at (s : t) (sub : t) j =
  let sub_len = Array.length sub in
  let rec aux k =
    k = sub_len || (Codepoint.equal s.(j + k) sub.(k) && aux (k + 1))
  in
  aux 0

(* Least index [>= start] where non-empty [sub] occurs in [s], or [-1]. [start]
   should be [>= 0]. *)
let find_from (s : t) (sub : t) start =
  assert (start >= 0);
  let last = Array.length s - Array.length sub in
  let rec loop j =
    if j > last then -1 else if matches_at s sub j then j else loop (j + 1)
  in
  loop start

let contains (s : t) (sub : t) : bool =
  Array.length sub = 0 || find_from s sub 0 >= 0

let prefixof (p : t) (s : t) : bool =
  Array.length p <= Array.length s && matches_at s p 0

let suffixof (q : t) (s : t) : bool =
  let q_len = Array.length q in
  let s_len = Array.length s in
  q_len <= s_len && matches_at s q (s_len - q_len)

let indexof (s : t) (sub : t) i : int =
  let n = Array.length s in
  if i < 0 || i > n then -1
  else if Array.length sub = 0 then i (* empty matches at i *)
  else find_from s sub i

let replace (s : t) (sub : t) (by_ : t) : t =
  if Array.length sub = 0 then concat by_ s
  else
    match find_from s sub 0 with
    | -1 -> s
    | j ->
        let sub_len = Array.length sub in
        let start = Array.sub s 0 j in
        let sub_end_pos = j + sub_len in
        let end_ = Array.sub s sub_end_pos (Array.length s - sub_end_pos) in
        concat_list [ start; by_; end_ ]

let replace_all (s : t) (sub : t) (by_ : t) : t =
  let sub_len = Array.length sub in
  if sub_len = 0 then s
  else
    let n = Array.length s in
    (* Collect, in reverse, the slices between matches and the replacement for
       each match, then join them. Advancing past each match keeps occurrences
       non-overlapping. *)
    let rec loop i acc =
      match find_from s sub i with
      | -1 -> List.rev (Array.sub s i (n - i) :: acc)
      | j -> loop (j + sub_len) (by_ :: Array.sub s i (j - i) :: acc)
    in
    concat_list (loop 0 [])

let to_code (s : t) : int =
  if Array.length s = 1 then (s.(0) : Codepoint.t :> int) else -1

let from_code n : t =
  if Codepoint.is_valid_code n then [| Codepoint.of_int_exn n |] else empty

let to_int (s : t) : int =
  let len = Array.length s in
  if len = 0 then -1 (* the empty string is not a numeral *)
  else
    let rec loop acc i =
      if i = len then acc
      else
        let c = (s.(i) : Codepoint.t :> int) in
        if c >= 0x30 && c <= 0x39 then loop ((acc * 10) + (c - 0x30)) (i + 1)
        else -1 (* a non-digit makes the whole string invalid *)
    in
    loop 0 0

let from_int n : t =
  if n < 0 then empty
  else
    let str = string_of_int n in
    Array.init (String.length str) (fun i ->
        Codepoint.of_int_exn (Char.code str.[i]))

let is_digit (s : t) = Array.length s = 1 && Codepoint.is_digit s.(0)
