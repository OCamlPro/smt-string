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
