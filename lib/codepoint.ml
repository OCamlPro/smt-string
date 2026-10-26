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

(* [t] is an [int], [private int] in the signature prevents code outside this
   module from constructing a [t] directly, and allows making sure that every
   value created is a valid code point. *)
type t = int

let equal = Int.equal
let compare = Int.compare
let min_code_point = 0
let max_code_point = 0x2FFFF
let is_valid_code n = n >= min_code_point && n <= max_code_point
let of_int n = if is_valid_code n then Some n else None

let of_int_exn n =
  if is_valid_code n then n
  else
    invalid_arg
      (Printf.sprintf "Smt_string.Codepoint.of_int_exn: %d out of range" n)

let to_int c = c
