open Smt_string

let ustr s =
  Ustr.of_code_points_exn
    (Array.init (String.length s) (fun i -> Char.code s.[i]))

let digit = Reglan.range (Codepoint.of_int_exn 0x30) (Codepoint.of_int_exn 0x39)

let () =
  assert (Codepoint.of_int (-1) = None && Codepoint.of_int 0x30000 = None);
  assert (Ustr.length (ustr "hello") = 5);
  assert (Ustr.equal (Ustr.concat (ustr "ab") (ustr "cd")) (ustr "abcd"));
  assert (Ustr.equal (Ustr.sub (ustr "hello") 1 3) (ustr "ell"));
  assert (Ustr.find (ustr "banana") (ustr "na") 0 = 2);
  assert (Ustr.find_opt (ustr "banana") (ustr "na") 7 = None);
  assert (
    Ustr.get_opt (ustr "ab") 1 <> None && Ustr.get_opt (ustr "ab") 2 = None);
  assert (Reglan.matches (Reglan.of_ustr (ustr "abc")) (ustr "abc"));
  assert (
    Reglan.matches digit (ustr "7") && not (Reglan.matches digit (ustr "a")));
  assert (Reglan.matches (Reglan.star digit) (ustr "000"));
  assert (Reglan.find digit (ustr "ab12") 0 = (2, 1));
  assert (Reglan.find_opt digit (ustr "ab") 0 = None);
  print_endline "All tests passed"
