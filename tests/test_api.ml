open Smt_string

let ustr s =
  Ustr.of_code_points_exn
    (Array.init (String.length s) (fun i -> Char.code s.[i]))

let digit = Reglan.range (Codepoint.of_int_exn 0x30) (Codepoint.of_int_exn 0x39)

let () =
  assert (Codepoint.of_int (-1) = None && Codepoint.of_int 0x30000 = None);
  assert (Ustr.length (ustr "hello") = 5);
  assert (Ustr.equal (Ustr.concat (ustr "ab") (ustr "cd")) (ustr "abcd"));
  assert (Ustr.equal (Ustr.substr (ustr "hello") 1 3) (ustr "ell"));
  assert (Ustr.indexof (ustr "banana") (ustr "na") 0 = 2);
  assert (
    Ustr.equal
      (Ustr.replace_all (ustr "aXbXc") (ustr "X") (ustr "-"))
      (ustr "a-b-c"));
  assert (Z.equal (Ustr.to_int (ustr "042")) (Z.of_int 42));
  assert (Ustr.equal (Ustr.from_int (Z.of_int 42)) (ustr "42"));
  assert (Reglan.in_re (ustr "abc") (Reglan.to_re (ustr "abc")));
  assert (Reglan.in_re (ustr "7") digit && not (Reglan.in_re (ustr "a") digit));
  assert (Reglan.in_re (ustr "000") (Reglan.star digit));
  assert (not (Reglan.in_re (ustr "") (Reglan.plus digit)));
  assert (
    let r = Reglan.loop 2 3 digit in
    (not (Reglan.in_re (ustr "1") r))
    && Reglan.in_re (ustr "12") r
    && not (Reglan.in_re (ustr "1234") r));
  assert (Reglan.indexof_re (ustr "ab12") digit 0 = 2);
  assert (
    Ustr.equal
      (Reglan.replace_re_all (ustr "a1b2c3") digit (ustr "#"))
      (ustr "a#b#c#"));
  print_endline "All tests passed"
