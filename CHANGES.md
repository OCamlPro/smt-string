# 0.2

- Keep only string and regex primitives, SMT-LIB semantics move to dolmen.
- Drop `zarith`, require OCaml >= 4.08.1.
- `Ustr`: add `get_opt`, `fold_left`, `sub`, `find`, `find_opt`.
- `Reglan`: expose private term type, add `deriv`, `find_opt`.

# 0.1

- Initial release.
- `Codepoint`: Unicode code points in the range `0x0`-`0x2FFFF` (planes 0-2).
- `Ustr`: SMT-LIB Unicode strings and the `str.*` operations.
- `Reglan`: SMT-LIB regular expressions (`re.*` operations), with membership
  decided by Brzozowski derivatives.
