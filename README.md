# smt-string

An OCaml library of Unicode strings and regular expressions following
[SMT-LIB](https://smt-lib.org/theories-UnicodeStrings.shtml) Unicode strings
theory.
Characters are Unicode code points in the range `0x0`-`0x2FFFF` (planes 0-2).

It provides implementations of the theory's operations for evaluating concrete
string and regex terms, e.g. when checking a model.

## Modules

- `Codepoint`: a single Unicode code point in the range `0x0`-`0x2FFFF`.
- `Ustr`: SMT-LIB strings (`str.*` operations).
- `Reglan`: regular expressions (`re.*` operations), with membership decided by
  Brzozowski derivatives.
