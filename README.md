# smt-string

An OCaml library of Unicode strings and regular expressions over the alphabet
of the [SMT-LIB](https://smt-lib.org/theories-UnicodeStrings.shtml) Unicode
strings theory, Unicode code points in the range `0x0`-`0x2FFFF` (planes 0-2).

It provides the primitives needed to build concrete string and regex values.

## Modules

- `Codepoint`: Unicode code points in the range `0x0`-`0x2FFFF` (planes 0-2).
- `Ustr`: immutable strings of code points, with substring search.
- `Reglan`: regular languages, with membership decided by Brzozowski
  derivatives and leftmost shortest match search.
