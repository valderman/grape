Grape
=====
Generic Reification of ADTs and Patterns for EDSLs

What?
-----
Proof of concept for reifying ADTs and pattern matching in deeply embedded
EDSLs. See `Example.hs`.

TODO
----
* Generalise `new` and `match` to any monadic EDSL with read, write, alloc,
  conjunction, equality and conditionals.
* Generalise pattern representation over per-EDSL primitive types.
  - Probably can't be done without using exceptions for "type-checking"
    injected terms.
