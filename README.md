Grape
=====
Generic Reification of ADTs and Patterns for EDSLs

What?
-----
Proof of concept for reifying ADTs and pattern matching in deeply embedded
EDSLs. See `Example.hs`.

TODO
----
* Get rid of a few methods from `PatM`.
* Generalise pattern representation over per-EDSL primitive types.
  - Probably can't be done without using exceptions for "type-checking"
    injected terms.
