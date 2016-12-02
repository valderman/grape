Grape
=====
Generic Reification of ADTs and Patterns for EDSLs

What?
-----
Proof of concept for reifying ADTs and pattern matching in deeply embedded
EDSLs. See `Example.hs`.

Pros
----
* Use arbitrary ADTs in EDSLs.
* Generalises to any monadic EDSL with load/store/conditionals.

Cons
----
* Holes in ADTs (as opposed to patterns) are only caught during CG, not by the
  type system.
* Introducing new vars for binding in patterns can be hairy.

TODO
----
* Get rid of a few methods from `PatM`.
* Generalise pattern representation over per-EDSL primitive types.
  - Probably can't be done without using exceptions for "type-checking"
    injected terms.
  - Might complicate things even further?
* Second-best solution: simply use `(nbytes :: Int, value :: Word64)` as the
  primitive type for everything.
  - Current non-solution provides no real benefit over this, but complicates
    the code quite a bit.
