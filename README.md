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
* Investigate possibility to derive instance for ADT m a => ADT m (Exp a)
  - Look into how this affects usefulness and generalizability if impossible