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
* Break out reference stuff into a `RefM`?
* Break out memory stuff into a `MemM`?
* Break out conditionals into a `CondM`?
* Semantics for reusing names in pattern: Haskell-time error, or check equality?
  - Need function `Ref a -> Exp a` in class.
* First implement products as structs, then implement sums as a tag field on the
  struct?
  - Could get rid of explicit conjunction (bake into ifThenElse), boolean
    literals.
  - Resulting sum struct needs to be the union of its parts.
  - Reuse parts.
  - Wastes memory in pathological cases, so explain about unions when doing it
    in C.
  - In JS, unions can be faked by just not creating the fields in question.
  - Would solve bug where `Exp Int` might be either an actual int or a
    pointer to one without having to think about it.
  - Can we even do this without losing generality?
* Split patterns and ADTs into two classes?
  - Patterns should only be first-class on the Haskell level.
  - ...because storing/loading them would complicate things, and not add much.
  - All patterns are ADTs, but not all ADTs are patterns.
* Investigate possibility to allow ADTs to be represented either as
  `Exp m (ADT a)` or `ADT a`. The former needs a tiny bit of support from the
  embedding. The latter doesn't, but might require more support on top of the
  embedding (i.e. conditionals where branches may return `ADT a` and not just
  `Exp m a`).
* Investigate changes needed to support Feldspar, Sunroof and SatPlus.
