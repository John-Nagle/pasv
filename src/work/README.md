# Work in progress on verifier

# temporary rule base

In the years since the Verifier has written, the syntax accepted by the Boyer-Moore theorem prover was
changed slightly. 

1.  The accessor list in ADD-SHELL cannot be ommtted, but can be "()", the empty list, for shells that
have no accessors. That's an easy fix.

2.  "!" is no longer allowed in symbols. This is a big problem, because the verifier uses "!" on all
its built-ins to avoid name clashes.

With those two fixes, the verifier's rule base will go through nqthm successfully, but it's not
usable with the verifier until a solution is found for the "!" syntax problem.
