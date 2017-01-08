.H 2 "A simple example worked by hand"
In this example the reader is taken `behind the scenes', so to speak, to see
how program verification can be performed manually.
The function shown here
is a fixed point square root routine.
In addition to the normal proof of runtime-error-free operation,
a full proof of correctness is attempted, which is to say that we actually
try to prove that the function computes square root to within some
explicit error bound.  This example is rather more complex mathematically
than most parts of control programs, and as will be seen the verification
conditions generated are sometimes difficult mathematically,
especially when one bears in mind that the rules of fixed-point
arithmetic are being interpreted strictly.
.DP

.FG "Function before addition of verification statements"

(*
        Square root by bisection

        This technique has the useful property that it
        it will work for any monotonic function.
*)
FUNCTION sqrt(s: FIXED 2.0 .. 100.0 PRECISION 0.1):
               FIXED 1.0 .. 10.0 PRECISION 0.1;
VAR
    x, lowbound, highbound: FIXED 1.0 .. 100.0 PRECISION 0.1;
BEGIN
    x := s;                     (* set initial try *)
    lowbound := 1.0;            (* lowest possible square root *)
    highbound := x;             (* highest possible *)
                                (* stop when interval tiny *)
    WHILE highbound - lowbound > 0.1 DO
    BEGIN                       (* choose new trial value *)
        x := (highbound + lowbound) / 2;
        IF x*x > s THEN BEGIN   (* if x is too big *)
            highbound := x;     (* then answer must be below x *)
        END ELSE BEGIN          (* if x is too small *)
            lowbound := x;      (* then answer must be above x *)
        END;
    END;
    sqrt := x;                  (* return answer *)
END;                            (* of sqrt *)

.DE
The first step in documenting the function for verification is to define what
the procedure is supposed to do.  This is simple in this case, because
square root is easy to define.  An exit assertion of the form
.DP
    EXIT abs(x * x - s) < 0.2;
.DE
describes the desired result.
.P
The next step is to figure out how to prove that the function will produce
the desired result.
We can prove this by noting the following facts about the situation
that exists when the loop exits.
.BL
.LI
x is between lowbound and highbound
.LI
s is between lowbound squared and highbound squared
.LI
lowbound is less than or equal to highbound
.LI
lowbound is within 0.1 of highbound
.LE
.P
All these conditions except the last hold for every iteration of the
loop, and thus form part of the loop invariant.
These insights must be provided to the verifier in a STATE statement.
.P
Proving that the loop terminates requires that we find some measure that
decreases as the loop iterates.  Since the algorithm operates by closing
the interval between highbound and lowbound, the difference between these
two is a suitable value to appear in the MEASURE statement.
.DP

.FG "Function after addition of verification statements"

(*
        Square root by bisection

        This technique has the useful property that it
        it will work for any monotonic function.
*)
FUNCTION sqrt(s: FIXED 2.0 .. 100.0 PRECISION 0.1):
               FIXED 1.0 .. 10.0 PRECISION 0.1;
EXIT abs(sqrt*sqrt - s) <= 0.2;  (* definition of result *)

VAR
    x, lowbound, highbound: FIXED 1.0 .. 100.0 PRECISION 0.1;
BEGIN
    x := s;                     (* set initial try *)
    lowbound := 1.0;            (* lowest possible square root *)
    highbound := x;             (* highest possible *)
                                (* stop when interval tiny *)
    WHILE highbound - lowbound > 0.1 DO
    BEGIN
        MEASURE highbound - lowbound;
        STATE  lowbound*lowbound <= s,
               highbound*highbound >= s,
               highbound >= lowbound,
               x <= highbound,
               x >= lowbound;
                                (* choose new trial value *)
        x := (highbound + lowbound) / 2;
        IF x*x > s THEN BEGIN   (* if x is too big *)
            highbound := x;     (* then answer must be below x *)
        END ELSE BEGIN          (* if x is too small *)
            lowbound := x;      (* then answer must be above x *)
        END;
    END;
    sqrt := x;                  (* return answer *)
END;                            (* of sqrt *)

.DE
.P
.P
We can now attempt actual verification.
The first step in verification is to trace out all possible paths
of control flow.  Paths begin and end at the boundaries of procedures and
at STATE statements.  There are thus six paths in this function.
.AL
.LI
The path
starting at the beginning of the function, treating the WHILE condition
as true, and ending at the STATE statement.
.LI
The path
starting at the beginning of the function, treating the WHILE condition
as false, and ending at the end of the procedure.
.LI
The path
starting at the STATE statement, treating the IF condition as true, going
around the loop, treating the WHILE condition as true, and ending at the
STATE statement.
.LI
The path
starting at the STATE statement, treating the IF condition as false, going
around the loop, treating the WHILE condition as true, and ending at the
STATE statement.
.LI
The path
starting at the STATE statement, treating the IF condition as true, going
around the loop, treating the WHILE condition as false, and ending at the
end of the procedure.
.LI
The path
starting at the STATE statement, treating the IF condition as false, going
around the loop, treating the WHILE condition as false, and ending at the
end of the procedure.
.LE
.P
After working out the control flow in the program, the
next step is to locate all the assertions which must be verified for
the path and generate a verification condition for each.  Assertions
come not only from the user's EXIT and STATE statements but from
internal requirements needed to prevent run-time errors.
.P
For the first path, it is necessary to verify all the following conditions.
.AL
.LI
A range error does not occur at `x := s'.  (No problem, the type of x has
less restrictive bounds than that of s.)
.LI
A range error does not occur at `lowbound := 1.0'.  (No problem, the
constant value is in the correct range.)
.LI
A range error does not occur at `highbound := x'.  (No problem, the
types match.)
.LI
The value in the MEASURE statement is non-negative.  (Since lowbound is
1.0, and highbound is constrained by its type to be 2.0 or greater,
this requirement is met.)  Note that we are only concerned with the first
time through the loop on this path.
.LI
The STATE assertion `lowbound * lowbound <= s' holds.  (Since
1.0 * 1.0 <= 2.0, this holds.)
.LI
The STATE assertion `highbound * highbound >= s' holds.
(Since on this path highbound = x, we need only prove that
x > 2.0 IMPLIES x*x > x.)
.LI
The STATE assertion `highbound >= lowbound' holds.
(We know that highbound is not less than than 2.0
and that lowbound is 1.0, so this is no problem.)
.LI
The STATE assertion `x <= highbound' holds.
(x is equal to highbound; no problem.)
.LI
The STATE assertion `x >= lowbound' holds.
(We know that lowbound is 1.0, and that x is 2.0 or greater, so this
is no problem.)
.LE
.P
Proceeding on to path two, the path on which the WHILE statement body is
never executed, we find an interesting phenomenon.  The loop is entered
under the following conditions:
.DP
    lowbound = 1.0
    highbound >= 2.0
.DE
For path two to be executed, the condition for exiting the loop,
.DP
    NOT(highbound - lowbound > 0.1)
.DE
would have to be true at initial entrance to the loop.
Since these three
conditions cannot all be true simultaneously, this path is
never executed, and the loop body is always executed
at least once when the function is called.
All verification conditions for this path are true
since the requirement for entry is false.
Note that there is no dead code; there merely is no case where
a specific set of decisions are taken when passing through
several conditional statements.
It is not unusual for this to happen and this presents no problems.
.P
We now reach the first of the two interesting paths, the one around the loop
taking the true branch at the IF statement.  This path starts and ends at
the STATE statement.
.P
The STATE statement defines the loop invariant.  We have already
proved (for path one) that the loop invariant is true at entry to the loop.
We must now prove that if the loop invariant is true for a given
iteration of the loop, it will still be true at the end of that iteration.
By proving this, we prove by induction that the loop invariant is true
for every iteration of the loop.
.P
This path is complex enough that each verification condition
is written out formally as a proposition to be proven.
.AL
.LI
The first verification condition is produced by trying to prove that
the invariant condition
.DP
        lowbound * lowbound <= x
.DE
holds.  The verification condition generated is
.DP
         lowbound >= 1.0 AND lowbound <= 100.0
     AND highbound >= 1.0 AND highbound <= 100.0
     AND s >= 2.0 AND s <= 100.0
     AND lowbound * lowbound <= s
     AND highbound * highbound >= s
     AND highbound >= lowbound
     AND x <= highbound
     AND x >= lowbound
     AND xNEW = (highbound + lowbound) / 2
     AND xNEW * xNEW > s
     AND highboundNEW = xNEW
     AND highboundNEW - lowbound > 0.1
   IMPLIES
     lowbound * lowbound <= s;
.DE
This simplifies to
.DP
     lowbound * lowbound <= s
   IMPLIES
     lowbound * lowbound <= s;
.DE
which is obviously true.
This is a trivial case, because lowbound did not change on this path.
.LI
.DP
         lowbound >= 1.0 AND lowbound <= 100.0
     AND highbound >= 1.0 AND highbound <= 100.0
     AND s >= 2.0 AND s <= 100.0
     AND lowbound * lowbound <= s
     AND highbound * highbound >= s
     AND highbound >= lowbound
     AND x <= highbound
     AND x >= lowbound
     AND xNEW = (highbound + lowbound) / 2
     AND xNEW * xNEW > s
     AND highboundNEW = xNEW
     AND highboundNEW - lowbound > 0.1
   IMPLIES
     highboundNEW*highboundNEW >= s;
.DE
which simplifies to
.DP
     xNEW * xNEW > s
   IMPLIES
     xNEW * xNEW >= s
.DE
which is true.
.LI
.DP
         lowbound >= 1.0 AND lowbound <= 100.0
     AND highbound >= 1.0 AND highbound <= 100.0
     AND s >= 2.0 AND s <= 100.0
     AND lowbound * lowbound <= s
     AND highbound * highbound >= s
     AND highbound >= lowbound
     AND x <= highbound
     AND x >= lowbound
     AND xNEW = (highbound + lowbound) / 2
     AND xNEW * xNEW > s
     AND highboundNEW = xNEW
     AND highboundNEW - lowbound > 0.1
   IMPLIES
     highboundNEW >= lowbound;
.DE
which simplifies to
.DP
         lowbound >= 1.0
     AND highbound >= 1.0
     AND highbound >= lowbound
   IMPLIES
     (highbound + lowbound) / 2 >= lowbound
.DE
which is true.
.LI
.DP
         lowbound >= 1.0 AND lowbound <= 100.0
     AND highbound >= 1.0 AND highbound <= 100.0
     AND s >= 2.0 AND s <= 100.0
     AND lowbound * lowbound <= s
     AND highbound * highbound >= s
     AND highbound >= lowbound
     AND x <= highbound
     AND x >= lowbound
     AND xNEW = (highbound + lowbound) / 2
     AND xNEW * xNEW > s
     AND highboundNEW = xNEW
     AND highboundNEW - lowbound > 0.1
   IMPLIES
     xNEW <= highboundNEW;
.DE
which transitivity of equality shows to be true.
.LI
The possibility that overflow might occur in the statement
.DP
     x := (highbound + lowbound) / 2;
.DE
must be considered.
The verification condition
.DP
         lowbound >= 1.0 AND lowbound <= 100.0
     AND highbound >= 1.0 AND highbound <= 100.0
     AND s >= 2.0 AND s <= 100.0
     AND lowbound * lowbound <= s
     AND highbound * highbound >= s
     AND highbound >= lowbound
   IMPLIES
         (highbound + lowbound) / 2 >= 1.0
     AND (highbound + lowbound) / 2 <= 100.0
.DE
describes this, and this can be shown to always hold based solely on the
restrictions on lowbound and highbound.
.LI
It is necessary to prove that the loop terminates.  This is done by
showing that the value in the MEASURE statement decreases with each
loop iteration but never becomes negative.
.DP
         lowbound >= 1.0 AND lowbound <= 100.0
     AND highbound >= 1.0 AND highbound <= 100.0
     AND s >= 2.0 AND s <= 100.0
     AND lowbound * lowbound <= s
     AND highbound * highbound >= s
     AND highbound >= lowbound
     AND x <= highbound
     AND x >= lowbound
     AND xNEW = (highbound + lowbound) / 2
     AND xNEW * xNEW > s
     AND highboundNEW = xNEW
     AND highboundNEW - lowbound > 0.1
   IMPLIES
         (highboundNEW - lowbound) < (highbound - lowbound)
     AND (highboundNEW - lowbound >= 0);
.DE
This immediately reduces to
.DP
         lowbound >= 1.0 AND lowbound <= 100.0
     AND highbound >= 1.0 AND highbound <= 100.0
     AND highbound >= lowbound
     AND ((highbound + lowbound) / 2) - lowbound > 0.1
   IMPLIES
     AND (highbound + lowbound) / 2 < highbound;
         (highbound + lowbound) / 2 >= lowbound;
.DE
which is true.
.LE
.P
The next path to be considered is the same as the one above, except that
the IF branch takes the `false' path.
We will spare the reader the details of this path, which are similar
to those shown above.
.P
The last two paths start at the STATE statement and go to the top of the
loop, but exit at the WHILE statement rather than continuing in the loop,
finally ending at the EXIT assertion.
Let us first consider the path through the `true' branch.
.AL
.LI
The replacement
.DP
    sqrt := x;          (* return answer *)
.DE
implies a restriction on x that x is less than or equal to 10.0, because
the type of the function `sqrt' is FIXED 1.0 .. 10.0, while the
type of `x' is FIXED 1.0 .. 100.0, leading to the verification condition
.DP
         lowbound >= 1.0 AND lowbound <= 100.0
     AND highbound >= 1.0 AND highbound <= 100.0
     AND s >= 2.0 AND s <= 100.0
     AND lowbound * lowbound <= s
     AND highbound * highbound >= s
     AND highbound >= lowbound
     AND x <= highbound
     AND x >= lowbound
     AND xNEW = (highbound + lowbound) / 2
     AND xNEW * xNEW > s
     AND highboundNEW = xNEW
     AND highboundNEW - lowbound <= 0.1
   IMPLIES
     xNEW <= 10.0;
.DE
Performing obvious simplifications,
we obtain
.DP
         lowbound <= 100.0
     AND highbound <= 100.0
     AND lowbound * lowbound <= 100.0
     AND highbound * highbound >= 2.0
     AND highbound >= lowbound
     AND xNEW = (highbound + lowbound) / 2
     AND xNEW - lowbound <= 0.1
   IMPLIES
     xNEW <= 10.0;
.DE
which we fail to prove.
.P
Let us see why.  A counterexample to the above verification condition is
.DP
    lowbound = 10.0
    highbound = 10.2
    xNEW = 10.1
.DE
which would cause overflow.  There are two possibilities to be
considered; either the Verifier does not have enough correct information
to constrain the value more, because of an ill-chosen STATE assertion,
or the program contains a bug.
.P
In this case, the program indeed contains a bug; an attempt to
take the square root of 100.0 will compute a value of 10.1, which is
within the desired error tolerance for the square root routine but
not within the range of approved values for the function.
This is a good example of the sort of bug the Verifier
is good at finding but which might be overlooked without verification.
.LI
Continuing onward, our last verification condition for this path is that
the square root function gets the right answer, or
.DP
         lowbound >= 1.0 AND lowbound <= 100.0
     AND highbound >= 1.0 AND highbound <= 100.0
     AND s >= 2.0 AND s <= 100.0
     AND lowbound * lowbound <= s
     AND highbound * highbound >= s
     AND highbound >= lowbound
     AND x <= highbound
     AND x >= lowbound
     AND xNEW = (highbound + lowbound) / 2
     AND xNEW * xNEW > s
     AND highboundNEW = xNEW
     AND highboundNEW - lowbound <= 0.1
   IMPLIES
     abs(xNEW * xNEW - s) <= 0.2;
.DE
Opening up the definition of `abs' gives us
.DP
         lowbound >= 1.0 AND lowbound <= 100.0
     AND highbound >= 1.0 AND highbound <= 100.0
     AND s >= 2.0 AND s <= 100.0
     AND lowbound * lowbound <= s
     AND highbound * highbound >= s
     AND highbound >= lowbound
     AND x <= highbound
     AND x >= lowbound
     AND xNEW = (highbound + lowbound) / 2
     AND xNEW * xNEW > s
     AND highboundNEW = xNEW
     AND highboundNEW - lowbound <= 0.1
   IMPLIES
        ((xNEW * xNEW > s) AND  (xNEW * xNEW - s) < 0.2))
     OR ((xNEW * xNEW < s) AND  (s - xNEW * xNEW) < 0.2));
.DE
which simplifies to
.DP
         lowbound >= 1.0 AND lowbound <= 100.0
     AND highbound >= 1.0 AND highbound <= 100.0
     AND s >= 2.0 AND s <= 100.0
     AND lowbound * lowbound <= s
     AND highbound * highbound >= s
     AND highbound >= lowbound
     AND xNEW = (highbound + lowbound) / 2
     AND xNEW * xNEW > s
     AND xNEW - lowbound <= 0.1
   IMPLIES
     xNEW * xNEW - s < 0.2;
.DE
which if augmented by the previously proved information that
xNEW is greater than or equal to lowbound,
can, with some difficulty, proved true.
.LE
.P
This completes the analysis of the example.  One clear bug was found,
as mentioned above;
an attempt to compute the square root of 100 will result in
an out-of-range result.
This sort of boundary condition bug is common, and is one of the things
which the Verifier is good at finding.
