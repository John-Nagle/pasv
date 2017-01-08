.H 1 "An introduction to verification"
Verifiers are not yet common software tools.  For this reason, a
substantial amount of explanation is in order.  This manual is addressed
to the professional programmer involved in the production of
high-reliability software.  Acquaintance with Pascal-F is assumed;
acquaintance with verification is not.
No specific mathematical background beyond that necessary to
comprehend a Pascal-like language is assumed.
Users who are uncomfortable with formal mathematics
in any form, however,
will find using the Verifier rather heavy going.
.P
There has been a certain mystique associated with verification.
Verification is often viewed as either an academic curiosity
or as a subject incomprehensible by mere programmers.
It is neither.  Verification is not easy, but then, neither is
writing reliable computer programs.  More than anything else, verifying a
program requires that one have a very clear understanding of the
program's desired behavior.
It is not verification that is hard to understand; verification is
fundamentally simple.  It is
.I really
understanding programs that can be hard.
.H 2 "What a verifier is and what it can and cannot do"
A verifier is a computer program, or set of computer programs.
It examines other programs and tries to prove that they
meet specified criteria.
Some of these criteria are implied by the language in which the program
is written.  Others are supplied by the programmer or the system designer.
It is the task of the verifier to try to show that the program
always meets the stated criteria, no matter what data or conditions the
program is faced with, provided only that the program is faithfully
executed by the computer.
.P
This task is not an easy one.
It is much more difficult than, for example, checking that a program
is syntactically correct.
Showing that the program works for all cases requires a much more powerful
approach than testing, simply because for programs of any complexity,
exhaustive testing requires numbers of test cases that are
far too large for there
to be any hope of trying all of them.
.P
Verifiers attack this problem by turning a program and its criteria
for correctness
into a large number of
mathematical formulas, and then trying to prove that all these formulas
always hold.
When the criteria for correctness can be expressed mathematically, this
approach is very useful.  When the criteria for correctness are not
easy to define, the approach is of limited use.
.P
Fortunately, it is quite straightforward to express
mathematically the concept of
a program ``blowing up at run-time.''  Errors that a
run-time system for a language can catch,
such as subscripting out of range, exceeding the allowed range of a
variable, and referencing a variable that has not yet been assigned a
value, are easy to define mathematically.  Unlike a run-time system,
which can only detect these errors when they occur, a verifier can
detect these errors
.I before
they occur.
.P
The first step in any sound verification is to show that no
run-time errors occur.  Some verifiers do not bother with
this step, but assume, for example, that integer variables can contain any
value and that arrays have infinite dimensions.  While such verifiers are
useful as research tools, one cannot place much confidence in a result of
`no errors found' from such a verifier.
.P
Ideally, verifying that a program cannot generate a run-time error should
not require any help from the programmer, since the program
contains sufficient information to decide this question.
However, our
verification tools are not sophisticated enough to do this checking
without help.
.P
The first step of verification is the
translation of the question of the correctness of
a program into a collection of mathematical formulas.
This process is complex, but it is no worse than that of translating
the program into
object code, which is performed by a compiler.
The hard part of verification is constructing proofs that the
formulas are true,
or discovering
that they are false.
.P
Early attempts at verification required users to write out
proofs by hand,
just as mathematicians have done for centuries.
Other people read over the proofs and checked them.
This approach didn't work
very well.  It worked about as well as desk checking of programs does.
A number of supposed program proofs published in various
professional journals have been
found to contain errors.
.P
The next step was automatic proof checking.  In this mode, people worked
out their proofs at computer terminals, with the computer checking each
step of the process.  This approach is reliable, but it is slow and
expensive.  It has not been much used because of the high
labor cost.
.P
Fully automatic generation of proofs is the ultimate goal.
Success has been achieved for certain kinds of problems.
Researchers are trying to extend the range of problems that can be
handled automatically.
There are good, fast techniques for a useful class of simple problems.
Other more powerful (though much slower) techniques can be used on
a larger class of problems.
Research continues in this area.
.P
A number of automatic theorem proving programs have been written.
All verification systems in serious use today use automatic theorem provers.
However,
verifiers are usually unable to
prove programs correct without substantial help from the programmer,
because of limitations of these theorem provers.
It is this fact that keeps verifiers from coming into widespread use.
.P
Fortunately, the mathematics in real-time programs and system programs
tends not to be very advanced.  More statements look like
.DP
.ce
x := x + 1;
.DE
than
.DP
.ce
x := (-b + sqrt(b**2 - 4*a*c)) / (2*a);
.DE
This fact makes verification of these programs tractable even with today's
theorem provers.
.P
We can attempt to verify any condition that can be expressed in the form
of a computable
Boolean expression that is always supposed to be true at some point in
the program.  Such conditions are called
.I assertions.
A typical assertion would be
.DP
.ce
ASSERT x > y;
.DE
One can compare this with a run-time check
of the form
.DP
.ce
IF NOT (x > y) THEN ABORT;
.DE
If correct operation of the program requires that x be greater
than y when control reaches this statement,
we would like to be convinced that
the ABORT will never occur.
A verifier can often generate a proof that the assertion is true
whenever control reaches the ASSERT statement.
.P
Verifiers generally are not very good at diagnosing why a program
cannot be verified.
When a verifier says a program is correct according to
its rules, then either the program is correct
or the verifier is not working properly.
(Sadly, an error in the verifier,
as well as a compiler error resulting in incorrectly generated object code,
are always possibilities.)  However,
when a verifier fails to prove a program correct, the program
may not be in error.  It may simply be that the verifier needs more
help from the programmer, or that some of the help already given is
misleading.  It is not always possible for the verifier to diagnose the
problem in a concise form.  But the verifier can usually point out
which section
of code is giving trouble.  Beyond that, the typical verifier merely lets
the programmer see the formula that it is trying to prove and lets the
programmer try to figure out what is wrong.
This level of diagnostic can be annoying,
but is really no harder than debugging.
.P
Verification is an iterative process.
One submits the program to a verification system, gets some error messages,
fixes the errors, and
tries again until all the errors are gone.
Because verifiers tend to be rather slow, facilities for
reverifying only the parts that have changed are often provided.
This feature is akin to being able to recompile only part of a program
after making a change,
and speeds up debugging substantially.
.P
In time, verification may become a routine part of programming.  At present,
it is an area of active research, but the techniques of verification
have been used only on a few projects.
In most cases, the software
tools needed to use verification on real projects have
been lacking.
This fact has retarded the acceptance of verification as a means of
improving the quality of programs.
.H 2 "The Pascal-F Verifier and its powers"
The Pascal-F Verifier is designed to be used to improve the
reliability of medium-sized real-time programs written in Pascal-F.
Its power is generally adequate to check programs for absence of
run-time errors.
Higher-level constraints may be also be submitted for verification,
and an attempt will be made to verify them, but there are limits on the
complexity of the relations that can be verified.
.P
The Verifier operates on a dialect of Pascal-F that has been augmented
with language features used to provide additional information
to the Verifier.  These extensions are described in Chapter 2.
The assertions required to verify a program are
placed in the program text itself; there is no separate specification
file.  An extended version of the Pascal-F compiler is available which
will accept but ignore the verification
statements, allowing
verified programs to be compiled without change.
.P
The basic units of verification are the procedure, function, monitor,
module, and main program.
Collectively, we call these 
.I "program units."
We also refer to procedures and functions as 
.I "routines."
Each program unit
is verified in isolation, using previously stored information about all
other relevant procedures and variables.
The information stored for each program unit is
.BL
.LI
The name, formal argument list, and result type
.LI
The ENTRY and EXIT assertions
.LI
The INVARIANT assertions
.LI
The list of global variables referenced
.LI
The list of global variables altered
.LI
The list of all routines called, and
the arguments to each call
.LI
Information concerning multiprogramming.
.LE
.P
These items define the
.I interface
of the routine.
The only information about a routine available
when verifying its callers
is the interface.
Therefore all the information about what a routine does must be
included in the EXIT assertions, and all the information about
what a routine needs must be included in the ENTRY assertions.
.P
Before verifying a program, the verifier checks the program for
violations of the rules given in chapter 2.  This is referred to as
.I "preverification checking."  
Once all parts of a program
have passed these tests,
which in themselves may show up program 
``bugs'', the actual verification
process begins.  The Verifier generates assertions for all statements that
could cause overflows, out-of-range subscripts,
references to variables not yet assigned a value, or other run-time errors.
Formulas called 
.I "verification conditions"
are generated for all these assertions and for
the user's assertions.  Attempts are made to prove all the verification
conditions using an automatic theorem prover.  Diagnostic messages are
generated for all unproven conditions.
.P
It is expected that the user will want to alter the program being
verified and to attempt reverification.  The Verifier maintains a
file that minimizes the amount of work required when reverifying a
program.  The units of reverification are the procedure, function, monitor,
and module.
In general a change made within a program unit will not
require reverification of parts of the program outside that unit unless
the interface of the unit is altered by the user.
