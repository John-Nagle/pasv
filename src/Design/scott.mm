.HU "J-graphs and J-code"
We divide the problem of generating verification conditions into
two parts.
We first translate the program to be verified into a representation we
call a J-graph, and then trace out the paths in the J-graph.
We have defined a textual language we call J-code which is used to
represent J-graphs.  The decompiling pass of the verifier generates J-code
from Icode.  The path tracer then parses J-code and builds a J-graph, then
traverses the J-graph to produce verification conditions.
.P
The J-graph is best thought of as a nondeterministic loop-free program.
J-graphs contain the primitives 
.B NEW\c
,
.B SPLIT\c
,
.B WHEN\c
,
.B BRANCH\c
,
.B JOIN\c
,
.B HANG\c
, and
.B REQUIRE.
.P
The 
.B NEW
instruction is a nondeterministic, simultaneous assignment statement.
The format of a
.B NEW
instruction
is
.DS
	NEW  	<variable-list> <formula>	.
.DE
When a NEW instruction is executed, the variables in the 
.I <variable-list>
are upated in such a way as to make the
.I <formula>
true.
If 
.I v
is a formula in the variable list, the notation 
.I v.old
may be used to denote the value of the variable before the
.I NEW
instruction is executed.
.P
Assignment statements and procedure calls are translated to 
.I NEW
instructions in a straightforward fashion.  For example, the assignment
statement:
.DS
	X := X + 1
.DE
is translated to:
.DS
	NEW (X) (X = X.OLD+1)
.DE
To translate a procedure call, the decompiler determines every variable which
could be modified as a result of the call, either by direct modification or
through side effects.
This list of variable forms the 
<variable-list>
of the
.B NEW
instruction.
The 
<formula> is derived from the 
.B EXIT
condition declared with the procedure.
.P
The 
.B SPLIT\c
,
.B WHEN\c
,
.B BRANCH\c
, and
.B JOIN
instructions are used to model flow of control in J-graphs.
.B SPLIT
and
.B WHEN
are used when the flow of control forks off in different directions, and
.B BRANCH
and 
.B JOIN
are used when it comes back together again.  This notation merely 
allows us to represent a loop-free flowchart in a linear fashion.
As an example,
.DS
	IF X > Y THEN M := X ELSE M := M + 1;
.DE
would be represented by
.DS
	SPLIT 	1
	WHEN X > Y    1
	NEW (M) (M = X)
	BRANCH     2
	WHEN NOT (X > Y)    1
	NEW (M) (M = M.old + 1)
	BRANCH 2
	JOIN 2
.DE
All similarly numbered statements are considered to be connected.
.P
The 
.B REQUIRE
statement is used to put verification goals into J-graphs.
The format of a
.B REQUIRE
instruction is
.DS
	REQUIRE <formula>	.
.DE
Whenever we must prove that some formula is always true when control reaches
some point in a program, we put a 
.B REQUIRE 
instruction containing that formula
at the corresponding point in the J-graph.
For each
.B REQUIRE
statement,
the path tracer will trace out all paths from the 
.B REQUIRE 
statement back to
the beginning of the program, and for each path, it will generate and submit
to the theorem prover a verification condition.
.P
Finally, the 
.R HANG
instruction terminates a path.  An execution path which reaches a HANG
instruction is not continued past that instruction. 
.P
J-graphs must be loop-free.  Every execution path which starts at the beginning
of the J-graph must be traced forward to a HANG instruction without reaching
any instruction in the graph more than once.  Since we require users to write
loop invariants for all loops, we are able to break the loop at the invariant
and thus end up with a loop-free graph.  For the program fragment
.DS
	REPEAT
	  s1;
	  INVARIANT p;
	  s2;
	  UNTIL b;
.DE
where
.I s1
and
.I s2
are blocks of code,
.I p
is the loop invariant, and
.I b
is a boolean expression,
the corresponding J-code is
.DS
	SPLIT 	1
	WHEN TRUE	1
	BRANCH 	2

	WHEN TRUE	1
	NEW (<list-of-variables-changed-in-loop>) p
	<J-code for s2>
	SPLIT 	3
	WHEN NOT b	3
	BRANCH 	2

	JOIN 	2
	<J-code for s1>
	REQUIRE p
	SPLIT 	4
	JOIN 	4
	HANG

	WHEN b	3
.DE
(The graph has been simplified for clarity 
by the omission of the statements usually present
for proof of loop termination.)
The loop invariant is proved by induction on the number of times the loop body
is executed.
For the base case of the induction, we must show that 
.I p
is satisfied when the
.B INVARIANT
statement is reached on the first execution of the loop body. 
This case is handled by the path which goes back from the REQUIRE through
the J-code for s1, the
.B "JOIN 2\c"
, the first 
.B "BRANCH 2"
and back to the beginning of the program.
For the induction step, we must show that 
if
.I p
holds when the
.B INVARIANT
statement in the original program,
then when the 
.B INVARIANT
is reached again, P will still hold.
This case is handled by the path from the
.B REQUIRE
back through the
.B "JOIN 2\c"
, the second
.B "BRANCH 2\c"
,
.B "WHEN NOT b\c"
, the J-code for s2, the
.B "NEW"
statement, and back through the
.B "WHEN TRUE 1"
and 
.B "SPLIT 1"
to the beginning of the program.
Finally, the path taken by the final execution of the loop body 
is represented by the path starting at the end of the J-code fragment
and continuing back via the 
.B "WHEN b 3"
statement, through the loop invariant in the 
.B "NEW"
statement, and eventually back to the beginning of the program.
.P
The J-graph thus generated
captures the semantics of the original program.
It is worth noting that our implementation generates J-code 
by a process very similar to
that used for generating machine code in a compiler.
The process of generating J-code is much more amenable to such treatment
than that of directly generating verification conditions, and allows us to
draw heavily on techniques from compiler technology in our verifier
implementation.
.P
.HU "Verification Condition Generation"
The semantics of the language having been captured in the J-graph,
the task of the verification condition generator is primarily that of
tracing out all paths back from every REQUIRE statement back to the beginning
of the J-graph and generating verification conditions during the process.
The verification condition formally expresses the proposition
``If this path is taken through the program then the formula on the
.B REQUIRE
instruction will be satisfied.``.
As soon as the path tracer generates a verification condition, it passes
the formula to the theorem prover.  If the theorem prover cannot simplify
the formula to TRUE, the path tracer displays a diagnostic message
similar to the one below.
.DS
    Could not prove {subscerr.pf:12} 
        (i + 1) - 1 <= 99    (subscript check for "tab" 1..100) 
    for path:
        {subscerr.pf:5} Start of "examine" 
        {subscerr.pf:8} IF->THEN 
        {subscerr.pf:7} IF->THEN 
.DE
The error message
identifyies the point in the program at which the 
.B REQUIRE 
was generated, the proof goal of the
.B "REQUIRE\c"
, and
the explaination as to why the
.B REQUIRE
was generated, and the path being traced.
The user is not ordinarily exposed to the verification condition itself.
Our experience is that the above information is usually sufficient to allow
the user to correct the problem.
.P
The actual verification condition is constructed from formulae on the
.B "WHEN\c"
, 
.B "NEW\c"
, and 
.B "REQUIRE\c" statements and the path.  The first step of the construction
involves renaming of variables.  A unique name is invented for each of the
variables in the variable lists of
.B NEW
instructions along the path.
Then one of these unique names is substituted for every variable in every
formula on the path.  If
.I v
is a variable in a formula, to find the unique name to be substituted for
.I v\c
,search back along the path for the first
.B NEW
instruction that has 
.I v
in its variable list.
The unique name associated with
.I v
in that instruction is used.
.P
Special attention is required when
.I v
occurs in both the variable list and formula of a 
.B NEW
instruction.
Recall that 
.I v
referes to the value of the variable after the 
.B NEW
is executed, and 
.I v.old
refers to the value of the variable before the 
.B NEW
is executed.  Therefore, the unique name for
.I v 
is taken from the 
.B "NEW\c"
, while the unique name for 
.I "v.old"
is taken from the previous 
.B NEW
instruction on the path mentioning 
.I v
in its variable list.
.P
After all the variables have been renamed, the verification condition
is simply
.DS
	p1 and p2 and ... pn implies q
.DE
where
.I q
is the formula in the 
.B REQUIRE
being processed,
and the terms 
.I p1
...
.I pn
are the formulas on all the other instructions on this path.  It is not
strictly necessary to include among the 
.I p
terms formulas from 
.B REQUIRE
instructions passed over during backwards tracing, but we do so to stop
the user from getting an avalanche of diagnostics that all result from
a single error.  In effect, after a 
.B REQUIRE
is passed, it is assumed to be true.  This is valid since the graph is
loop-free.  It is also very effective in reducing the number of diagnostic
messages and in speeding up the proofs.
.HU "Optimizing verification conditions"
The major motivation for our approach to verification condition generation
is the ability to provide good diagnostics.  However, the generation of
a separate verification condition for every
.B REQUIRE\c
/path
pair is expensive, since the number of paths is exponential in the size of
the graph.
Fortunately, we have discovered some techniques to reduce the
amount of computation required to process a J-graph.
.P
The first optimzation we call ``NEW balancing''.  We define a graph to
be NEW balanced if for every variable
.I v
and point
.I P
in the graph, every path from the beginning of the graph to P
contains the same number of 
.B NEW 
instructions with 
.I v
in their variable lists.
If a graph is NEW-balanced, the path tracer does not have to perform a
renaming operation each time it processes a verification condition.
Instead, renaming need only be performed only once, before any verification
conditions are generated.
.P
