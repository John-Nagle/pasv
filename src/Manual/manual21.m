.H 2 "The Verifier's view of Pascal-F"
The Verifier needs additional information beyond the Pascal-F
statements needed to produce a running program.
The basic form of additional information
is the assertion.  An assertion is a Boolean
expression (that is, one whose value is true or false)
that is supposed to be true whenever
some point in the program is reached.
A typical assertion is
.DP
.ce
ASSERT x > y;
.DE
This assertion
is a claim that whenever control reaches the statement, x will be
greater than y.  The Verifier will take on the task of proving that
the code that leads up to the ASSERT statement
always guarantees that x is greater than
y.  Following the ASSERT statement,
the Verifier will assume that x is greater than y.
.P
The Verifier does not execute programs.  It examines programs and attempts
to predict their actions when executed.
This examination takes place based on a set of built-in notions
about the execution environment.
.H 3 "Initialization"
All variables are considered to be uninitialized at start
and may not be referenced before being given a value.
At program start, only constants have values.
Upon entry to a routine, all local variables are uninitialized.
Everywhere that a variable is used, the Verifier will attempt to
prove that the variable has been assigned a value.
For simple variables this task is
usually trivial; for arrays it is usually harder.
See the section on the DEFINED predicate for information on
how to cope with arrays.
.H 3 "Ranges"
All types have finite bounds.  For subrange types, these are explicit.
The bounds of the type
INTEGER
are from -32768 to 32767; these bounds are determined by the 16-bit
hardware arithmetic used in the implementation of Pascal-F.
Everything that has a numeric value thus has a subrange associated with it.
All variables that have been initialized are assumed to be within range.
Hence, all actions that change the value of a variable must
be checked for out-of-range conditions.
.H 3 "Multiprogramming"
The Verifier
views programs as if control can be taken away from the current
process only at certain points called
``singular points.''
The singular points are at SEND and WAIT statements
and places where a process
calls a routine exported from a different monitor.
This assumption allows the Verifier to process all other parts
of programs as if they are purely sequential.
.P
This assumption is a simplification of reality, since
processes can be pre-empted
when interrupts occur.
However, since no
monitor is allowed to access a variable
that is declared in a different monitor,
the fact that there is preemption can be ignored
because the
preempted process cannot detect that it was preempted.
.P
Normally, the Verifier can assume that any variable not altered by
a statement does not change.
When a process P encounters a singular point, this assumption is not
valid.
At those points P can be interrupted
by other processes that can alter variables that P is allowed to read.
When a process
reaches a singular point,
monitor variables visible to that process are assumed to be
given new values that are
consistent with the monitor invariant.
Thus, the monitor invariant must be strong enough
to contain all the necessary information about the
values the monitor variables will contain whenever the
monitor is entered or left.
.P
The restrictions necessary to make this simplified model of multiprogramming
work are enforced by the Verifier
during the pre\%verifi\%cation checking phase.
A style of programming in which
shared variables are accessed only by routines declared
within monitors is required.
This style of programming is strongly recommended by
various structured programming enthusiasts, including Hoare
[HOARE74]
but it tends to result in programs with many tiny routines.
Efficient implementation of such programs will require additional
optimization support in the compiler.
.H 3 "Devices"
In Pascal-F, devices have the syntax of variables, but very different
semantics.  The Verifier treats devices as procedures.
Given the definitions
.DP

    TYPE atod = DEVICE              (* A to D converter *)
                    channel: 0..15; (* used to select channel *)
                    data: 0..2047;  (* returns data value *)
                END;
.DE
the program fragment
.DP
        atod.channel := 2;          (* select A/D channel *)
        tab[2] := atod.data;        (* read A/D value *)
.DE
is treated by the Verifier much as if the program read
.DP
        atodchannel(2);             (* select A/D channel *)
        atoddata(tab[2]);           (* read A/D value *)

.DE
.P
and
.B "atodchannel"
and
.B "atoddata"
were procedures.
The Verifier makes no assumptions about the values returned by devices
other than that
values returned from devices are assumed to be in the range declared in the
DEVICE declaration.
Thus, DEVICE ranges should include the entire range of values that
the device is electrically capable of producing.
.I "The Verifier assumes that the hardware has been\
 correctly described by the programmer."
.H 3 "Enforcement"
In this chapter are a number of restrictions on the way in which
Pascal-F programs intended for verification may be written.
The Verifier checks and enforces all of these restrictions before
proceeding with verification.  This step is referred to as
pre\%verifi\%cation checking.
.H 2 "Extensions to Pascal-F for verification"
.H 3 "Verification statements"
The statements described here have no effect on the
behavior of the program.  They serve only as a documentation
aid, to help the Verifier (and the human reader) understand
the operation of the program.
.P
Because verification statements are not allowed to call
have any effect on the program, they cannot contain
calls to functions that have side effects.
.H 4 "The ASSERT statement"
.DP
.ce
ASSERT  ( <Boolean expression list> ) ;
.DE
.P
This is the basic verification statement.  An ASSERT
statement is a claim that the given expressions are true whenever
control reaches the statement.  The Verifier will attempt
to prove this claim.
Like all verification statements, the ASSERT statement
has no effect on program execution.
.H 4 "The STATE statement"
.DP
.ce
STATE  ( <Boolean expression list> ) ;
.DE
The STATE statement may only be written within the body
of a loop, and represents the
.I "loop invariant"
of the loop.
The Boolean expressions in the STATE statement must
be true every time the STATE statement is reached.
The Verifier uses the STATE statement as the place at which it will
begin and end analysis of the loop.  Each Boolean expression in
the STATE statement is verified for the path around the loop and
for the path into the loop.  For the path around the loop, the path
tracing starts at the STATE statement, goes around the loop once (backwards)
and ends at the same STATE statement.
Thus, the Boolean expression list must include
all important information
about variables that are changed by the loop body,
but it need not mention variables that are left untouched.
More advice on the use of the STATE statement appears in the section
``What to do when a verification fails''.
.P
One (and only one)
STATE statement must be written for every loop.
The statement must contained in the loop body,
at the top level.
That is, if the loop body contains any compound statements then
the STATE statement for the loop must appear outside those statements.
.H 4 "The MEASURE statement"
.DP
.ce
MEASURE ( <numeric expression> ) ;
.DE
.P
The MEASURE statement is used to prove that loops terminate.
Every WHILE and REPEAT loop (except for deliberate infinite loops,
which must begin with
"WHILE true DO" or end with ``UNTIL false;'') must contain a MEASURE statement.
The MEASURE statement must immediately follow the STATE
statement associated with the loop.
The <numeric expression> is a limit on the number
of iterations of the loop body left to be performed.
More specifically, the
Verifier will insist that the value of the expression
.BL
.LI
be greater than or equal to zero when control reaches the MEASURE statement,
and
.LI
decrease by at least one (for integers) or the precision of the
expression (for fixed point numbers)
each time control flows from the MEASURE statement around the
loop back to the STATE statement.
.LE
.P
How complex a MEASURE statement is required is a function of
how obvious it is that the loop always terminates.
FOR loops always terminate, and no MEASURE statement is used.
If a loop uses a counter of one form or another,
the MEASURE statement will contain a simple expression involving
the counter.
For example, if the loop is counting i from 1 to 100,
"MEASURE\ 100\-i" would be a good choice.
If the fact that a loop terminates is subtle,
a complicated MEASURE statement,
perhaps one involving EXTRA variables, will be necessary.
If the loop does not terminate under some circumstances,
no MEASURE statement, no matter how complex, will be accepted
by the Verifier.
.H 2 "New expressions"
The Verifier allows the use of some new constructs for building
expressions.
Some of these constructs can be used
only in the special verification that will be described;
others can be used in executable statements as well.
.H 4 "The DEFINED predicate"
.DP
.ce
DEFINED(<variable>)

.ce
DEFINED(<array>,<low bound>,<high bound>)

.ce
DEFINED(<block name>)
.DE
DEFINED is a generic, built-in function that
can be applied to any variable or part thereof and
returns a Boolean value.
DEFINED cannot be used in executable statements.
A variable is said to be DEFINED whenever
it is guaranteed to have a meaningful value.  The following
rules are used to determine whether an expression is DEFINED:
.BL
.LI
All constants are DEFINED, including VALUE constants.
.LI
Values obtained from DEVICE variables are DEFINED.
.LI
A record variable is DEFINED if and only if all the fields of the
record are DEFINED.
.LI
An array variable is DEFINED if and only if all the entries of the
array are DEFINED.
.LI
A variable is DEFINED after it has been used on the left side of an
assignment statement in which the right side was DEFINED.
.LI
At the beginning of the body of a FOR loop, the index variable is DEFINED.
.LI
A subscripted variable is DEFINED if the subscript is DEFINED, the
subscript is in the range of the array bounds, and the specific
array element being referenced is DEFINED.
.LE
.P
Once a variable is DEFINED it remains DEFINED, unless an
operation is performed that results
in the variable receiving an indeterminate value.
Only the following circumstances
can result in a variable not being DEFINED:
.BL
.LI
The variable is used in an index statement of a FOR loop is
not DEFINED when the loop terminates.
.LI
Immediately after the tag field of a variant record is changed,
all the other fields in that record are no longer DEFINED.
.LE
.P
The second form of DEFINED is used to test the definedness of portions
of an array.  The form
.DP
.ce
        DEFINED(tab,i,j)
.DE
is true if the array
.B tab
has all elements DEFINED for elements with subscripts between
.B i
and
.B j
inclusive.
This form is often used in loop invariants, inside the STATE statement.
.P
The DEFINED predicate can also be applied to monitors and modules,
as in the third form given above,
but it has a slightly different meaning.  If an INVARIANT has been
declared for the monitor or module,
the invariant is expected to hold whenever
the construct is entered or left.
There must be one exception to this
rule, however.  Since no variables
are DEFINED when the program begins execution,
the invariant cannot be expected to hold.
It is the responsibility of the statements in the body of the
monitor or module to initially establish the invariant.
If m is a monitor or module, then DEFINED(m) is TRUE
whenever m has been initialized.
No routine exported from a monitor or module may be called unless
DEFINED(m) is true.
.H 4 "The OLD annotation"
At the beginning of every procedure and function,
the Verifier implicitly saves the value at entry of every variable
used as an input to the routine.
In assertions, it is possible to refer to these values by placing
an
.B .OLD
following a variable name or selector expression.
Thus, if X is
a parameter or variable,
.B X.OLD
denotes the value the parameter or variable had when the
routine was entered.
Old values of array variables are
referenced with forms such as
.B X.[3].OLD
for an array reference,
or
.B X.FIELDNAME.OLD
for field references.
OLD may only be used within verification statements
and PROOF statements, since this saving of OLD values does not occur in
the running program.
.P
The chief use of OLD is as a specification tool.
For example, suppose one wanted to write a procedure to
increment the value of a variable by one.  The interface
for this procedure might be
.DP
.ce 100
TYPE smallint = 1..100;          \&
PROCEDURE bump(VAR x:  smallint);\&
ENTRY x < 100;                   \&
EXIT x = x.OLD + 1;              \&
.ce 0
.DE
.H 4 "The IMPLIES operator"
A new Boolean operator IMPLIES is added to the language.
Informally, the expression
p\ IMPLIES\ q means,
"if p is TRUE,
then q will be TRUE as well."  If p and q are Boolean expressions,
then p\ IMPLIES\ q has the
value TRUE if p is FALSE or q is TRUE, and it has the value FALSE
in all other cases.  For purposes of precedence,
IMPLIES is considered to be a relational operator.
.P
The IMPLIES operator is for the most part
used in verification statements, but it
may be used in executable statements as well.
Because it happens to be the case that FALSE\ <=\ TRUE,
every IMPLIES operator in Pascal-F can be replaced by
the operator <= without changing the meaning of the program.
This practice is not advised, since the resulting program
will be harder to read than the one using IMPLIES.
.H 3 "Verification declarations"
To verify a program, more information about each routine is needed
than what is necessary to compile a program.  The Verifier
expects routine headers that are more complex than those of standard Pascal.
The extended syntax is:
.DP
        <block> ::= <entry declaration part>
                    <exit declaration part>
                    <effect declaration part>
                    <invariant declaration part>
                    <depth declaration part>
                    <constant declaration part>
                    <type declaration part>
                    <variable declaration part>
                    <statement part>
.DE
Each declaration part is either empty,
or consists of a keyword followed by a series of declarations,
each of which is followed by a semicolon.
The declarations that are not part of standard Pascal are explained
in this section.
.H 4 "The ENTRY declaration section"
.DP
.ce
ENTRY <Boolean expression series>;
.DE
.P
The Boolean expressions given in this section are
assertions that must be TRUE whenever the routine is called.
The scope rules of Pascal-F prevent
the Boolean expressions from containing variables other than
parameters of the routine and variables global to the routine.
In a routine exported from a module,
ENTRY assertions cannot use variables local to the module.
The purpose of the ENTRY section is to state
restrictions on the values of these variables.
.P
ENTRY assertions are a form of documentation as well as a verification
requirement.  Instead of writing "This procedure may only be called
when filesopen is 0" in a comment, one writes
.DP
.ce
ENTRY filesopen = 0;
.DE
.P
Some ENTRY assertions are automatically generated for
each routine.
.BL
.LI
For each value argument,
an assertion is generated that the argument is DEFINED.
.LI
For routines that are exported from a monitor or module m,
an ENTRY assertion is generated consisting of
the invariants for m.
.LE
.P
Whenever a variable is given a value, the Verifier checks
that the value is appropriate to the type of the variable.
Therefore, any variable that is DEFINED has a value that is
appropriate to its type.
Therefore is not necessary to write ENTRY
assertions that state that a parameter has a value appropriate
for its subrange type.
.P
When a variable
.B v
is passed to a
.B VAR
parameter
.B p\c
,
and used as an input variable, the Verifier automatically adds
.B "DEFINED(p)"
to the
ENTRY
conditions of the routine being called.
Similarly, for a
VAR
output variable,
.B "DEFINED(p)"
would be added to the
EXIT
conditions.
This automatic insertion of
.B DEFINED
predicates can be overridden by mentioning the formal argument
involved in a user-supplied
.B DEFINED
in an
ENTRY
or
EXIT
condition.
A common case in which the user must override the
Verifier's automatic insertion is shown below.
.DP
        PROCEDURE search(var n: integer);
        ENTRY DEFINED(n) = DEFINED(n);
        EXIT DEFINED(n);
        BEGIN
            n := 0;
            WHILE (n < 100) AND (tab[n] <> 0) DO BEGIN
                STATE(defined(n), defined(tab));
                n := n + 1;
                END;
            END;
.DE
Here, we do not want to require that
.B n
be DEFINED at entry to
.I search,
but
.B n
appears to be an output variable since there are references to it
within the procedure.  The use of
.DP
.ce
DEFINED(n) = DEFINED(n)
.DE
prevents the Verifier from adding a requirement that
.B n
be DEFINED at input, but does not impose any requirement of its own.
.P
There are rare cases in which a parameter p is only sometimes
used for input
depending on the values of the other parameters are.
In this case, an ENTRY assertion of the form
.DP
.ce
C IMPLIES DEFINED(p)
.DE
should be used, where C is the condition under which the variable
is read.
Routines that have array or record VAR parameters may
need complex ENTRY assertions indicating which portions
of the array must be DEFINED at entry.
.P
Monitors and modules may have ENTRY declaration sections.
These refer to the initialization block of the monitor or module.
For monitors
and modules, the assertions in the ENTRY section must be true when
the INIT statement for the monitor is executed, and will be
assumed true at the beginning of the initialization part of the block.
.H 4 "The EXIT declaration section"
.DP
.ce
EXIT <Boolean expression series>;
.DE
.P
The
EXIT
declarations define the state of the program upon exit from a
routine by giving assertions that will be true at that time.
The scope rules of Pascal-F
limit the variables allowed in an
ENTRY
assertion to parameters
and variables global to the routine.  An additional restriction
enforced is that value parameters cannot appear in an
EXIT
assertion,
except as fields of the record
.B OLD.
.P
EXIT
assertions tend to be long and complex,
because everything that the
routine does that is of importance to the caller must be
described in the
EXIT
assertions.
In some cases the
EXIT
assertions
may be as long as the body of the routine.
.P
If a routine has a
VAR
parameter p that is used for output,
.B "DEFINED(p)"
must be included among the
EXIT
assertions.
If the parameter is only used for output some of the time,
the situation must be described in an
EXIT
assertion
of the form
.DP
.ce
C IMPLIES DEFINED(p)
.DE
where C is the condition under which p is used for output.
If
.B p
is an array or record and only part of
.B p
is written, the situation must be described using more complex
.B EXIT
assertion.
.H 4 "The EFFECT declaration section"
.DP
.ce
EFFECT <Boolean expression series>;
.DE
This declaration may only appear in a routine that is
exported from a module.  An EFFECT assertion is similar
to an EXIT assertion, except that EFFECT assertions cannot
contain references to variables local to the module.
.P
Upon return from a ``regular'' routine (that is, one that is not
part of a module) the Verifier uses the fact
that the
EXIT
assertions (with actual arguments replacing
formal parameters) for that routine are true.
However, if the routine is exported from a module and called
from outside the module,
its
EXIT
assertions cannot be used since they may contain
variables not visible to the caller.
Instead, the
EFFECT
assertions are used by the Verifier.
These assertions cannot contain variables local to the monitor.
If a routine exported from a monitor is referenced from within
the monitor, both
EXIT
and
EFFECT
assertions are used.
.P
The following example shows how
OLD,
ENTRY,
EXIT,
and
EFFECT
are used to specify a module.  The module also contains an
INVARIANT
declaration.
INVARIANT
declarations are explained in the next section.
.DP
    MODULE stack;
    EXPORTS size, push, pop;
    VAR stp: 0..maxsize;
        buf: array [1..maxsize] of stackelt;

    EFFECT size = 0;

    INVARIANT FOR x: 1..maxsize
       ALL x<stp IMPLIES DEFINED(buf[x]);

    PROOF FUNCTION size: 0..maxsize;
    EXIT   size = stp;
    BEGIN  size := stp END;

    PROCEDURE push(x: stackelt);
    ENTRY  size < maxsize;
    EFFECT size = OLD.size + 1;
    BEGIN  stp := stp + 1;
           buf[stp] := x
    END;

    FUNCTION pop: stackelt;
    ENTRY  size > 0;
    EFFECT size = OLD.size - 1;
    BEGIN  pop := buf[stp];
           stp := stp - 1
    END;

    BEGIN stp := 0 END;
.DE
A few things should be noted about this example.
The
ENTRY
assertions indicate under what conditions
each routine can be called.  They are written in terms of
the proof function size.  The
EFFECT
declarations describe
what each routine does to size so that the users
of the module can tell when it is valid to call
.B push
and
.B "pop."
The
EXIT
assertion in size is used by the Verifier
to translate the
ENTRY
and
EFFECT
assertions in
.B push
and
.B pop
into assertions about the variable
.B "stp\c"
, which is
(unbeknownst to the caller) changed
by those routines.
.H 4 "The INVARIANT declaration"
.DP
.ce
INVARIANT <Boolean expression series>;
.DE
.P
The assertions given in an INVARIANT declaration are required to hold at
multiple points in a program.
.BL
.LI
Putting an INVARIANT\ P in a routine is equivalent to putting
ASSERT\ P statements at the beginning of the routine
(i.e. as an ENTRY assertion),
the
end of the routine
(as an EXIT assertion),
before and after each WAIT statement in the routine,
and before and after each call to another routine.
.LI
Putting an INVARIANT P in a MONITOR
is equivalent to putting
ASSERT\ P statements
at the end of the monitor block,
before and
after each WAIT statement in the monitor, before and after each
call to a routine outside the monitor, and adding P as
an
INVARIANT
assertion to each routine exported from the
monitor.  Note that an ASSERT \P is not added to the beginning
of the monitor block; the invariant is not assumed to hold
until the monitor has been initialized.
.LI
If the declaration INVARIANT P is put into a MODULE,
the only variables on which P can depend are those local
to the MODULE.
The assertion P must be true
at the end of the initialization block of the MODULE,
before and after each WAIT statement in the module,
before and after each
call to a routine outside the module, and at the beginning
and end of each routine exported from the MONITOR.
.LE
.sp 1
In other words, if an assertion P is declared to be
an INVARIANT of a construct, P must be true whenever
control passes into or out of that construct.
.H 4 "The DEPTH declaration"
A recursive routine R1 is one that can call R2, which in turn
calls R3, and so on until R1 is called again.
A special case of recursion is a
routine that calls itself.  When a program uses recursion, it is
necessary to prove that it is impossible to initiate an endless
sequence of routine calls, none of which ever returns.
.P
Every recursive routine must contain a DEPTH declaration of
the form:
.DP
.ce
DEPTH <integer expression>;
.DE
By the scope restrictions of Pascal-F, the integer expression
may only contain parameters of and variables global to the
routine.  Note that a fixed point expression cannot be used.
.P
The DEPTH expression is an indication of how much time will be used by
the routine.  The ENTRY assertion for
the routine must be strong enough to imply that the DEPTH expression
is nonnegative whenever the routine is called.
.P
From a recursive routine R1 two kinds of calls to other routines
are possible.
If R1 calls itself, or if it calls a routine R2 that can initiate
a chain of routine calls that eventually leads to R1 being called
again, the call is said to be a recursive call.
Otherwise, the call is said to be nonrecursive.
.P
Wherever a recursive routine R1 makes a recursive call to a routine R2
(a special case is when R1 and R2 are the same routine),
it must be established that
an infinite chain of calls is not being initiated.
Let d1 be the value (at the time R1 was called) of the DEPTH
expression declared for R1.  Similarly, let d2 be the value
(at the time R2 is being called) of the DEPTH expression declared
for R2.  The Verifier will attempt to prove that
d2 is strictly less than d1.
If this condition is proved
for every recursive call, then each
call in a chain must perform a successively easier task,
so that every use of
recursion must eventually terminate.
.H 3 "EXTRA variables and PROOF statements"
Quite often, it is not easy to demonstrate to the Verifier that a program
works.  One method of simplifying this task
is to demonstrate that a program with some
`debug code' added works.  For example, in proving that a linked list
is correctly linked, it is easier to prove that a doubly-linked
list is correct than that a singly-linked list is correct, because
in the doubly-linked case, an invariant can be stated that claims that
the operations of inserting and deleting from the list keep
the backward and forward links consistent.  Consistency for a
singly-linked list is harder to define.
.P
In a case such as the above, implementing a doubly-linked list may not
be necessary for the operation of the program, but may be
desirable for verification.
EXTRA variables, for use in assertions,
and PROOF statements, for manipulating EXTRA variables,
are useful in such situations.
.H 4 "The EXTRA variable attribute"
.DP
.ce
<variable name list>: EXTRA <type>;
.DE
The EXTRA attribute may be used in
VAR declarations, in formal parameter declarations, and in RECORD
definitions.
The type of the result of a function may not have the EXTRA attribute.
In each case, the attribute denotes a variable, parameter, or
record field that is to be used for proof purposes only.
That is, EXTRA variables must be used in such a way that
all the EXTRA information can be removed from a program
without affecting its execution.  See the section on PROOF statements
for more details.
.H 4 "The EXTRA function and procedure attribute"
.DP
.ce
EXTRA FUNCTION <function definition>

.ce
EXTRA PROCEDURE <procedure definition>
.DE
Functions and procedures
designated as EXTRA are solely for use in PROOF statements and
assertions.
Every parameter to an EXTRA routine is implicitly an EXTRA
variable, and every statement in the routine is implicitly
a PROOF statement.
.H 4 "RULE functions"
.DP
.ce
RULE FUNCTION <function definition> BEGIN END;

.DE
Functions may be declared as RULE functions for use in proof rules.
Such function definitions have no body.
.P
Rule functions are used when an expression is needed
in an assertion but the needed expression cannot be written as
a simple Pascal-F expression.
The use of rule functions is covered in detail in the chapter
on rules.
.P
Rule functions are restricted to results of types
.B integer,
.B char,
and
.B boolean.
Arguments to rule functions may be declared as any valid Pascal-F type.
.H 4 "The PROOF statement"
.DP
.ce
PROOF <statement>
.DE
Any executable statement, even a compound statement,
may be turned into a PROOF statement by preceding
the statement with the keyword PROOF.  Such statements are ignored by the
compiler.  PROOF statements are used to manipulate EXTRA variables and
to control conditional execution of other PROOF statements.
The removal of all PROOF statements and EXTRA variables
from a program must not change the execution of the program.
The Verifier checks this restriction by enforcing rules
that prevent any PROOF statement from affecting any non-PROOF variable.
.P
Any expression containing an EXTRA variable will be referred to
as a `proof expression'.
The specific restrictions imposed to prevent PROOF statements and expressions
from affecting program execution are as follows.
.BL
.LI
Proof expressions may not contain calls to non-EXTRA
functions that have side effects (other than the modification of
EXTRA variables).
.LI
Proof expressions can only be used in PROOF statements and as
arguments to routines.  The following parameter matching rules
must be observed when calling non-EXTRA routines:
.BL
.LI
If a formal value parameter of a routine is an EXTRA variable, the
corresponding argument cannot have any side effects (other than the
modification of EXTRA variables).
.LI
If a formal VAR parameter of a routine is an EXTRA variable,
the corresponding argument must also be an EXTRA variable.
.LI
If a formal parameter is not an EXTRA variable, the corresponding
argument must not contain any EXTRA variables.
.LE
.LI
EXTRA functions and procedures may be called only from within
PROOF statements.
.LI
Non-EXTRA variables may not be modified in any way by
PROOF statements.  This restriction applies to assignment statements,
FOR loops, and routine calls.
.LI
The multiprogramming statements WAIT, SEND, and INIT are forbidden in
PROOF statements.
.LE
.sp 1
.ig
.H 4 "When and how to use EXTRA variables"
This is a difficult subject to discuss in the abstract.
..
Study of the sample engine control program in Chapter 4, which contains
a number of EXTRA variables, will give some insight into
the use of this language feature.
.H 2 "Restrictions on Pascal-F programs"
The restrictions given here are imposed to make the task
performed by the Verifier
easier.  In most cases, the restrictions are in line with good
programming practice.  However, the concern here is not style but
verifiability.  A construct is prohibited only when there is some
specific problem in handling that construct.
.H 3 "Restrictions on program structure"
.BL
.LI
The only legitimate ways to write non-terminating
loops for a process is
.DP
.ce
WHILE true DO BEGIN <loop body> END;
or
.ce
REPEAT <loop body> UNTIL true;
.DE
This construct may not appear within any other control structure.
This restriction is imposed so that the Verifier can discriminate between
accidental and deliberate non-terminating loops.
.LI
The iteration variable of a FOR loop may not be modified from
within the loop.  Further, the value of the iteration variable is not
DEFINED after the loop terminates.  The Verifier issues an error
message whenever a variable that is not DEFINED is accessed.
.LE
.H 3 "Restrictions on variant records"
Variant records are not permitted by the verifier.
.H 3 "Restrictions on exception handling"
Pascal-F incorporates a powerful exception handling mechanism
similar to that in Ada.
This version of the Verifier operates on the assumption that
exception handlers are not used in normal operation and
enforces the following rules.
.BL
.LI
The RAISE statement, for exception handling, is considered to be a
mechanism for recovering from hardware errors only.  The Verifier will
try to prove that no RAISE statement is ever executed.
.LI
Exception handling routines are not examined.
.LE
.H 3 "Aliasing and side effects"
.P
Aliasing and side effects are two related phenomena that
make programs difficult to understand and verify.
Aliasing is the condition in which two names refer to the
same variable.  A side effect occurs when a function
modifies one of its parameters or a global variable.
The Verifier enforces restrictions that prevent variables from being
changed
in a fashion that it cannot detect,
or in such
a fashion that the resulting value of the modified variable
depends of the order of evaluation.
.P
The following program contains an example of aliasing.
.DP
    PROGRAM pr;
    VAR x: 1..100;

    PROCEDURE p(VAR a: 1..100);
    BEGIN
        a := 1;
        x := 2;
        ASSERT(a = 1);
    END;
    BEGIN
        p(x);
    END pr.
.DE
.P
Procedure p, viewed in isolation, is valid.
The variable  `a' is clearly 1 after the body
of the procedure has been executed.
However, the call `p(x)' causes the variables
`a' and `x' to refer to
the same variable within the procedure `p'.  The assignment
to `x' will therefore  change `a' at the same time, and the
assertion `a\ =\ 1' will not be valid.
.P
Because this sort of thing is far more often a cause of error than a
useful feature of the language, it is prohibited.
.P
When a function is called,
VAR parameters of the function
or variables global to the function can be modified.
The modification of a variable by a function is called a "side effect"
because the modification is usually not the primary purpose of
the statement in which the function is called.
Generally, programming with side effects is a dangerous practice
because it is easy to forget that the side effect will take place.
.P
This danger is diminished when using the Verifier, since the
Verifier will detect side effects.  However, there is
a class of side effects whose result depends on the order
of evaluation in expressions.
As an example, suppose the functions
f and g set their arguments to 1 and 2, respectively.
The value given to x by the statement:
.DP
.ce
y := f(x) + g(x)
.DE
depends on whether the call to f or g is evaluated first.
The Verifier allows statements to have side effects
only if it can determine that the results of the
side effects are independent of the order of evaluation
in expressions.
.P
The programmer can use the following simple rules
to avoid problems with aliasing and side effects.
.AL
.LI
Do not pass the same variable to two different
VAR parameters of a routine.
.LI
Do not pass a variable or any component thereof
to a VAR parameter of a routine that sets or uses the variable as a global.
.LI
Within a function,
do not modify VAR parameters or global variables,
perform WAIT statements, or call a routine
exported from a monitor, unless the function is called only in
simple assignment statements.
.LI
Do not use functions exported from monitors in expressions;
use them only as the right side of simple assignment statements.
.LE
The Verifier detects any violations of these rules.
.H 3 "Restrictions on multiprogramming"
The restrictions required to make programs with multiple processes
verifiable are somewhat severe.  Unlike the other restrictions, which
generally prohibit only language forms of little if any real use, the
multiprogramming restrictions can be difficult to live with.
The object of these restrictions, as discussed
earlier in this chapter,
is to allow the Verifier (and the programmer) to
generally ignore the fact that semi-concurrent operations are taking
place.
.P
The major restriction required to make multiprogramming
work is to require that any code referencing a static variable must be at the
same priority as the variable.  In Pascal-F, this is easy to enforce,
because both the priority of both code and static variables is determined
strictly by the priority of the module in which they are enclosed.
The rules are as follows.
.BL
.LI
No variable may be imported or exported from a monitor.
.LI
A routine that has been exported from the monitor may not
be called before the monitor has been initialized with an INIT statement.
.LI
A monitor variable may not be passed as a VAR argument to
a routine outside the monitor.
(There are no restrictions on arguments passed by value.)
.LI
A signal may not be a component of any other type.
.LE
.H 2 "Using Pascal-F with verification statements"
.H 3 "Guidelines for writing verifiable programs"
.BL
.LI
Every variable should be of the minimum subrange type required for
the range of values needed.
For example, if negative values are illegal for a variable, its type
should not permit negative values.  Likewise, if zero is also prohibited, a
positive range should be specified.  These declarations
give the Verifier extra information
to use without extra writing by the programmer.
They also can save space in the object program.
.LI
Where alternatives are mutually exclusive, use
the IF - THEN - ELSE structure in preference to consecutive
IF statements so that it is clearly impossible
for more than
one alternative to be executed.
Properly structuring a program holds down the number of
possible paths to be traced out.
Large numbers of paths make for long
sessions with the Verifier.
.LI
Use many short assertions instead of a few big ones.
Do not lump assertions
together with AND operators.  The diagnostic messages,
which refer to individual assertions, will then be more useful.
.LI
Because many diagnostic messages are line-number oriented, it helps
to put only one assertion on a source line.
.LI
When writing loops, remember to use a STATE statement to describe
what the state
is after each iteration.  Keeping the loop simple
eases the difficult task of writing loop state assertions.
.LI
The Verifier can make better deductions
about addition and subtraction than it
can
about multiplication and division.  It will be quite difficult
to verify anything
that depends on more than the most obvious properties of
multiplication and division.
Multiplication by constants is not a problem.
The Verifier can deduce the possible range of the result of a
division, but little else.
.LI
Avoid scaling fixed-point operations so that truncation occurs.
The Verifier can deduce the possible range of the result of a truncation,
but little else.
.LI
When progressing through an array with a loop, use a FOR loop unless
early exit from the loop is planned.
The Verifier supplies the proofs that FOR loops terminate.
.LI
When processes at different priorities must communicate, one process
will have to call a routine in another module
to access any shared variables.  This restriction
is an incentive to reduce access to
shared variables.
.LI
Do not combine data that logically belongs to different priority code
in the same record.
Doing so will force the use of access routines unnecessarily.
.ig  \"DELETE this item
//.LI
//When possible, use `simple' functions.
//Simple functions are considered to be those with no
//references to global variables,
//no VAR
//arguments, no priority changes, and no SEND or WAIT statements.
//For simple functions, the Verifier is able to use the fact that
//given the same arguments, the function always returns the same result.
//For non-simple functions, the Verifier will not use this fact,
//even if it happens to be true.
..
.LE
.H 3 "Obtaining the most from the Verifier"
Having a Verifier around encourages ``defensive programming''.
Good programmers often write error detection into programs.
Unfortunately, when program space or time are at a premium, it is
not possible to put in (or in some cases leave in) all the traps for
software bugs that should be there.
When these traps are written as verifiable assertions, the checking can be
done during verification, and there is no execution penalty.
.P
In this sense, the most important defense against errors is
extensive use of ENTRY assertions.
Use ENTRY assertions as a documentation aid, to explain each routine
to its users.  These assertions (and module INVARIANT assertions)
can be used
to protect the routine from its callers, since the Verifier requires that
the state of the system is what it is supposed to be when the
routine is called.
.P
Once all these ENTRY assertions are verified, they should be left in
the program as a guide for those who must maintain the program.
Over the long haul, one of the most important benefits of verification is
that it allows the designer of the original program to leave behind
rules about how the program is supposed to work.  These rules might be
unknown, forgotten, or ignored by future maintenance programmers,
but if they are in the program text, the Verifier will use them.
