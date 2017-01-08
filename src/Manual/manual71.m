.H 2 "Introduction"
Using the Rule Builder requires a substantially greater
level of expertise in both formal mathematics and
verification than does using the Verifier proper.
.P
The Rule Builder is a version of the Boyer-Moore theorem prover
initialized with a knowledge base compatible with the Verifier's built-in
knowledge.  It is a difficult program to use, and requires some
training in formal logic to use
successfully.  Not all users of the Verifier need be familiar with the
Rule Builder, but users must have access to someone with this expertise
for assistance when a verification requires a new rule.
.H 3 "Required reading"
Before attempting to use the rule builder, it is necessary to become
familiar with three documents.
The first of these is the book
.I "A Computational Logic,"
by Robert S. Boyer and J. Strother Moore, published in 1979 by
Academic Press, New York, NY.
This book describes the theory upon which the prover is based.
Chapters 1, 2, and 3 should be studied closely, and reading the
entire text while trying out some of the examples is advisable.
.P
Some knowledge of the Franz Lisp implementation of Lisp is required.
The reference manual is provided with the Berkeley UNIX distribution,
and is also available from Franz, Inc. of Berkeley, California.
However, anyone with a working knowledge of Lisp will probably not require
this manual.
.P
Finally, the manual
.I "A Theorem Prover for Recursive Functions: A User's Manual"
published in 1979 as Report CSL-91 by SRI International of Menlo Park, CA.,
is useful.  This manual describes the mechanics of running the theorem prover.
The key parts have been extracted and appear in the next few pages,
but the theorem prover user's
manual goes into more
detail and should be on hand.
.P
The prover has changed somewhat since the 1979 manual.  We will try to
cover the important differences in this manual.
.H 2 "An introduction to the Rule Builder"
In
.I "A Computational Logic"
[BOYER79],
Boyer and Moore describe a formal logic based on recursive functions,
and they present a large number of techniques for discovering proofs
in that theory.  These techniques are implemented in their theorem prover.
This chapter of the verifier manual is an adapted version of
Boyer and Moore's theorem prover manual.
.H 3 "Teaching the theorem prover"
While using the rule builder the user will spend most of his time teaching the
system about the concepts he defines and their relationships to other
defined concepts.  The system is taught by defining functions and suggesting
lemmas for it to prove and remember for future use.
.P
The system uses axioms and previously proved lemmas in four distinct
ways.  The system does not decide automatically how to use a given theorem;
whenever any new theorem is introduced, the user must specify how the lemma is
to be used by providing the system with a list
names drawn from the following keywords.*
.FS *
.R
The
.I induction
lemma type has been discontinued.
It is now necessary to use manually-provided
.I hints
to help the prover with difficult inductions.
.FE
.VL 22 2
.LI "rewrite"
lemmas are used to rewrite terms.  Most lemmas are rewrite lemmas.
.LI "elim"
lemmas are used to replace certain complex terms by
single variables.
.LI "generalize"
lemmas are used to guide the theorem prover when it looks for stronger
induction hypotheses.
.LE
.P
Any lemma with an empty list of lemma types will never be used again by
by the system.
.P
The theorem prover is very sensitive to the syntactic form chosen by the user
to represent each new fact.  For example, a rewrite lemma of the form
.DS

.ce
(implies (and p q) (equal r s))

.DE
is used to rewrite instances of
.I r
to
.I s
provided that the system can first establish
.I p
and then
.I q.
This is the most common form of lemma.
Note the asymmetry between hypothesis and conclusion, and between left and
right hand sides of the conclusion.
In fact, because the system must limit the resources it is willing to
spend establishing
.I p
and
.I q,
even the order of the hypotheses is relevant to the system.
Thus, the above rewrite lemma causes different behavior than any of the
following logically equivalent formulae:
.DS

.ce
(implies (and p q) (equal s r))

.ce
(implies (and p (not (equal r s))) (not q))

.ce
(implies (and q p) (equal r s))

.DE
.P
To become an effective user of the system one must understand how the
commands influence the behavior of the system.  It is possible to infer the
meaning of the various lemma types after enough hands-on experience with the
system.
(Boyer and Moore add the comment here
``It is also possible to infer the structure of
a brick wall by battering
it down with your head''.)
.H 4 "Events, Dependencies, and Commands"
The insertion of a definition or lemma into the knowledge base is
called an
.B "event."
All events have names.
Some events, such as definitions of new functions, are naturally
associated with a name (e.g., the name of the function defined); others,
such as theorems, are given names by the user.  See the section on syntax
below.
.P
The basic theorem-prover commands are those that create new events:
the definition of a new function, and the proof and storage of a new
theorem.  The commands that create new events are
.B "dcl,"
.B "defn,"
.B "prove-lemma,"
and
.B "move-lemma."
.P
Events are related to each other by logical dependencies.  For example,
the admission of a certain formula as a theorem depends on all of the
functions and lemmas used in the proof of the theorem.  Similarly, the
admission of a new recursive function definition depends not only
upon all of the previously introduced concepts used in the
definition, but also upon the functions and lemmas used to prove that
the proposed
``definition''
truly defines a function.
.P
Thus, the theorem-prover's
knowledge base is actually a noncircular, directed graph of events.
The theorem prover's performance is largely determined by its knowledge
base.  The Rule Builder is initialized with a knowledge base with
definitions and lemmas which define the basic objects of Pascal-F
verifications, integers and arrays, and provide a reasonable set of
knowledge about arithmetic and array operations.  The user will need to
add his own definitions and prove theorems about them.  It is possible
to dump the system's knowledge base to a ``library file'' to save the
system's state from one session to the next, and to provide information
to the Verifier proper.
.P
After proving several theorems, the user finds that one of
his earliest
defined concepts was inconveniently or inappropriately defined, the user can
undo that definition
(using the
.B "undo-name"
or
.B "undo-back-through"
commands)
and lose only those results whose meaning or logical validity may depend on
that definition.
.H 3 "Error handling"
If one tries to execute an inappropriate command (e.g., assign the same name to
two different events, or attempt to define a function in terms of unknown
concepts) self-explanatory error messages will be printed.  The system
checks for over 100 errors and has an error handling mechanism designed
to keep the theorem proving machine in a consistent state.  For example,
when an new command is processed, all possible errors are checked before the
first change is made to the data base, since an abortion midway through the
update would leave the machine in an unacceptable state.
.P
Error messages are grouped into three classes; WARNING, ERROR,
and FATAL ERROR messages.  Warnings arise when the system has
detected something unusual but not logically incorrect.  For example,
the system prints a WARNING message if the user defines a function but do not
refer to one of the formal parameters in the body of the function.
After printing a WARNING message, the system continues normal execution.
.P
ERROR messages result from true errors in the sense that the system
cannot continue until the error is repaired,
but the error can be repaired by editing a formula or changing a name.
When such an error occurs the system prints an explanatory error
message and then 
returns to the LISP command prompt level, discarding the failed command.
.P
FATAL ERROR messages occur when system resources are exhausted or when
internal checks indicate the presence of inconsistency in the data base
or bugs in the theorem prover itself.  It is usually not possible to
proceed past a fatal error.  When a FATAL ERROR is observed, it should
be reported as described in the section on reporting verifier problems.
.P
Despite the precautions taken in the theorem prover, it is possible
to get the system in an illegal state by aborting a command while
the data base is in the process of being changed.
Ctl-C will abort any command, but this is unsafe.  However,
the prover may spend hours exploring dead ends when trying some proofs.
It is thus necessary to abort commands on some occasions.
The following cautions apply:
.BL
.LI
.B "dcl,"
.B "make-lib,"
.B "move-lemma,"
.B "note-lib,"
and
.B "undo-name"
should never be aborted
while running.
.LI
.B "dependent-events,"
.B "events-since"
.B "prove,"
.B "ppe,"
and
.B "chronology"
may be aborted at any time without harm.
.LI
.B "defn"
and
.B "prove-lemma"
can be aborted but there is a small risk of corrupting the theorem prover's
database, if the
.B "defn"
or
.B "prove-lemma"
had in fact succeeded and the database was in the process of being updated.
.LE
.P
Boyer and Moore recommend that the result of any proof where a command
was aborted be considered suspect.
.H 3 "Output"
The theorem prover prints an English description of what it is doing
as it proceeds.  The sample session, shown below, shows what this is like.
Normally, the output goes to the user's terminal, but can be diverted.
.H 3 "Syntax"
All formulas in Boyer-Moore logic are written in a LISP-like
prefix notation.  This notation is fully described in
chapter III of
.I "A Computational Logic."
.H 4 "Functions"
The functions usable in this notation are those defined in
the section ``The Built-In Knowledge Base'' and any
new user-defined functions the user introduces with the
.B defn
and
.B dcl
commands.  Functions have a fixed number of arguments
as specified in the definition of the function.*
.FS *
.R
Some functions are by design N-ary, for example, and and or.
.FE
.P
.H 4 "Variables"
The variables used in this notation are free
variable names and have no relationship to names used in Pascal-F programs.
Names of variables, and of new functions, may be composed of the characters
A-Z, a-z, 0-9, ``.'', and ``!''.
Upper case letters are converted to lower case letters.
.H 4 "Constants"
The available constants are the integers, written in the usual way, and the
explicit constants shown below.
.VL 22 2
.LI "(true)"
Boolean true.
.LI "t"
Alternate for (true).
.LI "(false)"
Boolean false.
.LI "f"
Alternate for (false).
.LI "(undefined)"
A specific object which is not an array, an integer, or a Boolean value.
It is used in building up the theory of arrays.
.LI "(empty.array)"
An array-valued object, all elements of which are equal to
(undefined).
.LE
Only 
.B t, 
.B f\c
, and the integers are typically used by the user of the Rule Builder.
.H 2 "The mechanics of using the Rule Builder"
.H 3 "Starting up the program"
The instructions in this section apply to the UNIX version of the program.
.P
The command
.DP

        rulebuilder

.DE
invokes the Rule Builder.  This is a version of the Boyer-Moore prover
pre-initialized with a knowledge base compatible with the Verifier.
It takes about a minute to load the program (which requires about 2 megabytes
of memory.)
The messages
.DP

        Pascal-F Rule Builder of  2-JAN-86 16:02:04
        [load /usr/lib/verifier.lisp]
        Standard Pascal-F knowledge base loaded.
        ->

.DE
indicate that the system is ready for commands.
.P
If a previous session with the Rule Builder has been used to produce a
knowledge base, that knowledge base can be used as a starting point for a
new Rule Builder session by invoking the Rule Builder with the command
.DP

        rulebuilder knowledgebase

.DE
where
.I knowledgebase
is the name of a knowledge base created with the
.I make-lib
command.
.P
Commands are aborted with control-C,
which is sometimes unsafe, as mentioned above.
.P
.H 3 "Commands Summary"
The commands listed below are a subset of the full command list of the
Boyer-Moore theorem prover.  The commands listed are normally sufficient
for building rules for the Verifier.
.H 4 "chronology"
The word 
.B chronology\c
, without parentheses, will display a list of the names
in the current knowledge base, in
reverse
chronological order.
Thus, the last name listed will be the name of the oldest
event, which is usually 
.B ground-zero.
The 
.B chronology
list covers events
brought in through 
.B note-lib\c
, so the events which make up a library may
be listed.
.H 4 "(dcl <name> <arglist>)"
.I "Dcl"
declares
.I name
to be an undefined function of N arguments, where N is the
length of
.I arglist,
which must be a list of distinct, but otherwise meaningless, variable names.
Functions created via
.I dcl
have no semantics
in the Rule Builder, but
may have semantics in the Verifier.
The theory of uninterpreted functions does apply to functions created
by dcl, which means only that
if
.I f
is an uninterpreted function,
.DS

        x = y
    implies
        f(x) = f(y)

.DE
.H 4 "(defn <name> <arglist> <body> [<hints>])"
.I Defn
defines a function named
.I name,
with formal argument list
.I arglist
and body
.I body.
The optional parameter
.I hints
allows the user to assist the theorem prover in validating the
definition.
The
.I arglist
must be a list of distinct variable names, and
.I body
must be an expression in the theory.  This expression must be constructed
as described under the section on syntax, and may use as variable names only
the names present in
.I arglist.
Only previously defined functions, and the the function being defined in the
defn, may be used in the
.I body.
.P
For recursive definitions, the system insists that the recursion terminate
and will not accept the definition fully unless it can prove this.
The system will try to prove that some measure of
.I arglist
gets smaller in each recursive call to
.I name
within
.I body.
.P
When the system cannot prove that a recursive definition terminates,
a WARNING message appears stating that the definition is not well-founded.
The definition is not, however, rejected by the system.  It should be.
The user should immediately remove it with an
.B undo-name
command.
.P
The 
.I hint
parameter to 
.I defn
is seldom required.
If the function being defined is nonrecursive, it
is never required.  If the function is recursive but recurses in such a way
that at least one of the arguments in the recursive call is clearly
less than the value at invocation, no hint should be required.
For example, if the function recurses by subtracting 1 from an argument
until the value becomes zero, the theorem prover will be able to satisfy
itself of the soundness of the definition without difficulty.
But if a function recurses by 
.I adding
one to an argument until a limit is reached, a hint will be
necessary.
.P
Hints for 
.I defn
are very similar to the Pascal-F
.B MEASURE
statement; the user must supply an expression whose value becomes
smaller with each recursion.
Hints have the form of a list of 
.B "(<comparing-operator> <recursion measure>)"
terms.  In the usual case, where only one hint is required, the
.I hint
parameter has the form
.B "((<comparing-operator> <recursion measure>))."
Suitable
.I comparing-operators
are
.B lessp,
and
.B lex2*.
.FS *
.R
See 
.I "A Computational Logic" 
for an explaination of 
.I lex2.
.FE
The 
.I "recursion measure"
must be some expression which, when evaluated both at
entry to the recursive function and at entry to the recursive function one
level deeper in the recursion, becomes smaller with each recursion.
Here, ``smaller'' is defined relative to the comparing-operator chosen.
.P
Note that
.B defn
definitions are actually small recursive programs.  It is
possible to run these programs on test data; see the
.B r
command.
.H 4 "(dependent-events <name>)"
.I Dependent-events
takes an event name (i.e. a function or lemma name)
and returns the events which depend on it.
If
.I name
is deleted with
.I undo-name,
all the dependents of
.I name
will be deleted.
.P
Dependency is simply defined.  If a
.B "defn"
.I g
mentions a function
.I f,
then
.I g
depends on
.I f.
If the proof of lemma
.I x
used lemma
.I y
and
.B "defn"
.I f,
then
.I x
depends on
.I f
and
.I y.
.H 4 "(events-since <eventname>)"
This returns a list of all the events stored since the named event.
The list is based strictly on time, not dependency.
.H 4 "(exit)"
.I "Exit"
causes an exit from the Rule Builder.  Any new knowledge added
since the last
.B "make-lib"
is lost.
.H 4 "(lemmas <functions>)"
This is a cross-referencing tool.  A list of all lemmas which mention
any function in
.I functions
is returned.
.H 4 "(make-lib <file>)"
.I "Make-lib"
makes a file named
.I file\c
.r .lib
and a file named
.I file\c
.r .lisp
which together contain 
the entire current knowledge base.  Invoking the Rule Builder with
.DP
        rulebuilder <file>
.DE
.P
will restore the state of the Rule Builder to that in effect when the
.B make-lib 
was executed.
.P
Library files are of moderately large size, about 100 kilobytes, and
contain not only the events but substantial amounts of internal information.
.H 4 "(move-lemma <name> <lemmatypes> <oldname>)"
The
.I "move-lemma"
command allows the user to change the
ways in which a lemma can be
applied within the Rule Builder.  It is used primarily to ``hide'' lemmas
or definitions which although correct cause the Rule Builder to pursue
dead ends.  Usually,
.I lemmatypes
is NIL, which causes
the
.B "defn"
or
.B "prove-lemma"
event
.I oldname
to be hidden.  When a
.B "prove-lemma"
event is hidden, the lemma will not be used
by the Rule Builder, and when a defn event is hidden, that definition will
not be opened up.  Hiding a
.B "defn"
does not prevent its evaluation with the
r command.
.H 4 "(note-lib <file>.lib <file>.lisp)"
.I "Note-lib"
reads in
.I file,
which must have been produced by
.B "make-lib,"
and
reinitializes the Rule Builder with the knowledge base in that file.
This is a reinitialization, not an addition; the state of the theorem
prover is cleared before the read.
.P
.H 4 "(ppe <eventname>)"
The
.I "ppe"
command prints the event
.I eventname
in a tidy format.
.H 4 "(prove <thm>)"
.I "Prove"
attempts to prove the conjecture
.I thm,
using all the theorem-proving techniques at the system's disposal.
prove prints an English-language description of the proof attempt in
real-time, so the user can monitor the progress of the attempt.
The result of the proof is not saved.
.H 4 "(prove-lemma <eventname> <lemmatypes> <thm> [<hints>])"
This is the most important command in the Rule Builder.
.I "Prove-lemma"
attempts to prove a lemma as with
.B "prove,"
and if the attempt
is successful, the lemma will be saved, available for use in the ways
specified in the list
.I lemmatypes.
The allowed lemma types are rewrite, elim, and generalize.
.B "prove-lemma"
first checks to see if the syntactic form of
.I thm
is acceptable for the
.I lemmatypes
indicated.  If no error is diagnosed in this pre-processing, a proof
is attempted as with the prove command.
If the proof succeeds, the lemma is stored under the name
.I eventname
and is usable with the lemma types
.I lemmatypes.
.P
If
.I eventname
ends in ``-rule'' or ``-RULE'', and
the
.I lemmatypes
list
includes
``rewrite''
then the event is considered a Verifier rule and, if placed in a
Rule Builder library with 
.B "make-lib"
and then copied to a
Verifier database file with
the ``putrules'' utility,
will be used by the Verifier.
.P
The optional parameter
.I hints
allows the user to order the theorem prover to try a specific lemma
or induction at the beginning of the proof, thus affording some minimal
control over the proof process.
Hints are not normally necessary; it is 
better when possible to help the theorem prover along by proving
lemmas which will then be used in later proofs.
However, hints can be provided if necessary.
This is an advanced feature of the prover and is not recommended for new
users.  
.P
The
.I hints
arguement to the 
.B prove-lemma
command, if not ommitted, must be a list of 
.I "hint entries."
There are five kinds of hints:
.VL 22 2
.LI "use"
Indicates that a specific lemma is to be 
applied in a specific way at the
beginning of the proof.  The form of a 
.B use
hint is
.DP

      (use (event1 (v1 t1) ... (vn tn))
           ...
           (eventk (vk tk) ... (vm tm)))

.DE
where each 
.I eventi
is the name of an 
.B add-axiom, 
.B prove-lemma,
or
.B defn
event, each 
.I vi
is a variable name from the definition of the event, and each 
.I ti
is a term in the formula being proven.
The user is thus explicitly requesting the application of a rule, and
the user must give the exact bindings of the variables in the rule
to the terms in the formula being proven.
.P
A
.B use
hint is a request, not a demand; if the lemma indicated
in the hint cannot be applied to the
formula being proven, the hint will be ignored and the prover will proceed
without it.  It is thus important when using 
.B use
hints to watch the beginning of the proof for the application of the
hint.
.LI "expand"
Indicates that a
.B defn
should be expanded at the beginning of the proof.
.LI "disable"
Prevents the use of a named lemma or expansion of a named definition
in the proof.
.LI "induct"
Indicates the induction strategy that the
prover is to use.
.LI "time"
We don't know what this is for.
.LE
.P
The
.I use
and
.I disable
hint types are sufficient for most proofs.
The prover has relatively good diagnostics for incorrectly formed hints;
a badly formed hint will produce a message giving the correct form and
an indication of why the prover is unhappy with the hint.
.H 4 "(r <term>)"
The
.I "r"
command evaluates
.I term,
which must be an expression constructed from valid built-in functions,
defn functions, and constants.
For example,
.DP

        (r (plus 2 2))

.DE
will return 4.
More usefully, if we define a function of our own,
.DP

        (defn FACTORIAL (N)
                (if (lessp 0 N)
                        (times N (FACTORIAL (difference N 1)))
                        1))

.DE
we can then try test cases on it.
.DP

        (r (FACTORIAL 4))

.DE
will return 24.
.P
This evaluation process is quite fast.  The Rule Builder 
generates Lisp
code for each
.B "defn"
when the
.B "defn"
is created, so that definitions can be
rapidly evaluated for constant values.
Recursive definitions
generate recursive code.  Since the Rule Builder insists
that definitions
be provably well-founded, infinite recursion is prevented.
.P
The Boyer-Moore theory is a constructive theory of total functions.
This means that any syntactically valid
variable-free expression can
be evaluated.  For example, FACTORIAL can be applied to T, the Boolean
value true, and a consistent value will be returned.  When constructing
definitions,
the actions for inputs of unexpected types must be borne in mind.
.P
It is worthwhile to try out the
.B "r"
command on some of the built-in functions
to develop a feel for what they do.  An interesting exercise is to produce
an array-valued result.  For example,
.DP
.ce
(r (storea! (storea! (empty.array) 3 100) 5 200))
.DE
will produce an array-valued object with element 3 equal to 100 and
element 5 equal to 200.  This gives the user some insight into
how arrays are represented internally.
.H 4 "(undo-back-through <eventname>)"
.I Eventname
and all events performed since
.I eventname
will be undone.
This includes events not dependent on
.I eventname.
.H 4 "(undo-name <eventname>)"
.I Eventname
and all events dependent upon it will be undone.
.H 2 "The built-in knowledge base"
The Rule Builder, as stated before, is a version of the Boyer-Moore theorem
prover pre-initialized with a knowledge base compatible with the
Verifier.  This knowledge base includes definitions of arithmetic for
the natural numbers and the integers, Boolean and comparison functions,
and a definition of arrays.  It also contains about a hundred lemmas of
general utility, most of which are statements about arithmetic.
.H 3 "The built-in functions"
The predefined functions are as follows.
.VL 22 2
.LI "(add1\ N)"
Adds one, usable on natural numbers only.
.LI "(and\ B1\ B2\ ...)"
N-argument and.
.LI "(difference\ N\ M)"
Natural number subtraction.
.LI "(equal\ X\ Y)"
Equality; usable on any type operands.
.LI "(if\ P\ X\ Y)"
If P then X else Y.
if must not appear in rules, but may be used in
.B "defn"
definitions
or non-rule lemmas.
.LI "(implies\ P\ Q)"
Boolean implication.
.LI "(lessp\ N\ M)"
Natural number comparison.
.LI "(not\ P)"
Boolean negation.
.LI "(numberp\ N)"
True if value is a natural number.
.LI "(or\ B1\ B2\ ...)"
N-argument or.
.LI "(plus\ N\ M)"
Natural number addition.
.LI "(quotient\ N\ M)"
Natural number division.
.LI "(remainder\ N\ M)"
Natural number remainder.
.LI "(sub1\ N)"
Subtracts one.
.LI "(times\ N\ M)"
Natural number multiplcation.
.LI "zero"
Equivalent to 0.
.LI "(zerop\ N)"
True if N is equal to 0.
.LI "(addi!\ I\ J)"
Integer addition.
.LI "(alltrue!\ r)"
True if all parts of the argument are true.  This is
a dcl to the rule builder, and has no semantics in the rule builder.
The Verifier expands
.I alltrue!
based on type information.
.LI "(arrayp!\ A)"
Array type predicate, true if the argument is an array.
.LI "(arraytrue!\ A\ I\ J)"
True if the elements from I to J of array A are
.I alltrue!.
When
.DP
.ce
defined(A,I,J)
.DE
is written in Pascal-F source,
.I "(arraytrue! A I J)"
applied to the definedness part of A will be generated in the
verification condition.  This allows inductive proofs of definedness of
arrays.
.LI "(booleanp!\ A)"
Type predicate, true if A is Boolean.
.LI "(divi!\ I\ J)"
Integer division.
.LI "(gei!\ I\ J)"
Integer >=.
.LI "(gti!\ I\ J)"
Integer >.
.LI "(integerp!\ I)"
Type predicate, true if I is an integer.
.LI "(lei!\ I\ J)"
Integer <=.
.LI "(lti!\ I\ J)"
Integer <.
.LI "(mod!\ I\ J)"
Integer remainder.
Mod should be applied to positive numbers only, because the
Verifier has no knowledge about what the result is for negative numbers.
This reflects the Pascal-F implementation.
.LI "(muli!\ I\ J)"
Integer multiply.
.LI "(negi!\ I)"
Integer negation.
.LI "(numberp!\ I)"
Type predicate, true if I is a natural number (nonnegative).
.LI "(selecta!\ A\ I)"
Array subscripting function; equivalent to the Pascal form A[I].
.LI "(selectr!\ A\ F)"
Record selector, equivalent to the Pascal form ``A.F''.
The Rule Builder does not know about records because it lacks
type information.  This is a dcl uninterpreted
function.  The Verifier interprets
.I selectr!
based on the type information from the program.
.LI "(storea!\ A\ I\ V)"
Array store function.  The result of
.I storea!
is an array equal to A except that A[I] = V.
This function has no Pascal infix-form
equivalent, but is displayed in the Verifier's log
of verification conditions as ``<A I V>''.
.LI "(storer!\ A\ F\ V)"
Record store function.  The result of
.I storer!
is a record equal to A except that A.F = V.
This function is displayed in verification conditions as ``<A I V>'',
and looks just like the array store function in that form.
The
.I storer!
function, like
.I selectr!,
is a dcl.
.LI "(subi!\ I\ J)"
Integer subtraction.
.LE
.P
The upper-case names are identical to those in
.I "A Computational Logic"
in both syntax and meaning.
We have not changed these because they are built into the Boyer-Moore system.
No rule function may be given the same name as one of the built-in names.
The names ending in ``!'' represent the additional functions needed
to handle Pascal-F verification conditions.
