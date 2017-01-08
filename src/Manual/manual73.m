.H 2 "Additional output from the verifier"
To show what the Verifier actually does with the information provided through
rules, we show some of the diagnostic output the Verifier produces for
the use of those building and testing rules.
When a verification is unsuccessful, it is usually best to try to fix the
problem by looking at the source program.  There are, though, additional
outputs available from the Verifier for dealing with difficult problems.
.P
This section is not intended to give a real understanding of how the
Verifier works.  There is an internal documentation manual for the
Verifier, and the sections of that manual entitled
.B "Icode to Jcode Translator"
and
.B "Verification Condition Generator"
cover the generation of verification conditions
and the internal file formats
in much greater detail.
.H 3 "The logging file"
The output below all appears in the file
.B "p3-vcs"
in the Verifier's scratch directory
.B "programname\c"
.pf (but only if the
.B "-dvcg"
flag is set on the call to
.B "pasver.)"
.H 4 "The rule listing"
The first part of this file is the list of rules found in the
.B "ruledatabase"
file.
.DP

    allzero-extend-upward-rule --
        Usable on conclusions only, free variables (A I J g00002)
    Trigger pattern sequence: ((allzero A I g00002) (allzero A I J))

    arrayp!(A)
    and numberp!(I)
    and numberp!(J)
    and allzero(A,I,J)
    and (A[addn!(J,1)] = 0)
    implies
      allzero(A,I,addn!(J,1))

.DE
We remember this rule from the Rule Builder session.  Here, the rules
appear in infix form, in Pascal-like notation.
Operators which have no Pascal-F source representation appear
as function calls.  These names all end with ``!'' to avoid interference
with user-defined functions.
Note that
numberp has become
.B "numberp!,"
and
(add1 J)
has become
.B "addn!(J,1),"
where
.B "addn!"
is the plus function (natural number add) from the Rule Builder.
.P
The list of free variables lists all the terms which must be bound when
the rule is instantiated.  The dummy name
.B "g00002"
is a placeholder for
the
.B "addn!"
term in the conclusion.
.P
The
.B "trigger pattern sequence"
shows when the rule will be applied.  The Verifier will look at the
verification condition, and will try to match the first pattern in the
sequence.  Once it matches, the variables in that pattern are bound
and the Verifier begins looking for the second pattern in the sequence.
The message ``usable on conclusions only'' indicates that the first
pattern is general enough (containing only one function symbol) that
applying the rule everywhere in every way that
.B "allzero"
appeared
would slow down the system too much.  So this rule will only be applied
when
.B "allzero"
appears in a proof goal, that is, something written in an
.B "ASSERT,"
.B "STATE,"
.B "SUMMARY,"
.B "ENTRY,"
.B "exit,"
or
.B "INVARIANT,"
or (not possible in this case)
in an internally generated requirement used to insure subscripts within
range or arithmetic results within bounds.  Within this limitation,
rules are applied in every possible combination of ways,
but only one deep, so if a proof can be found in one step with the given
rules, it will be found.
.DP

    allzero-select-rule --
        Usable on conclusions only, free variables (A I J X)
    Trigger pattern sequence: ((selecta! A X) (allzero A I J))

    allzero(A,I,J)
    and numberp!(I)
    and numberp!(J)
    and arrayp!(A)
    and numberp!(X)
    and not gtn!(X,J)
    and not gtn!(I,X)
    implies
      A[X] = 0

.DE
Note that A[X] is
.B "(selecta! A X)"
in the pattern.
The function
.B "gtn!"
is the greater than operator for the natural numbers.
.DP

    allzero-single-rule --
        Usable on conclusions only, free variables (A I J)
    Trigger pattern sequence: ((allzero A I J))

    arrayp!(A) and numberp!(I) and numberp!(J) and (I = J) and (A[I] = 0)
    implies
      allzero(A,I,J)

.DE
.DP

    allzero-unchanged-1-rule --
        Usable anywhere, free variables (A I J V X)
    Trigger pattern sequence: ((allzero (storea! A X V) I J))

    numberp!(I)
    and numberp!(J)
    and arrayp!(A)
    and allzero(A,I,J)
    and numberp!(X)
    and (gtn!(I,X) or gtn!(X,J))
    implies
      allzero(<A,X,V>,I,J)

.DE
Note that this rule is usable anywhere.  Because there are nested function
symbols in the pattern, we don't expect to see too many places where this
rule could be applied without purpose.  Also note the appearance
of
.B "<A,X,V>"
which is the infix form of (storea! A X V).
.DP

    allzero-unchanged-2-rule --
        Usable anywhere, free variables (A I J X)
    Trigger pattern sequence: ((allzero (storea! A X 0) I J))

    allzero(A,I,J)
    and arrayp!(A)
    and numberp!(I)
    and numberp!(J)
    and numberp!(X)
    implies
      allzero(<A,X,0>,I,J)

.DE
.DP

    allzero-void-rule --
        Usable on conclusions only, free variables (A I J)
    Trigger pattern sequence: ((allzero A I J))

    arrayp!(A) and numberp!(I) and numberp!(J) and gtn!(I,J)
    implies
      allzero(A,I,J)

.DE
The following rules are part of the standard database, and are used
for proving definedness.
.DP

    arraytrue-extend-upward-rule --
        Usable on conclusions only, free variables (A I J g00005)
    Trigger pattern sequence: ((arraytrue! A I g00005) (arraytrue! A I J))

    (arraytrue!(A,I,J) = true) and (alltrue!(A[addn!(J,1)]) = true)
    implies
      arraytrue!(A,I,addn!(J,1)) = true

.DE
.DP

    arraytrue-single-rule --
        Usable on conclusions only, free variables (A I)
    Trigger pattern sequence: ((selecta! A I))

    arraytrue!(A,I,I) = true = alltrue!(A[I]) = true

.DE
.DP

    arraytrue-unchanged-rule --
        Usable anywhere, free variables (A I J V X)
    Trigger pattern sequence: ((arraytrue! (storea! A X V) I J))

    numberp!(X)
    and numberp!(I)
    and numberp!(J)
    and arrayp!(A)
    and (arraytrue!(A,I,J) = true)
    and (gtn!(I,X) or gtn!(X,J))
    implies
      arraytrue!(<A,X,V>,I,J) = true

.DE
.DP

    arraytrue-void-rule --
        Usable on conclusions only, free variables (A I J)
    Trigger pattern sequence: ((arraytrue! A I J))

    gtn!(I,J) implies arraytrue!(A,I,J)

.DE
.H 4 "The verification trace"
Now we go on to the verification goals themselves.
If a verification condition had failed, it would have been printed, but all
these succeeded, so only the goal and path appear.  This is exactly the
text that would appear as an error message if the proof failed.
.DP

    Verification condition for {example6.pf:18} table1[(j - 1) + 1] = 0
        (ASSERT assertion)
    Path:
        {example6.pf:11} Start of "example6"
        {example6.pf:11} FOR loop never entered
        Tried arraytrue-single-rule.
    VC #1 proved in 1.00 seconds.

.DE
This one took one second, and
.B "arraytrue-single-rule"
was successfully pattern-matched, although it is irrelevant in this case.
The message ``Tried'' indicates only that the rule could have been applied,
not that it actually was.
.DP

    Verification condition for {example6.pf:18} table1[(j - 1) + 1] = 0
        (ASSERT assertion)
    Path:
        {example6.pf:11} Start of "example6"
        {example6.pf:11} FOR loop exit
        Tried allzero-select-rule.
        Tried arraytrue-single-rule.
    VC #2 proved in 2.70 seconds.

.DE
Here there is some statement about definedness in the hypothesis of the
verification condition which triggered the built-in rule
.B "arraytrue-single-rule."
.DP

    Verification condition for {example6.pf:16} allzero(table1,1,100)
        (ASSERT assertion)
    Path:
        {example6.pf:11} Start of "example6"
        {example6.pf:11} FOR loop never entered
        Tried allzero-extend-upward-rule.
        Tried allzero-single-rule.
        Tried allzero-void-rule.
    VC #3 proved in 0.63 seconds.

.DE
We do not get any useful information about which rule did it, if any.
.DP

    Verification condition for {example6.pf:16} allzero(table1,1,100)
        (ASSERT assertion)
    Path:
        {example6.pf:11} Start of "example6"
        {example6.pf:11} FOR loop exit
        Tried allzero-extend-upward-rule 2 times.
        Tried allzero-single-rule.
        Tried allzero-void-rule.
    VC #4 proved in 1.00 seconds.

.DE
.DP

    Verification condition for {example6.pf:11} i <= 99
        (FOR loop count)
    Path:
        {example6.pf:11} Start of "example6"
    VC #5 proved in 0.75 seconds.

.DE
.DP

    Verification condition for {example6.pf:14} allzero(table1,1,i)
        (STATE assertion)
    Path:
        {example6.pf:11} Start of "example6"
        {example6.pf:15} Back to top of FOR loop
        Tried allzero-unchanged-2-rule 2 times.
        Tried allzero-unchanged-1-rule 2 times.
        Tried allzero-extend-upward-rule 2 times.
        Tried allzero-single-rule.
        Tried allzero-void-rule.
    VC #6 proved in 28.81 seconds.

.DE
This is the inductive case around the loop, the hard one.  It took 28 seconds.
.DP

    Verification condition for {example6.pf:14} allzero(table1,1,i)
        (STATE assertion)
    Path:
        {example6.pf:11} Start of "example6"
        {example6.pf:11} Enter FOR loop
        Tried allzero-unchanged-2-rule 2 times.
        Tried allzero-unchanged-1-rule 2 times.
        Tried allzero-extend-upward-rule 2 times.
        Tried allzero-single-rule.
        Tried allzero-void-rule.
    VC #7 proved in 2.45 seconds.

.DE
.DP

    Verification condition for {example6.pf:13} allzero(table1,1,i - 1)
        (ASSERT assertion)
    Path:
        {example6.pf:11} Start of "example6"
        {example6.pf:15} Back to top of FOR loop
        Tried allzero-unchanged-2-rule.
        Tried allzero-unchanged-1-rule.
        Tried allzero-extend-upward-rule.
        Tried allzero-single-rule.
        Tried allzero-void-rule.
    VC #8 proved in 9.25 seconds.

.DE
.DP

    Verification condition for {example6.pf:13} allzero(table1,1,i - 1)
        (ASSERT assertion)
    Path:
        {example6.pf:11} Start of "example6"
        {example6.pf:11} Enter FOR loop
        Tried allzero-unchanged-2-rule.
        Tried allzero-unchanged-1-rule.
        Tried allzero-extend-upward-rule.
        Tried allzero-single-rule.
        Tried allzero-void-rule.
    VC #9 proved in 2.03 seconds.

.DE
.DP

    Verification condition for {example6.pf:12} i - 1 <= 99
        (subscript check for "table1" 1..100)
    Path:
        {example6.pf:11} Start of "example6"
        {example6.pf:15} Back to top of FOR loop
    VC #10 proved in 0.30 seconds.

.DE
This is an internally-generated proof goal, a subscript check.
.DP

    Verification condition for {example6.pf:12} i - 1 <= 99
        (subscript check for "table1" 1..100)
    Path:
        {example6.pf:11} Start of "example6"
        {example6.pf:11} Enter FOR loop
    VC #11 proved in 0.26 seconds.

.DE
.DP

    Verification condition for {example6.pf:12} i - 1 >= 0
        (subscript check for "table1" 1..100)
    Path:
        {example6.pf:11} Start of "example6"
        {example6.pf:15} Back to top of FOR loop
    VC #12 proved in 0.31 seconds.

.DE
.DP

    Verification condition for {example6.pf:12} i - 1 >= 0
        (subscript check for "table1" 1..100)
    Path:
        {example6.pf:11} Start of "example6"
        {example6.pf:11} Enter FOR loop
    VC #13 proved in 0.26 seconds.

.DE
.DP

    Verification condition for {example6.pf:12} "i" is defined
    Path:
        {example6.pf:11} Start of "example6"
        {example6.pf:15} Back to top of FOR loop
    VC #14 proved in 0.08 seconds.

.DE
This is a test to make sure the variable
.B "i"
was defined at line 12.  These usually are very fast to prove.
.DP

    Verification condition for {example6.pf:12} "i" is defined
    Path:
        {example6.pf:11} Start of "example6"
        {example6.pf:11} Enter FOR loop
    VC #15 proved in 0.06 seconds.

.DE
.P
That is the summary of the verification.  When a verification
is unsuccessful, and it is is not clear why, examination of this
file can be quite useful.  Remember that this file is not generated
unless requested, and generating it does slow down the verification by
about 20-40%.
.H 4 "What verification conditions look like"
It is not usually necessary for the user to look at verification conditions,
but when difficulties are encountered it can sometimes be useful.
When using a compiler, it is sometimes useful to turn on a listing of
generated object code.  The equivalent listing for a verifier is the
listing of verification conditions.
We thus provide an explanation as to how to read a verification condition.
.P
Had a verification condition failed,
in the above example,
we would see, in this log of
verification conditions, the verification condition itself.
If we force some errors by trying
to run the verification above without any rules about
.B "allzero"
available, we would find the information below in
the log.
.DP

    Verification condition for {example6.pf:14} allzero(table1,1,i)
        (STATE assertion)
    Path:
        {example6.pf:11} Start of "example6"
        {example6.pf:15} Back to top of FOR loop

    (TEMP4__v01 = 1)
    and (TEMP5__v01 = 100)
    and true
    and (i_4v03 <= TEMP5__v01)
    and allzero(table1_2v02,1,i_4v03)
    and (i_4v03 >= TEMP4__v01)
    and (i_4v03 < TEMP5__v01)
    and (i_4v03 <= 99)
    and (i_4v04 = i_4v03 + 1)
    and (i_4v04 - 1 >= 0)
    and (i_4v04 - 1 <= 99)
    and (table1_2v03 = (table1_2v02[(i_4v04 - 1) + 1] := 0))
    and allzero(table1_2v03,1,i_4v04 - 1)
    implies
      allzero(table1_2v03,1,i_4v04)

    VC #6 FAILED in 1.48 seconds.

.DE
Here, we see the actual verification condition to be proven.
This verification condition is for the path around the loop,
with the proof goal of
.B "allzero(table1,1,i)"
in the STATE statement.
.P
Verification conditions are always of the form ``big conjunction implies
proof goal''.  The proof goal is always the term printed after the
implication.  The terms in the hypothesis are generated by backwards
tracing through the program, examining each statement along the indicated
path.  The details of how this is done are beyond the scope of this manual,
but generally follow the backwards-tracing approach of Floyd, Manna, and
others.  The basic idea is that programs are converted to formulas by
tracing backwards through each statement, using a new variable name for
each variable every time its value is changed.
For example, the Pascal-F statements
.DP

        x := 1;
        x := x + 1;
        assert(x = 2);

.DE
would generate a verification condition of the form
.DP

       x_1v01 = 1
   and x_1v02 = (x_1v01 + 1)
   implies
       x_1v02 = 2

.DE
Note that
.B "x_1v01"
and
.B "x_1v02"
represent values of
.B "x"
at different points in the program.
In our
.B "allzero"
example,
the variable names
.B "i"
and
.B "table1"
appear here in modified form; the
.B "table1_2v03"
string is the variable
.B "table1"
after the third assignment to it.
The names are actually constructed by taking the user's name of the
variable,
adding a delimiter
and a variable serial number,
(so that variables with the same name but in different scopes are made unique)
and adding a
.B "v"
or a
.B "d,"
where a
.B "v"
indicates that the
.B "value"
of the variable is being referred to, and
.B "d"
indicates that the
.B "definedness part"
of the variable is being referenced.
Finally, a two digit suffix indicating, as in the little example with
.B "x"
above, which value of the variable we are referring to, is added to the name.
the code.
.P
The
.B "v"
or
.B "d"
component deserves some extra discussion.
Definedness of variables is handled by a convenient fiction.
We pretend that associated with each variable there is a definedness flag,
set to true when any value is assigned to the variable and tested at every
reference to the variable.
We then try to prove that the flag is always true at every reference.
For an array or record our definedness part will be an array or record
with all-Boolean elements or lowest-level fields.
When the Verifier generates a verification condition about the
definedness of a variable, it constructs a name using the
.B "d"
letter.
.P
Not shown in the verification condition is the type information.
The theorem prover has available to it all the information in the
type declarations of the Pascal-F program.  For example, if the
declaration
.DP

.ce
var i: 0..100;

.DE
appeared in a Pascal-F program, the theorem prover would be able to prove
.DP

        true
        implies
            i_1v01 <= 100

.DE
without any difficulty.  We can assume that all variables stay within their
type because we generate proof goals for the bounds of every variable at
every assignment to that variable.  Since we also check at every reference
to every variable that the variable has been initialized (defined) we
are thus safe in assuming that variables stay within their types.
.H 3 "The diagnostics file"
The file
.B "p3-diags"
contains any error messages produced during the verification.
This is useful if messages scroll by on a CRT terminal and are thereby lost.
Only messages from pass 3 appear in this file, but pass 3 is where all the
time goes.
.H 3 "The rule data base file"
The rule data base file
.B "ruledatabase"
is created by the
.B "putrules"
utility
and read by the Verifier.  It contains the rule data base to be used
for the current verification.
.H 3 "The history file"
The history file
.B "history"
contains the entire intermediate code for each program
unit previously verified successfully.  On reverification attempts,
if the newly generated intermediate code matches that found in the
history file, verification of that program unit is skipped.
Removal of this file will force a complete rerun of the verification.
Note that
.B "putrules"
will remove this file if a new database is used with different
.B "defn"
entries for some previously used
.B "defn"
definition.
