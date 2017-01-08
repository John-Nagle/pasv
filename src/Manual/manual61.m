.H 2 "An introduction to rules"
Consider the following simple program fragment.
.DP

    type tabix = 1..100;
    type tab = array [tabix] of integer;
    var table1: tab;
        i,j: tabix;
    ...
    for i := 1 to 100 do begin
        table1[i] := 0;
        end;
    ...
    assert(table1[j] = 0);  { table1[j] must be 0 }

.DE
.P
Here we have cleared an array to zero, and somewhere further along
in the program we need to be sure that a specific element in the array
is zero.  
To do this we will need some way to express the concept that all the
elements in the array are zero.
Since there is no built-in way in Pascal to talk about
the values of all the elements in an array in a collective sense, it
is necessary to introduce a means for doing this.
.P
In the is case we introduce a 
.I "rule function"
called
.I allzero.
We define it in Pascal-F with the declaration
.DP
.ce
rule function allzero(a: tab; i,j: tabix): boolean; begin end;
.DE
Here, the keyword
.B rule
identifies this as a rule function.  Rule functions are used only for
verification purposes and are ignored by the compiler.  Rule functions
may not have function bodies or entry and exit assertions; they have
only a minimal declaration as shown in the example.
.P
Rule functions are useful only when some rules about them have been proven.
We will define
.I allzero
informally as being true if the array elements of
.I a 
are all zero from element 
.I i
to element
.I j
inclusive and false otherwise.
Some useful rules for 
.I allzero 
are
.DS
.ce
(j < i) implies allzero(a,i,j)
.DE
which says that
.I allzero
is vacuously true if the the upper bound is less than the lower bound;
.DS
.ce
(i = j) implies (allzero(a,i,j) = (a[i] = 0))
.DE
which tells us that if both bounds are the same, the value of allzero
is equal to true if the value at the bounds is true and false otherwise;
.DS
.ce
(allzero(a,i,j) and (a[j + 1] = 0)) implies allzero (a,i,j + 1)
.DE
which states that if
.I allzero
is true from
.I i
to
.I j
and
.I "a[j + 1]"
is equal to zero then
.I allzero
is true from 
.I i
to 
.I "j + 1;"
.DS
.ce
(allzero(a,i,j) and ((x < i) or (x > j))) 
.ce
implies allzero (<a,x,v>,i,j),
.DE
(where the strange notation 
.I <a,x,v>
means ``the array
.I a 
with element 
.I x 
replaced by 
.I v\c
'')
which says that if
.I allzero
was true from
.I i
to
.I j,
storing into an element
.I x
outside the range 
.I i
to
.I j
will not destroy the
.I allzero
property;
and finally
.DS
.ce
((allzero(a,i,j) and (x >= i) and (y <= j)) implies (a[i] = 0)
.DE
which allows us to use the information that allzero is true for a range
of values to prove that a specific value is zero.
.P
For the time being, we will ignore where these rules came from
and will concentrate on what can be done with them.
Going back to our program, it is clear that we are going to want to prove
.DS
.ce
allzero(table1,1,100)
.DE
at the end of the
.B for
loop.
This implies that a loop invariant, a
.B state
statement, will be required to describe the situation which is true at
each iteration of the loop.
.P
One minor complication is that it is not obvious to the Verifier
that storing into 
.I "table1[i]"
does not change the fact that
.I allzero
is true from 
.I "1" 
to 
.I "i-1."
We are going to need a rule that tells the
Verifier that out-of-bounds stores don't cause 
.I allzero
to become false.
Also,
the rule handler in the Verifier only uses one rule between any two
statements.  We thus will have to put an assertion in the program that
the previous STATE is still true after the assignment statement.  Since
.I i
has increased by 1, this assertion will be that
.I allzero
is true from
.I "1" 
to 
.I "i-1."
So now let us see the program with the addition of the required assertions.
.DP

    type tabix = 1..100;
    type tab = array [tabix] of integer;
    rule function allzero(a: tab; i,j: tabix): boolean; begin end;
    var table1: tab;
        i,j: tabix;
    ...
    for i := 1 to 100 do begin
        table1[i] := 0;
        assert(allzero(table1,1,i-1));
        state(allzero(table1,1,i));
        end;
    assert(allzero(a,1,100));
    ...
    assert(table1[j] = 0);  { table1[j] must be 0 }

.DE
.P
Each time through the loop, 
.I allzero 
becomes true for one more element,
and
.I allzero
is then true from 1 up to the loop index.
.P
Given the rules described, 
the verifier is able to prove all the assertions
in the program fragment automatically.
.P
Typically, the user will be provided with a 
knowledge base containing a
library of rules which cover
most common situations in programming, and will be able to proceed much
as shown above.  In the next chapter, we will return to the example
above and show in detail how the rules for it are proven.
.H 2 "Rules and the Verifier"
.H 3 "How the Verifier applies rules"
Rules are almost always of the form
.DS
.ce
A implies B
.DE
where 
.I A 
is referred to as the 
.I hypothesis
and
.I B
as the
conclusion.
The Verifier, when trying to prove something such as
.DP
.ce
assert(allzero(a, i, j));
.DE
where 
.I allzero
is a rule function,
will search the database for rules with conclusions of the form
.DS
.ce
allzero(x,y,z)
.DE
and will then
apply the rule by
.I binding
each free variable in the rule to the corresponding expression in
the form being proven.
This process is called
.I instantiating
the rule, because the general form of the rule has now been applied
to a specific instance.
The Verifier will try every applicable rule in every legitimate way, but
will not apply a rule to a form introduced by a rule.  In other words,
the Verifier applies rules only one deep.  It is the user's job to
add assertions to the source program so that between any two assertions
the application of only one rule is required.
How to do this was
described in detail in chapter 2.
.H 3 "How the rules get to the Verifier"
Rules are created with the Rule Builder (described in the next chapter)
and made available to the Verifier with the
.B putrules
utility program.
The Rule Builder creates a
.B "knowledge base"
file, which may be used for verifying several different programs.
The rules being used in a given verification must be placed in a
.B ruledatabase  
file.
There is one such file for each program being verified, and
this file resides in the directory of work files used by the verifier
for each program being verified.  The directory has the name
.DP
.ce
<programname>_d
.DE
where the program being verified has the name
.DP
.ce
<programname>.pf
.DE
.P
This directory is created by the Verifier the first time an attempt is
made to verify a given program, and is never deleted by the Verifier.
The
.B ruledatabase
and
.B history
files, along with all the scratch files used during verification, reside
in this directory, which is managed entirely by the Verifier.  The user
should not alter any file in the work file directory at any time.
.H 4 "The putrules utility"
The
.B putrules
utility reads a
knowledge base file and creates a
.B ruledatabase
file.  It is invoked with the call
.DP
.ce
putrules <knowledge base> <program>_d
.DE
where 
.B program
is the name of the program (not including the trailing 
.B ".pf\c"
) being verified.
Putrules can only be run after the verification of the program has been tried
at least once and the Verifier's work directory
.B "<program>_d"
created by the Verifier.
After any change to the knowledge base, it is necessary to rerun 
.B putrules
to make the changes in the knowledge base available to the Verifier.
.P
If the changes to the knowledge base included the alteration or deletion
of any rule function definition, 
.B putrules
will print a message so stating and will clear the history of
successfully verified routines, so that a full reverification will
be performed the next time the Verifier is invoked.
This check is required to insure soundness.
.H 3 "Standard knowledge bases"
Standard knowledge bases may be created.  
A knowledge base may contain information useful for verifying more than
one program.
For example, the information about
.B allzero
above would be useful in any program that cleared arrays to zero.
One useful knowledge base
is the Rule Builder's built-in knowledge base.
This knowledge base is called 
.B verifier
and is usually sufficient (and necessary) for programs which contain
no user-provided rule functions but do contain instances of the
3-argument form of
.B DEFINED
for proving the definedness of parts of arrays.
.P
Usually a copy of this knowledge base is maintained in a well-known place on
each system on which the Verifier is installed.
But a copy of this knowledge base can be obtained, if required, by invoking the
Rule Builder and, without proving any new theorems, using the
Rule Builder command
.DP
.ce
(MAKE-LIB 'verifier)
.DE
which will create the knowledge base files
.I verifier.lib 
and
.I verifier.lisp\c
in the current directory.
