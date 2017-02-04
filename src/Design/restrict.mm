.nr Hc 1
.nr Hs 9
.nr Hb 9
.nr Ej 0
.nr Pt 0
.rm ul
.rm cu
.TL
Pascal-F Verifier Internal Design Document

Enforcement of Language Restrictions
.AF "FACC / Palo Alto"
.AU "John Nagle" JBN "" "Software Technology"
.PH "''Pascal-F Verifier Design'Page \\\\nP'"
.PF "'Draft 1.9'Enforcement of Language Restrictions'10/6/82'"
.MT 4
.SA 1
.H 1 "Introduction"
The validity of verification depends on the way the language is used.
We find it necessary to impose various restrictions on the use of
Pascal-F in order that the verification process remain valid.
This document details these restrictions and the way in which they are
enforced.
.P
The primary purpose of this document is to help insure that no
restriction is unenforced.  The Verifier is expected to enforce all
restrictions required for the verification process to be valid.
Some of these restrictions are enforced in the compiler pass
(CPCI #1), some are enforced during preverification checking (CPCI #2),
and some are enforced in the verification condition generator,
(CPCI #3).
.H 1 "Restrictions on program structure"
.H 2 "Routine nesting"
.VL 22 2
.LI "RESTRICTION"
Routines may not be defined within other routines
.LI "ENFORCEMENT"
This restriction has been dropped.
.LE
.H 2 "Non-terminating loops"
.VL 22 2
.LI "RESTRICTION"
The only legitimate way to write a non-terminating loop
for a process is ``WHILE true DO BEGIN <loop body> END;
or ``REPEAT <loop body> UNTIL false;''.
.LI "ENFORCEMENT"
Enforced in Jcode generation.
.LE
.H 2 "FOR control variable"
.VL 22 2
.LI "RESTRICTION"
The iteration variable of a FOR loop may not be modified from within the
loop.
.LI "ENFORCEMENT"
Preverification checking generates FREEZE and THAW operations
as required during Jcode generation.
.LE
.H 1 "Variant Records"
Variant records are presently not implemented, because of the difficulty
of determining from Icode which variant is being accessed.  This is
not a fundamental problem and could be overcome; however, the restrictions
below would then need to be enforced.
.H 2 "Tag field presence"
.VL 22 2
.LI "RESTRICTION"
Undiscriminated variants are not permitted.
.LI "ENFORCEMENT"
Considered a syntax error to define a record with undiscriminated variants.
.LE
.H 2 "Variant access"
.VL 22 2
.LI "RESTRICTION"
A field in a variant record may be accessed only when the tag field of the
record has the same value as the constant that labels the variant.
.LI "ENFORCEMENT"
A REQUIRE (EQUAL <tag field> <variant #>) will be generated
during Jcode generation for each reference to a field in a variant record.
.LE
.H 2 "Variant erasure"
.VL 22 2
.LI "RESTRICTION"
Whenever the tag field is changed, all the fields of all the variants
selected by the tag field cease to be defined.
.LI "ENFORCEMENT"
A NEW <field> will be generated for each field subordinate to the
changed tag.  Performed during Jcode generation.
.LE
.H 1 "Exception handling"
.H 2 "RAISE statement"
.VL 22 2
.LI "RESTRICTION"
The Verifier will try to prove that no RAISE statement is ever executed.
.LI "ENFORCEMENT"
REQUIRE (false) will be generated for each RAISE statement.
.LE
.H 1 "Aliasing and side effects"
.VL 22 2
.LI "RESTRICTION"
No distinct symbolic names are ever allowed to refer
in the same context to overlapping storage.
.LI "ENFORCEMENT"
The only place this problem can occur in PASCAL-F is
via the procedure call mechanism.  The processing
done in the safeexpr routine generated includes a
check sufficient to prevent aliasing.
.LI "RESTRICTION"
No variable can be modified twice in the same statement,
or modified and referenced in two different parts of the
same statement.
.LI "ENFORCEMENT"
Once again, the only culprit is the routine call.
The processing done in the safeexpr routine includes
a check sufficient to prevent conflicting side effects.
.H 1 "Multiprogramming"
.H 2 "Data sharing between processes"
.VL 22 2
.LI "RESTRICTION"
Non-process-local data cannot be shared between code executing at
different priorities under any circumstances whatsoever.
This is to be enforced by the restriction that
no variable other than a simple variable of type
SIGNAL may be imported into or exported from
a monitor.
.LI "ENFORCEMENT"
Enforced during compiler processing of IMPORT lists.
.LE
.H 2 "Records containing signals"
.VL 22 2
.LI "RESTRICTION"
Signals may not be components of arrays or records.
.LI "ENFORCEMENT"
[NOT PRESENTLY ENFORCED]
.LE
.H 2 "Arrays of signals"
.VL 22 2
.LI "RESTRICTION"
Arrays of signals will be supported.  However, where an array of signals
is referenced by WAIT operations from two processes, the subscripts
must be constant, so that a static check can insure that no more than one
WAIT can reference a given signal.  This restriction does not apply to
SEND operations.
.LI "ENFORCEMENT"
[NOT PRESENTLY ENFORCED]
.LE
.H 2 "Signals may not be passed as function or procedure arguments"
.VL 22 2
.LI "RESTRICTION"
Signals may not be passed as function or procedure arguments.
.LI "ENFORCEMENT"
Enforced during compiler processing of argument lists.
.LE
.H 2 "Export of procedures and functions"
.VL 22 2
.LI "RESTRICTION"
Procedures and functions may be exported from a monitor only into a
lower priority environment.
.LI "ENFORCEMENT"
Enforced in both the compiler and Jcode generation phases.
.LE
.H 2 "Import of procedures and functions"
.VL 22 2
.LI "RESTRICTION"
Procedures and functions may be imported into a monitor only into a
lower priority environment.
.LI "ENFORCEMENT"
Enforced in both the compiler and Jcode generation phases.
.LE
.H 2 "Data is shared between processes only through procedures or functions"
.VL 22 2
.LI "RESTRICTION"
Data is shared between processes only through procedures or functions
exported from the higher priority monitor and called from the lower
priority monitor.
.P
This is a consequence of the above decisions.
.LI "ENFORCEMENT"
Implicit in rules above.
.LE
.H 2 "Export of signals"
.VL 22 2
.LI "RESTRICTION"
Simple variables of type SIGNAL may be exported from a
monitor only into an environment of lower priority.
The additional restriction is imposed that the SEND operation is
permitted only on SIGNAL variables explicitly defined in the current
environment, i.e. not imported into the current environment.
.LI "ENFORCEMENT"
Enforced in the compiler.
(to be supplied)
.LE
.H 2 "Import of signals"
.VL 22 2
.LI "RESTRICTION"
Simple variables of type SIGNAL may be imported only into
a monitor of lower priority than that of the monitor's environment.
.LI "ENFORCEMENT"
(to be supplied)
.LE
.H 2 "Synchronous signals"
.VL 22 2
.LI "RESTRICTION"
Signals which are not imported or exported across a monitor
boundary are referred to as ``synchronous signals''.  A SEND on
such a signal must cause immediate activation of a process waiting
on the signal.
.P
Synchronous signals will not be implemented in Pascal-F at this time.
This is an implementation restriction.
.LI "ENFORCEMENT"
Treated as asynchronous signals
.LE
.H 2 "Asynchronous signals"
.VL 22 2
.LI "RESTRICTION"
Signals which are imported or exported across a monitor boundary are
referred to as ``asynchronous signals''.  A SEND on such a signal
must never cause deactivation of the sending process.  (Note that the
restrictions on export and import of signals imply that such a send
must be a ``send down''.)
.LI "ENFORCEMENT"
(to be supplied)
.LE
.H 2 "Use of the AWAITED function is limited to synchronous signals"
.VL 22 2
.LI "RESTRICTION"
Use of the AWAITED function is limited to synchronous signals.
.P
Initially, the AWAITED function will not be implemented.
.LI "ENFORCEMENT"
Compiler considers any use of AWAITED to be an error
.LE
.H 2 "Signal / wait relationship"
.VL 22 2
.LI "RESTRICTION"
Each SIGNAL variable is to
be mentioned in exactly one WAIT statement.
.LI "ENFORCEMENT"
This restriction appears to be unnecesary.
.LE
.H 2 "Reachability of wait statements"
.VL 22 2
.LI "RESTRICTION"
No WAIT statement is to be statically reachable by more than one process.
.P
.LI "ENFORCEMENT"
See below.
.LE
.H 2 "WAIT operation in routines"
.VL 22 2
.LI "RESTRICTION"
The initial compiler implementation will also enforce the restriction
that the WAIT operation is forbidden within procedures and functions.
This implicitly
implies that no WAIT statement can be reached by more than one
process.
.LI "ENFORCEMENT"
During compilation of WAIT statements
.LE
.H 2 "Calls out of monitors"
.VL 22 2
.LI "RESTRICTION"
Routines imported into monitors are classified as either
``atomic'' or ``non-atomic''.  Atomic routines do not perform
WAIT or SEND operations, nor do they call routines that do.
For non-atomic routines, each call of the routine is considered an
exit from the monitor and the monitor invariant must be proven.
.LI "ENFORCEMENT"
Enforced by check performed after call graph has been closed.
.LE
.H 1 "PROOF and EXTRA information"
.H 2 "Use of EXTRA variables"
.VL 22 2
.LI "RESTRICTION"
EXTRA variables may not be mentioned in non-PROOF executable statements,
except that EXTRA variables may be passed to EXTRA formal arguments
to non-PROOF procedures.
.LI "ENFORCEMENT"
In compiler, along with usual type checking.
.LE
.H 2 "Assignment to non-EXTRA variables by PROOF code"
.VL 22 2
.LI "RESTRICTION"
No PROOF statement or component of a compound PROOF statement
may perform an assignment upon a non-PROOF variable.
.LI "ENFORCEMENT"
The compiler, when compiling statements, is either in ``PROOF mode''
or ``non-PROOF mode''.
PROOF mode is on during PROOF statements and when compiling EXTRA procedures.
When in PROOF mode, assignments to non-PROOF
variables are forbidden.
.LE
.H 2 "Calls to routines from PROOF code"
.VL 22 2
.LI "RESTRICTION"
PROOF code may not call non-EXTRA procedures.
[OPEN ITEM: Levelless functions]
.LI "ENFORCEMENT"
When in PROOF mode, the compiler considers calls to
non-EXTRA procedures to
be errors.
.LE
.H 1 "Scope of names"
.H 2 "ENTRY and EXIT name usage"
.VL 22 2
.LI "RESTRICTION"
The only variable names permitted in ENTRY and EXIT declarations are
those of formal arguments, constants, and global variables visible
in all scopes to which the routine name has been exported.
.LI "ENFORCEMENT"
Enforced in Jcode generator.
.LI "RESTRICTION"
If a name appears in an ENTRY or EXIT declaration in a routine R,
that name must appear in an executable statement or EXTRA statement
in R routine, or in an a routine
that can be (directly or indirectly) called by R.
.LI "ENFORCEMENT"
When routine calls are augmented by implicit parameters,
every name appearing in the entry or exit condition must
be an implicit or explicit parameter of the routine.
.LE
.H 2 "Variables in INVARIANT declarations"
.VL 22 2
.LI "RESTRICTION"
Variables in module and monitor INVARIANT declarations must be defined in
the same module or monitor header as is the invariant.
.LI "ENFORCEMENT"
Enforced in Jcode generator.
.LE
.H 2 "Calls out of modules and monitors"
.VL 22 2
.LI "RESTRICTION"
Routines imported into monitors or modules may not make calls to
routines exported from that same monitor, either directly or through
intervening routines.
.LI "ENFORCEMENT"
Enforced by check performed after call graph has been closed.
.LE
.H 2 "Open items - scope"
.RL
.LI
What about functions in ENTRY, EXIT, and EFFECT declarations?
.LI
What about functions in INVARIANT statements?
.LI
What about routine INVARIANTS?
.LE
.H 1 "Specification statements"
.H 2 "STATE statement in loops"
.VL 22 2
.LI "RESTRICTION"
One and only one STATE statement must be written for every loop.
The STATE statement must be contained in the loop body, at the top level.
.LI "ENFORCEMENT"
Enforced in Jcode generation.
.LE
.H 2 "MEASURE statement in loops"
.VL 22 2
.LI "RESTRICTION"
Every WHILE, REPEAT, and LOOP loop must contain a MEASURE statement.
The MEASURE statement must immediately follow the STATE statement
associated with the loop.
.LI "ENFORCEMENT"
Enforced in Jcode generation.
.LE
.H 1 "Forms in expressions"
.H 2 "Conditional expression type matching"
.VL 22 2
.LI "RESTRICTION"
The expression after the THEN and the expression after the ELSE must
have identically the same type.
.LI "ENFORCEMENT"
Enforced in compiler, during normal expression type checking.
.LE
.H 2 "OLD psuedofunction use"
.VL 22 2
.LI "RESTRICTION"
OLD can only be used in assertions, can only be used within routines, and
can only be applied to parameters of the routine and variables global to
the routine.
.LI "ENFORCEMENT"
Enforced in the compiler.
.LE
