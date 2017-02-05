.nr Hb 9
.nr Ej 0
.rm ul
.rm cu
.TL
Pascal-F Verifier Internal Design Document

Design Overview
.AF "FACC / Palo Alto"
.AU "John Nagle" JBN "" "Software Technology"
.AU "Scott Johnson"
.PF "'Draft 1.12'Design Overview'4/30/81'"
.PH "'Pascal-F Verifier Design''Page \\\\nP'"
.MT 4
.SA 1
.H 1 "Overall System Design"
.H 2 "CPCI #1 -- Language Processing"
.H 3 "Phase 1A"
.DS
.ft CR

                       *----------*
                       |Dictionary|            *------*
                       *----------*            |Source|
                            |                  |Line  |
                   *--------------------*      |File  |
                   |                    |----->*------*
                   |                    |
   *------*        |                    |      *-----*
   |Source|        |    Phase One       |----->|Icode|
   |Input |------->|    of Pascal-F     |      *-----*
   |Stream|        |    Compiler        |
   *------*        |    (modified)      |      *------------------*
                   |                    |----->|Routine Interfaces|
                   |                    |      *------------------*
                   |                    |
                   |                    |----->*-------*
                   *--------------------*      |Error  |
                                               |Message|
                                               |Stream |
                                               *-------*

.ft
.FG "Phase 1A -- Syntax and Type Checking"
.DE
The syntax and type checking phase has the task of accepting Pascal-F
input and producing a representation of the program in a reverse Polish
notation called ``Icode''.
This phase is also required to perform the normal
checks expected of a Pascal compiler, including syntax checks and
variable type checks.
.P
Phase 1 of the Pascal-F compiler will be
modified to accept the additional language constructs required for
verification and will then become phase 1 of both the compiler and the
veriifer.
.P
Inputs and outputs of this phase are as follows:
.VL 22 2
.LI "Source Input"
An ASCII text file containing the source program.  Read only.
.LI "Source Lines"
A file of source lines indexed in such a way that any line can be
retrieved by line number.  This file exists to allow
retrieval of program text for use in diagnostic messages of later phases.
Write only in this phase.
.LI "Dictionary"
A file of named objects, indexed by name and scope, containing the
properties of the objects.  This file is a representation of the
compiler's dictionary.  Created in this phase.
.LI "Routine Database"
A file of routines and properties thereof.
This information includes
.BL
.LI
The list of procedures and functions directly called by each procedure
and function
.LI
The list of imported variables directly
referenced by each procedure and function.
.LI
The list of imported variables directly
altered by each procedure and function.
.LI
The entry and exit assertions of each procedure and function.
.LI
The invariants of each module and monitor.
.LI
The priority of each procedure, function, monitor, and module.
.LI
The containing monitor or module of each procedure, function, monitor, and
module.
.LI
For each procedure and function, answers to the questions:
.AL
.LI
Does the routine perform a ``wait'' operation?
.LI
Does the routine perform a ``send'' operation?
.LE
.LE
.P
This list may
not be complete.  Because the final definition of the Pascal-F language
has not been received, it is not yet possible to be sure that
all required information about routines is listed above.
.P
Created in this phase.
.LI "``Icode''"
The reverse Polish form of the program text.  A sequential file,
readable forward only.  Created in this phase.
.LI "Error Messages"
User-readable error messages.  Standard output.
.LE
.H 2 "CPCI #2: Preverification Checking and Jcode Generation"
.H 3 "Phase 2A -- Transitive Property Propagation"
.DS
.ft CR

                     *-----------------*
   *---------*       |                 |
   |Routine  |       |  General        |
   |Interface|<----->|  Transitivity   |
   |Database |       |  Propagator     |
   *---------*       |                 |
                     *-----------------*

.ft
.FG "Transitive Property Propagation Phase"
.DE
.P
The task of this phase is to perform transitive closure on those
properties in the Routine Interface Database which are transitive.
For example,
the knowledge that "procedure A calls procedure B" and
"procedure B modifies variable X" would be
combined to produce the information
that "procedure A modifies variable X".
This new information would then be added to the Routine Interface
Database.
The following properties are considered transitive:
(This list may not be complete).
.AL
.LI
A calls B
.LI
A modifies X
.LI
A references X
.LI
A contains a ``wait''
.LI
A contains a ``send''
.LE
.P
Inputs and outputs of this phase are as follows:
.VL 22 2
.LI "Interface Database"
Read/write in this phase.
.LE
.H 3 "Phase 2B -- Jcode Generation"
.DS
.ft CR
                                                    *------*
                         *----------*               |Source|
                         |Dictionary|               |Line  |
                         *----------*               |File  |
                              |                     *------*
                              v                        |
        *-----*    *------------------------*    *---------------*
        |Icode|--->|                        |    |   Error       |
        *-----*    |                        |--->|   Message     |
                   |    Icode-to-Jcode      |    |   Generator   |
                   |      Translator        |    *---------------*
   *----------*    |                        |
   |Routine   |--->|                        |    *-----*
   |Interface |    |                        |--->|Jcode|
   |Database  |    *------------------------*    *-----*
   *----------*

.ft
.FG "Jcode Generation"
.DE
.P
Jcode is a representation of the source program designed to accomodate
the needs of the verification condition generator.  Jcode is defined by
the ``Jcode Interface Control Document''.
.P
Icode contains addresses of variables; Jcode contains their names.
The names of variables are obtained by searching an internal table of
variables ordered by address, which is created from the dictionary.
.P
Inputs and outputs of this phase are as follows:
.VL 22 2
.LI "Icode file"
Read only in this phase.
.LI "Dictionary"
Used to obtain definitions of variables and records.
Read only in this phase.
.LI "Source Lines"
Read only in this phase.
.LI "Interface Database"
Read only in this phase.
.LI "Jcode"
Created in this phase.
.LI "Error messages"
Standard output.
.LE
.H 2 "CPCI #3: Verification Condition Generation"
.H 3 "Phase 3A -- Jcode Augmentation"
.DS
.ft CR


               *---------*
               |Routine  |
               |Interface|
               |Database |
               *---------*
                    |
             *------------------*
 *-----*     |                  |     *---------*
 |Jcode|---->|    Jcode         |---->|Augmented|
 *-----*     |    Expander      |     |Jcode    |
             |                  |     *---------*
             |                  |
             *------------------*
                      |
                      v
 *------*    *-----------------*
 |Source|    |                 |     *--------*
 |Line  |--->|  Error Message  |---->|Error   |
 |File  |    |  Printer        |     |Messages|
 *------*    |                 |     *--------*
             *-----------------*

.ft
.FG "Phase 3A -- Jcode Augmentation"
.DE
The primary purpose of Jcode augmentation is the expansion of
procedure and function calls into assertions about the variables
involved in the calls.  The description of a routine in augmented
Jcode stands alone; no information external to the Jcode is required
to define the information to be proven about the routine.
If the augmented Jcode of a routine does not change
from one verification attempt to the next, there is no need to reverify the
routine.
.H 3 "Phase 3B -- Verification Condition Generation"
.DS
.ft CR



                                            *--------*
                          *-------------*   |Source  |
                          |             |   |Line    |
                          |  Error      |-->|File    |
                          |  Message    |   *--------*
                          |  Generator  |
                          |             |-->*--------*
                          *-------------*   |Error   |
                                ^           |Messages|
                                |           *--------*
                                |
                                |
       *---------*    *---------------*
       |Augmented|    |               |        *--------*
       |Jcode    |--->|  VC Generator |        |Proved  |
       |File     |    |  Supervisor   |<------>|Routines|
       *---------*    |               |        |Database|
                      *---------------*        *--------*
                        ^            |
                        |            v
                        |      *--------------*
                        |      |              |
                        |      |    Path      |
                        |      |  Traverser   |
                        |      |              |
                        |      *--------------*
                        |             |
                        |             v
                        |   *---------------------*
                        |   |                     |
                        |   |    Verification     |
                        |   |     Condition       |
                        |   |     Generator       |
                        |   |                     |
                   error|   *---------------------*
                messages|             |
                    from|             |formulae
                 theorem|             |to be proven
                 proving|             v

.ft
.FG "Phase 3B - Verification Condition Generation"
.DE
.P
It is the task of this phase to generate the verification conditions
for which proof will be attempted.
This phase interacts directly
with the next phase, in that each verification
condition, as it is generated, is submitted to the theorem prover, so that
the success or failure of the proof is immediately available to the
verification condition generator supervisor.
.P
Inputs and outputs of this phase are as follows:
.VL 22 2
.LI "Jcode file"
Read only in this phase.
.LI "Proved Routines"
This database is used to avoid reverification of unchanged routines.
If the augmented Jcode for a routine has not changed since the last
verification attempt, verification condition generation and theorem proving
are ommitted.
.P
The error messages generated during theorem proving are saved in this
database, so that reverification attempts produce the same error messages
as previous tries.
.LI "Source Line File"
Read-only in this phase.
.LI "Error Messages"
Standard output.
.LE
.H 2 "CPCI #4: Theorem Proving"
.DS
.ft CR

                        ^             v
                   error|             |formulae to
                messages|             |be proven
                    from|             |
                  prover|             |
                        |   __________|
                        |   |
                        |   |
                        |   v
            *------------------*
            |   Theorem        |
            |   Prover         |
            |   Supervisor     |
            *------------------*
                ^           ^
                |           |
                v           v
            *--------*   *--------------------*
            |Proved  |   |                    |
            |Theorems|   |   Theorem Prover   |
            |Database|   |                    |
            *--------*   *--------------------*

.ft
.FG "Theorem Proving Phase"
.DE
.P
Inputs and outputs of this phase are as follows:
.VL 22 2
.LI "VC file"
Read only in this phase.
.LI "Proved Theorems"
A dictionary of proved theorems used to avoid resubmitting
already-proved (or disproved) conjectures to the theorem prover.
This database is preserved from run to run of the verifier as a file
associated with the program being verified.
Read/write in this phase; may be created empty if nonexistent.
.LI "Error Stream"
Error messages.  Write only.
.LE
