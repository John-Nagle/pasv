.nr Hc 2
.nr Hs 9
.nr Hb 9
.nr Ej 2
.nr Pt 0
.rm ul
.rm cu
.TL
Pascal-F Verifier Internal Design Document

Changes required to run Boyer-Moore Theorem Prover 
on stock Version 4 TOPS-20.
.AF "FACC / Palo Alto"
.AU "John Nagle" JBN "" "Software Technology"
.PH "''Pascal-F Verifier Design'Page \\\\nP'"
.PF "'Draft 1.3'CPCI #6 -- Theorem Prover Installation'2/17/83'"
.MT 4
.SA 1
Assuming that INTERLISP was not previously available, the following
changes appeared to be required:
.AL
.LI
The file <LISP>LISP.SAV must be renamed <LISP>LISP.EXE.
.LI
The same change applies to other executable files.
.LI
To run MAKESYSOUT, it is necessary to change MAKESYSOUT
so that LISP is invoked with "RUN <LISP>LISP" instead of
just "<LISP>LISP".
.LI
The file <LISP>TOPS20.RELEASE must be created.  I put a
"3", in here, matching SRI, even though our system runs
version 4.  This may be questionable.
.LI
The file CLAUSIFY.COM had to be filled with a null readable
LISP makefile before MAKESYSOUT would run.  I used
.DS
	(CAR NIL)
	STOP
.DE
which worked.
.LI
The prover, when compiling a DEFN, sends the message output of
the INTERLISP compiler to the device "NIL:".  This device apparently
is non-standard.  It does not exist on
our system.
.LE
