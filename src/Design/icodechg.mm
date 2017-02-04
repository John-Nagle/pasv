.nr Hc 2
.nr Hs 9
.nr Hb 9
.nr Ej 0
.nr Pt 0
.rm ul
.rm cu
.TL
Pascal-F Verifier Internal Design Document

Icode Changes by Compiler Level
.AF "FACC / Palo Alto"
.AU "John Nagle" JBN "" "Software Technology"
.PH "''Pascal-F Verifier Design'Page \\\\nP'"
.PF "'Draft 1.2'Icode Changes by Compiler Level'4/6/83'"
.MT 4
.SA 1
.HU "Introduction"
From time to time, Ed Nelson at Ford Motor 
Scientific Research Lab releases a new version of
the Pascal-F compiler.  This usually contains undocumented changes in the
format of the intermediate code produced by Pass 1.  This document
covers the undocumented changes found by comparing versions of the compiler.
.HU "Changes found between versions 1.5c to 1.8"
.AL
.LI
The offset argument of the FIELD operator is now a word rather than a byte.
.LI
The temporary number of an RTEMP is now a word rather than a byte.
.LI
The temporary number of an DTEMP is now a word rather than a byte.
.LI
The routine number of the CALL operator is now a word rather than a byte,
and that field is now last.
.LI
The INVOK operator now takes a word rather than a byte, but is unimplemented.
.LI
The ENABL operator now generates a REFER before the signal rather than
referencing the signal directly.
.LI
The count of cases for the CASE statement is now a word rather than a byte.
.LE
