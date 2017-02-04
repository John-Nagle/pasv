.nr Hc 2
.nr Hs 9
.nr Hb 9
.nr Ej 0
.nr Pt 0
.rm ul
.rm cu
.TL
Pascal-F Verifier Internal Design Document

CPCI #5 -- Main Control Program and Other Utilities
.AF "FACC / Palo Alto"
.AU "John Nagle" JBN "" "Software Technology"
.PH "''Pascal-F Verifier Design'Page \\\\nP'"
.PF "'Draft 1.2'CPCI #5 -- Main Control Program'2/7/83'"
.MT 4
.SA 1
.H 1 "Introduction"
CPCI #5 is a collection of small parts of the Verifier.  Each of the
programs here is quite small and is documented with comments in the
source text.  Here, only an overview is provided.
.H 1  "Pasver"
The main control program is the program invoked when the user types
.DS
	pasver	<programname>
.DE
and is a UNIX Bourne shell procedure.  It loads, in order, the
programs 
.B pasver1,
.B pasver2,
.B jcheck,
and
.B jver.
The use of 
.B pasver
is described in the Verifier manual.
.H 1 "Jcheck"
Jcheck is a syntax checker for the internal form 
.B jcode
emitted by 
.B pasver2 
and read by
.B jver.
It has no other function than to catch internal errors in 
.B pasver2.
Although it has no function other than internal error trapping, it is always
run, just as a safety measure.
.H 1 "Putrules"
Putrules is a Bourne shell procedure for moving rules from a Boyer-Moore
knowledge base to a Verifier database.
The use of 
.B putrules
is described in the Verifier manual.
It invokes
.B getrules.
.H 1 "Getrules"
.B Getrules 
is a program which does the work of scanning a Boyer-Moore database
and finding all Boyer-Moore events.  Events are copied to the output; other
items in the database are ignored.
There are two versions of 
.B getrules,
one in C and one in Pascal.  They function identically but the C version is
about 10 times as fast.
