.nr Hc 2
.nr Hs 9
.nr Hb 9
.nr Ej 0
.rm ul
.rm cu
.ND "2/7/83"
.TL
Pascal-F Verifier Internal Design Document

CPCI #1 -- Syntactic Processing
.AF "FACC / Palo Alto"
.AU "Scott D. Johnson"
.AU "John Nagle"
.PF "'Draft 1.3'CPCI #1 -- Syntactic Processing'2/7/83'"
.MT 4
.SA 1
.H 1 "Introduction"
Phase 1 of the Pascal-F Verifier is a modified version of the
Pascal-F compiler.
Much of the same source text is used in both systems.
The source text is divided into the sections given below.
Both the first pass of the Verifier and a modified first pass of the
compiler can be made from these files.  
The first pass of the compiler is built by compiling P1X-CMP.p; 
the first pass of the Verifier is built by compiling P1X-VER.p.
The modified first pass of the
compiler accepts the syntax defined in the Verifier manual but discards
all verifier-only items.
.H 1 "Source files"
.VL 22 2
.LI "ERMSG-TXT"
Error message text.  The program looks for this file in
/usr/p/frl/bin.d/ERMSG-TXT when looking up error messages.
Used both by the compiler and the Verifier.
.LI "P1X-CMP.p"
Main for compiler; consists only of includes.
.LI "P1X-PAS00.i"
Compiler/verifier common files.
.LI "P1X-PAS01.i"
.LI "P1X-PAS02.i"
.LI "P1X-PAS03.i"
.LI "P1X-PAS04.i"
.LI "P1X-PAS05.i"
.LI "P1X-PAS06.i"
.LI "P1X-PAS07.i"
.LI "P1X-PAS08.i"
.LI "P1X-PAS09.i"
.LI "P1X-PAS10.i"
.LI "P1X-PAS11.i"
.LI "P1X-PAS12.i"
.LI "P1X-PAS13.i"
.LI "P1X-PAS14.i"
.LI "P1X-UNIX00.i"
Unix-only files, used by both compiler and Verifier when running on UNIX.
.LI "P1X-UNIX01.i"
.LI "P1X-UNIX02.i"
.LI "P1X-UNIX03.i"
.LI "P1X-VER.p"
Verifier main program; consists only of includes.
.LI "P1X-VER00.h"
Verifier definitions file for constants.
This file is also used by pass 2 of the Verifier.
.LI "P1X-VER00.i"
Verifier constants not needed in pass 2.
.LI "P1X-VER01.h"
Verifier definitions file for types.
This file is also used by pass 2 of the Verifier.
.LI "P1X-VER02.i"
Verifier variables.
.LI "P1X-VER07.i"
Code added for verifier use is here, where entire procedures and functions
were added.  The primary addition is the symbol table output processor,
which produces the file "pasf-vars".
.LE
.H 1 "Inputs and outputs"
.H 2 "Input files"
The only input file is the source program.  Includes are understood with
Pascal-F include syntax but with UNIX pathnames in the include form.
.H 2 "Output files"
.VL 22 2
.LI "pasf-data"
VALUE clause constant values; always produced.
.LI "pasf-files"
List of files included; used with pasf-source to get the names of files
for diagnostics.  This is a text file.
.LI "pasf-icode"
The ICODE (or RCODE) file; used by both the compiler and the verifier.
.LI "pasf-source"
Source code in a format for random access lookup;
produced only by Verifier and used by pass 2 to produce diagnostics.
.LI "pasf-symbols"
Symbol table for compiler; produced by both versions.
.LI "pasf-vars"
Variable file for verifier; produced only by Verifier version.
.LE
