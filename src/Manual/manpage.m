.SK
.SP 4
.de hu				\" heading for manual page section
.SP
.ps -2
.in 0
.nf
.ft B
\\$1
.ft P
.fi
.ps +2
.in 4
..
.hu "NAME"
pasver - Pascal-F verifier
.hu "SYNOPSIS"
.B "pasver \c"
[\c
.B "-d\c"
] [file]
.hu "DESCRIPTION"
.I "pasver"
examines Pascal-F programs and attempts to prove the absence
of certain types of run-time errors.
When invoked with file names as arguments,
.I "pasver" initiates verification of the Pascal-F program identified by
the 
.I "file"
argument.  This name must end in ``.pf''.
A directory will be created for the many scratch files used during the
verification.  This directory will be created in ``.'', and will
have the name of the file being verified, with the ``.pf'' changed to
``.d''.  If a directory by that name already exists, the verifier will
expect it to contain results from a previous verification attempt.
Reverifications are much faster than original verifications.
.P 0
The
.B "-d"
flag enables all internal debug output, which is extensive.
The
.B -d1 turns on pass one debug output only,
.B -d2 turns on pass two debug output,
and 
.B -d3 turns on debug output from pass 3 and the theorem prover.
The flag
.B -dvcg causes a log of all verification conditions to be generated.
.hu "FILES"
.I Pasver invokes
.I pasver1,
.I pasver2,
.I vcg,
.and
.I simplifier, all of which are searched for by path name.
.hu "SEE ALSO"
.I "Pascal-F Verifier User's Manual"
by S. Johnson and J. Nagle.
.in 0
.SK
