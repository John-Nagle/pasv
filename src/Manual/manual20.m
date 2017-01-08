.H 1 "Writing verifiable Pascal-F"
Programs to be verified must in a sense be 
``understood''
by the
Verifier, for which it
needs a substantial amount of help from the programmer.
Most of this help is supplied in the form of special statements
embedded in the text of the program.  These statements are meaningless to the
Pascal-F compiler (though the compiler will recognize and ignore them),
but to the Verifier they supply information about how the program works.
