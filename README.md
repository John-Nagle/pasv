# pasv
The Pascal-F Verifier

The Pascal-F Veriifer is an early proof-of-correctness system.
It was developed between 1982 and 1985 and works on a dialect
of Pascal used for real-time programming.

It ran on UNIX 4.x BSD on early VAX and Sun systems in the 1980s.
The plan is to bring it back to life as a milestone in the history
of program verification.

The manual is here:

   http://www.animats.com/papers/verifier/verifiermanual.pdf
   
## Current status (1 FEB 2017)

### Pass one (CPC1)
Converted to Free Pascal and working.

### Pass two (CPC2)

Converted to Free Pascal and working.

### Pass 3, Pascal part (CPC3)

Conversion not started. This program runs the LISP theorem prover
in a subprocess, so the theorem prover is being converted first.

### Pass 4, theorem prover in LISP (CPC4)

Partially converted to GNU Common LISP. The built-in theories of the Oppen-Nelson simplifier 
appear to be working, but added rewrite rules are not working yet. 
	
## Original copyright notice

Permission is hereby given to modify or use, but not for profit,
any or all of this program provided that this copyright notice 
is included:

  Copyright 1985

  Ford Motor Company
  
  The American Road
  
  Dearborn, Michigan  48121

This work was supported by the Long Range Research Program of
the Ford Motor Company, and was carried out at Ford Scientific
Research Labs in Dearborn, Michigan and Ford Aerospace and
Communications Corporation's Western Development Laboratories
in Palo Alto, California.
