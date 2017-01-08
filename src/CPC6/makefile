#
#	Makefile for Pascal-F Rule Builder  (Franz Lisp version)
#
#					Version 1.3 of 2/24/86
#
SOURCE=..
CPC6=$(SOURCE)/cpc6

THM=thm
#
#	Build the rulebuilder, given the rule base.
#
rulebuilder: verifier.lib verifier.lisp mkrulebld.l
	$(THM) < mkrulebld.l
	rm -f rulebuilder
	mv savedlisp rulebuilder
	chmod 555 rulebuilder
#
#	Prove the rule base.
#
verifier.lib:
	$(THM) < mkveriflib.l
verifier.lisp:

#
#	SCCS dependencies
#
mkrulebld.l:	$(CPC6)/src/s.mkrulebld.l ; get $(CPC6)/src/s.mkrulebld.l
mkveriflib.l:	$(CPC6)/src/s.mkveriflib.l ; get $(CPC6)/src/s.mkveriflib.l
