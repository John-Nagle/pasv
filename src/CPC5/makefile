
SOURCE=..
DEST=../bin
PASS5=$(SOURCE)/cpc5/src
#
#	Makefile for Verifier utility programs
#
#					Version 1.17 of 2/6/86
#
#	pasver	- the main program of the Verifier (a shell procedure)
#	putrules- the utility which moves rules from Boyer-Moore to Verifier
#	getrules- a program used by putrules to scan databases.  There
#		  are both C and Pascal versions of getrules; the
#		  C version is much faster but less portable.
#	pasver3b- the main program of the VCG pass; runs vcg and simplifier.
#	jcheck	- jcode syntax checker
#	
#
all: pasver pasver3b putrules getrules jcheck
install: pasver pasver3b putrules getrules jcheck
	-rm -f $(DEST)/pasver.old $(DEST)/pasver3b.old $(DEST)/putrules.old \
		$(DEST)/getrules.old $(DEST)/jcheck.old
	-mv $(DEST)/pasver $(DEST)/pasver.old
	-mv $(DEST)/pasver3b $(DEST)/pasver3b.old
	-mv $(DEST)/putrules $(DEST)/putrules.old
	-mv $(DEST)/getrules $(DEST)/getrules.old
	-mv $(DEST)/jcheck $(DEST)/jcheck.old
	ln pasver $(DEST)
	ln pasver3b $(DEST)
	ln putrules $(DEST)
	ln getrules $(DEST)
	ln jcheck $(DEST)
#
pasver: pasver.sh; rm -f pasver; cp pasver.sh pasver; chmod 555 pasver
#
pasver3b: jver.c
	cc -O jver.c
	rm -f pasver3b
	mv a.out pasver3b
	chmod 555 pasver3b
#
putrules: putrules.sh getrules
	rm -f putrules
	cp putrules.sh putrules
	chmod 555 putrules
#
#	getrules -- using C version
#
getrules: getrules.c
	cc -O -o getrules getrules.c
	chmod 555 getrules
#	
#	jcheck
#
jcheck: main.o lex.o y.tab.o
	cc main.o y.tab.o lex.o
	rm -f jcheck
	mv a.out jcheck
	chmod 555 jcheck

lex.o: lex.c ; cc -c -O lex.c
main.o: main.c ; cc -c -O main.c
y.tab.c: jcode.y; yacc jcode.y
y.tab.o: y.tab.c; cc -c -O y.tab.c
#
#	SCCS dependencies
#
pasver.sh: $(PASS5)/s.pasver.sh; get $(PASS5)/s.pasver.sh 
putrules.sh: $(PASS5)/s.putrules.sh; get $(PASS5)/s.putrules.sh
jver.c: $(PASS5)/s.jver.c; get $(PASS5)/s.jver.c
getrules.c: $(PASS5)/s.getrules.c; get $(PASS5)/s.getrules.c
getrules.p: $(PASS5)/s.getrules.p; get $(PASS5)/s.getrules.p
jcode.y: $(PASS5)/s.jcode.y; get $(PASS5)/s.jcode.y
main.c: $(PASS5)/s.main.c; get $(PASS5)/s.main.c
lex.c: $(PASS5)/s.lex.c; get $(PASS5)/s.lex.c
