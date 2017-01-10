
#
#		Makefile for Verifier pass two
#
#					Version 1.31 of 2/6/86
#                   Revised for Free Pascal in 2017.
#
DEST=../bin
SOURCE=..
PASS1=$(SOURCE)/CPC1
PASS2=$(SOURCE)/CPC2
POPTIONS=-gl -Co -Cr
#
#	make 			with no arguments makes pasver2
#	make install		makes and installs pasver2
#	make all		is equivalent to make 
#
all:	pasver2
#
#	install pasver2 in DEST
#
install: pasver2
	-rm -f $(DEST)/pasver2.old
	-mv $(DEST)/pasver2 $(DEST)/pasver2.old
	cp pasver2 $(DEST)/pasver2
#
P2SRC=p2consts.i p2iconst.h $(PASS1)/P1X-VER00.h $(PASS1)/P1X-VER01.h p2defs.i p2vars.i \
	p2tables.i p2tprint.i p2errors.i p2util.i p2block.i p2varfile.i \
	p2enforce.i p2build.i p2augment.i p2jgen.i p2jvars.i \
	p2select.i p2bound.i p2call.i p2expr.i p2stmt.i p2junit.i  \
	p2rdata.i p2loop.i p2routine.i p2dump.i p2alias.i p2spec.i \
	p2trmain.i p2trutil.i p2trinvar.i p2trvar.i \
	p2case.i p2wait.i p2icall.i p2jcommon.i p2recurse.i \
	p2trshare.i p2main.i p2.p
#
pasver2: $(P2SRC)
	rm -f pasver2
	fpc $(POPTIONS) p2.p | tee errs
	mv p2 pasver2
	chmod 555 pasver2
#
#
#	Utility programs  --  not part of pasver2 proper
#
#
#	Mergemsgs  --  merges messages from REQUIREs with source
#
mergemsgs: $(PASS2)/s.mergemsgs
	get $(PASS2)/s.mergemsgs
	chmod 755 mergemsgs
#
#	Finddiag  --  finds all internal error msgs and makes doc
#
finddiag: $(PASS2)/s.finddiag.sh
	get $(PASS2)/s.finddiag.sh
	rm -f finddiag
	mv finddiag.sh finddiag
	chmod 555 finddiag
#
internalerrs.m: $(P2SRC) finddiag
	cat $(P2SRC) | finddiag > internalerrs.m
