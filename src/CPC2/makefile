
#
#		Makefile for Verifier pass two
#
#					Version 1.31 of 2/6/86
#
DEST=../bin
SOURCE=..
PASS1=$(SOURCE)/cpc1/src
PASS2=$(SOURCE)/cpc2/src
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
	ln pasver2 $(DEST)
#
P2SRC=p2consts.i p2iconst.h P1X-VER00.h P1X-VER01.h p2defs.i p2vars.i \
	p2tables.i p2tprint.i p2errors.i p2util.i p2block.i p2varfile.i \
	p2enforce.i p2build.i p2augment.i p2jgen.i p2jvars.i \
	p2select.i p2bound.i p2call.i p2expr.i p2stmt.i p2junit.i  \
	p2rdata.i p2loop.i p2routine.i p2dump.i p2alias.i p2spec.i \
	p2trmain.i p2trutil.i p2trinvar.i p2trvar.i \
	p2case.i p2wait.i p2icall.i p2jcommon.i p2recurse.i \
	p2trshare.i p2main.i p2.p
#
pasver2: $(P2SRC)
	pc -g -w p2.p | tee errs
	rm -f pasver2
	mv a.out pasver2
	chmod 555 pasver2
#
p2consts.i:  $(PASS2)/s.p2consts.i
	get $(PASS2)/s.p2consts.i
p2iconst.h:  $(PASS2)/s.p2iconst.h
	get $(PASS2)/s.p2iconst.h
P1X-VER00.h:  $(PASS1)/s.P1X-VER00.h
	get $(PASS1)/s.P1X-VER00.h
P1X-VER01.h:  $(PASS1)/s.P1X-VER01.h
	get $(PASS1)/s.P1X-VER01.h
p2defs.i:  $(PASS2)/s.p2defs.i
	get $(PASS2)/s.p2defs.i
p2vars.i:  $(PASS2)/s.p2vars.i
	get $(PASS2)/s.p2vars.i
p2tables.i:  $(PASS2)/s.p2tables.i
	get $(PASS2)/s.p2tables.i
p2tprint.i:  $(PASS2)/s.p2tprint.i
	get $(PASS2)/s.p2tprint.i
p2errors.i:  $(PASS2)/s.p2errors.i
	get $(PASS2)/s.p2errors.i
p2util.i:  $(PASS2)/s.p2util.i
	get $(PASS2)/s.p2util.i
p2block.i:  $(PASS2)/s.p2block.i
	get $(PASS2)/s.p2block.i
p2varfile.i:  $(PASS2)/s.p2varfile.i
	get $(PASS2)/s.p2varfile.i
p2enforce.i:  $(PASS2)/s.p2enforce.i
	get $(PASS2)/s.p2enforce.i
p2build.i:  $(PASS2)/s.p2build.i
	get $(PASS2)/s.p2build.i
p2augment.i:  $(PASS2)/s.p2augment.i
	get $(PASS2)/s.p2augment.i
p2trutil.i: $(PASS2)/s.p2trutil.i
	get $(PASS2)/s.p2trutil.i
p2trvar.i: $(PASS2)/s.p2trvar.i
	get $(PASS2)/s.p2trvar.i
p2trinvar.i: $(PASS2)/s.p2trinvar.i
	get $(PASS2)/s.p2trinvar.i
p2trshare.i: $(PASS2)/s.p2trshare.i
	get $(PASS2)/s.p2trshare.i
p2trmain.i: $(PASS2)/s.p2trmain.i
	get $(PASS2)/s.p2trmain.i
p2setused.i:  $(PASS2)/s.p2setused.i
	get $(PASS2)/s.p2setused.i
p2jgen.i:  $(PASS2)/s.p2jgen.i
	get $(PASS2)/s.p2jgen.i
p2jvars.i:  $(PASS2)/s.p2jvars.i
	get $(PASS2)/s.p2jvars.i
p2rdata.i: $(PASS2)/s.p2rdata.i
	get $(PASS2)/s.p2rdata.i
p2select.i:  $(PASS2)/s.p2select.i
	get $(PASS2)/s.p2select.i
p2bound.i:  $(PASS2)/s.p2bound.i
	get $(PASS2)/s.p2bound.i
p2jcommon.i: $(PASS2)/s.p2jcommon.i
	get $(PASS2)/s.p2jcommon.i
p2routine.i: $(PASS2)/s.p2routine.i
	get $(PASS2)/s.p2routine.i
p2recurse.i: $(PASS2)/s.p2recurse.i
	get $(PASS2)/s.p2recurse.i
p2call.i:  $(PASS2)/s.p2call.i
	get $(PASS2)/s.p2call.i
p2case.i: $(PASS2)/s.p2case.i
	get $(PASS2)/s.p2case.i
p2wait.i: $(PASS2)/s.p2wait.i
	get $(PASS2)/s.p2wait.i
p2icall.i: $(PASS2)/s.p2icall.i
	get $(PASS2)/s.p2icall.i
p2expr.i:  $(PASS2)/s.p2expr.i
	get $(PASS2)/s.p2expr.i
p2loop.i:  $(PASS2)/s.p2loop.i
	get $(PASS2)/s.p2loop.i
p2stmt.i:  $(PASS2)/s.p2stmt.i
	get $(PASS2)/s.p2stmt.i
p2junit.i:  $(PASS2)/s.p2junit.i
	get $(PASS2)/s.p2junit.i
p2dump.i:  $(PASS2)/s.p2dump.i
	get $(PASS2)/s.p2dump.i
p2alias.i: $(PASS2)/s.p2alias.i
	get $(PASS2)/s.p2alias.i
p2spec.i: $(PASS2)/s.p2spec.i
	get $(PASS2)/s.p2spec.i
p2main.i:  $(PASS2)/s.p2main.i
	get $(PASS2)/s.p2main.i
p2.p: $(PASS2)/s.p2.p
	get $(PASS2)/s.p2.p
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
