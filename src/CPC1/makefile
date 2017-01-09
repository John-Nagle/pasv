#		Makefile for verifier pass one
#
#					Version 1.6 of 2/6/86
#                   Revised January 2017 for Free Pascal
#
SOURCE=..
DEST=../bin
PASS1=$(SOURCE)/cpc1/src
POPTIONS=-gl -Co -Cr
#
#	make 		with no arguments makes pasver1.
#	make install	makes and installs pasver1
#	make all	makes pasver1 and testtools
#
default: pasver1
#
#	install pasver1 in DEST
#
install: pasver1
	-rm -f $(DEST)/pasver1.old
	-mv $(DEST)/pasver1 $(DEST)/pasver1.old
	ln pasver1 $(DEST)
#
#	make everything including test tools
#
all:	pasver1 pascmp analyzer1 varprint intprint
#
#	Verifier Pass One
#
pasver1: P1X-VER.p P1X-PAS00.i P1X-VER00.i  P1X-UNIX00.i P1X-VER00.h \
    P1X-PAS01.i P1X-UNIX01.i P1X-VER01.h P1X-PAS02.i P1X-UNIX02.i \
    P1X-VER02.i P1X-PAS03.i P1X-UNIX03.i P1X-PAS04.i P1X-PAS05.i \
    P1X-PAS06.i P1X-VER07.i P1X-PAS07.i P1X-PAS08.i P1X-PAS09.i \
    P1X-PAS10.i P1X-PAS11.i P1X-PAS12.i P1X-PAS13.i P1X-PAS14.i
	fpc $(POPTIONS) P1X-VER.p | tee errs
	rm -f pasver1
	mv P1X-VER pasver1
	chmod 555 pasver1
#
#	Compiler Pass One
#
pascmp: P1X-CMP.p \
    P1X-PAS00.i P1X-CMP00.i P1X-UNIX00.i P1X-PAS01.i P1X-UNIX01.i \
    P1X-PAS02.i P1X-UNIX02.i P1X-PAS03.i P1X-CMP03.i P1X-UNIX03.i \
    P1X-PAS04.i P1X-PAS05.i P1X-PAS06.i P1X-PAS07.i P1X-PAS08.i \
    P1X-PAS09.i P1X-PAS10.i P1X-PAS11.i P1X-PAS12.i P1X-PAS13.i P1X-PAS14.i
	fpc $(POPTIONS)  P1X-CMP.p | tee errs
	rm -f pascmp
	mv P1X-CMP pascmp
	chmod 555 pascmp
#
#	Verifier Pass One Output Analyzer
#
analyzer1: analyzer1.p P1X-VER00.h P1X-VER01.h
	rm -f analyzer1
	fpc $(POPTIONS) analyzer1.p | tee errs
	chmod 555 analyzer1
#
#	Compiler Icode Printer
#
intprint: intprint.p
	rm -f intprint
	fpc $(POPTIONS) intprint.p | tee errs
	chmod 555 intprint
#
#	Verifier Dictionary Printer
#
varprint: varprint.p P1X-VER00.h P1X-VER01.h
	rm -f varprint
	fpc $(POPTIONS) varprint.p | tee errs
	chmod 555 varprint

#
#	Everything Maker
#
all: pasver pascmp analyzer1 intprint varprint ERMSG-TXT
