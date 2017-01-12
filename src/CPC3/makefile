
#
#	Makefile for pass 3 of verifier
#
#				Version 1.9 of 2/6/86
#               Revised January 2017 for Free Pascal
#
#	Programs of pass 3 are
#
#	    pass3a -- the jsort program which sorts jcode
#	    pass3b -- the control program for passes 3 and 4
#	    pass3c -- the verification condition generator
#
SOURCE = ..
DEST=../bin
PFLAGS = -I/usr/include/pascal -c -g -C
CFLAGS = -I/usr/src/lib/libpc -c -g
PASS3 = $(SOURCE)/CPC3/src
POPTIONS=-gl -Co -Cr
#
#	make			with no arguments makes pasver3c and pasver3a
#	make install		makes and installs pasver3c and pasver3a
#	make all		is the same as make with no arguments
#
pofiles = charin.o env.o error.o finish.o init.o output.o parse.o \
          path.o string.o table.o changed.o 
sfiles = global.h charin.p env.p error.p finish.p init.p output.p parse.p \
         path.p string.p table.p main.p changed.p unixio.c

all: pasver3c pasver3a
install: pasver3c pasver3a
	-rm -f $(DEST)/pasver3a.old 
	-mv $(DEST)/pasver3a $(DEST)/pasver3a.old
	ln pasver3a $(DEST)
	-rm -f $(DEST)/pasver3c.old 
	-mv $(DEST)/pasver3c $(DEST)/pasver3c.old
	ln pasver3c $(DEST)

pasver3c: main.o unixio.o $(pofiles)
	pc main.o unixio.o $(pofiles)
	rm -f pasver3c
	mv a.out pasver3c
	chmod 555 pasver3c

source: $(sfiles)

pasver3a: jsort.p jsort.h
	rm -f pasver3a
	fpc $(POPTIONS) jsort.p | tee errs
	mv jsort pasver3a
	chmod 555 pasver3a

unixio.o:      pascaliodefs.h
$(pofiles):    global.h
main.o:        global.h main.p;  $(PC) $(PFLAGS) -w main.p


