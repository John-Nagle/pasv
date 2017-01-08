
#
#	Makefile for pass 3 of verifier
#
#				Version 1.9 of 2/6/86
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
PASS3 = $(SOURCE)/cpc3/src
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

pasver3a: jsort.o
	pc jsort.o
	rm -f pasver3a
	mv a.out pasver3a
	chmod 555 pasver3a

jsort.o:       jsort.p jsort.h
unixio.o:      pascaliodefs.h
$(pofiles):    global.h
main.o:        global.h main.p;  $(PC) $(PFLAGS) -w main.p

# SCCS dependencies
global.h: $(PASS3)/s.global.h; get $(PASS3)/s.global.h
charin.p: $(PASS3)/s.charin.p; get $(PASS3)/s.charin.p
env.p: $(PASS3)/s.env.p; get $(PASS3)/s.env.p
error.p: $(PASS3)/s.error.p; get $(PASS3)/s.error.p
finish.p: $(PASS3)/s.finish.p; get $(PASS3)/s.finish.p
init.p: $(PASS3)/s.init.p; get $(PASS3)/s.init.p
main.p: $(PASS3)/s.main.p; get $(PASS3)/s.main.p
output.p: $(PASS3)/s.output.p; get $(PASS3)/s.output.p
parse.p: $(PASS3)/s.parse.p; get $(PASS3)/s.parse.p
path.p: $(PASS3)/s.path.p; get $(PASS3)/s.path.p
string.p: $(PASS3)/s.string.p; get $(PASS3)/s.string.p
table.p: $(PASS3)/s.table.p; get $(PASS3)/s.table.p
changed.p: $(PASS3)/s.changed.p; get $(PASS3)/s.changed.p
unixio.c: $(PASS3)/s.unixio.c; get $(PASS3)/s.unixio.c
jsort.p: $(PASS3)/s.jsort.p; get $(PASS3)/s.jsort.p
jsort.h: $(PASS3)/s.jsort.h; get $(PASS3)/s.jsort.h
pascaliodefs.h: $(PASS3)/s.pascaliodefs.h; get $(PASS3)/s.pascaliodefs.h

