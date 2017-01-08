
#
#	Makefile for verifier test utilities
#
#					Version 1.2 of 3/3/86
#
SOURCE=..
TEST=$(SOURCE)/test
#
regress: regress.sh
	rm -f regress
	cp regress.sh regress
	chmod 555 regress
vertestdrive: testdrive.sh
	rm -f vertestdrive
	cp testdrive.sh vertestdrive
	chmod 555 vertestdrive
#
#	SCCS dependencies
#
regress.sh:	$(TEST)/src/s.regress.sh ; get $(TEST)/src/s.regress.sh
testdrive.sh:	$(TEST)/src/s.testdrive.sh ; get $(TEST)/src/s.testdrive.sh
