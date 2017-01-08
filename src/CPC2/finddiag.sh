#! /bin/sh
#
#	Diagnostic Extractor
#
#	Finds calls to "badnode" and "internalerror" in pass 2 of
#	the verifier and constructs a file of the numbers and the
#	associated comments.
#					Version 1.5 of 1/14/83
#
rm -f FINDBAD?
cat $* | grep "badnode *(" > FINDBAD1
cat $* | grep "internalerror *(" >> FINDBAD1
#	Extract calls to internalerror and badnode with comments
cat FINDBAD1 | sed \
	-e "s/.*internalerror *(//" \
	-e "s/.*badnode *(.*, *\([0123456789][0123456789]* *\))/\1)/" \
	-e "s/)[;	 ]*{ */	/" \
	-e "s/[} 	]*$//" > FINDBAD2
#	Sort into numeric order
grep "^[0123456789]" FINDBAD2 | sort -n | uniq > FINDBAD3
#	Remove duplicates by number for misuse of number check
sort -n -u FINDBAD3 > FINDBAD4
#	Locate duplicate numbers
if cmp FINDBAD3 FINDBAD4 
then :
else echo "finddiag: SAME ERROR NUMBER USED TWICE" >&2 
     diff FINDBAD3 FINDBAD4
fi
cat FINDBAD3

