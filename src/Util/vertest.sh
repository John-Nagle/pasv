
:	Verifier Test Driver
:
:	Operates on test programs which have been prepared by
:	placing the word "ERROR" on any line which should be printed in an
:	error message.
:
:	Usage is simply	
:
:		testdrive	programs
:
:	but it is nice to start in an empty directory with nothing but copies
:	of the test programs, since the verifier creates working directories
:	and for testing one probably does not want a reverification.
:
:	Any test failure stops the test cycle.
:
:	The verifier must print the line in error for the test driver to work
:	correctly, since the driver matches the lines printed with the source
:	file.
:
:					Version 1.8 of 4/30/82
:
for SOURCE 
do
echo "${SOURCE}:"
rm -f VERTEST1 VERTEST2 VERTEST3 VERTESTDIAGS
:	Create file of expected error messages
numberer < ${SOURCE} | grep "ERROR" | canonize > VERTEST2
:	Find out if any errors expected
if `test -s VERTEST2` 
then
    echo "    Expecting errors."
else
    echo "    Errors not expected."
fi
:	Run the verifier
pasver ${SOURCE} >> VERTESTDIAGS 2>> VERTESTDIAGS
STATUS=${?}
echo "    Verifier exit status ${STATUS}"
:	Create file of actual error messages
grep "^ *[123456789][0123456789]*\. " VERTESTDIAGS | canonize > VERTEST1
if `test -s VERTEST2` 
then
:	Errors expected   difference expected diags with actual diags
    sort -u -n VERTEST1 > VERTEST3
    diff VERTEST2 VERTEST3 > VERTEST4
else
:	Errors not expected  Diags file should be empty
    cp VERTEST1 VERTEST4
if `test ${STATUS} -ne 0`
then
	echo "Bad exit status from verifier"
	exit 1
fi
fi
:
:	Check file of extra/missing diags
:
if `test -s VERTEST4` 
then
    echo "****** TEST ${SOURCE} FAILED ******"
    echo ""
    cat VERTEST4
    exit 1
else
    echo "    Test case ${SOURCE} OK."
    echo ""
fi
done
echo "All test cases produced expected results."
exit 0
