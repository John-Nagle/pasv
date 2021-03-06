#!/bin/sh
#@(#)pasver.sh	1.10
#
#	Pascal-F Verifier
#
#	Control Program (pasver)
#
#	Version 1.10 of 12/10/85
#
# check arguments
#
case $# in
0) echo "Call: " `basename $0` ' [<flags>] <name>'
   exit 1;;
*) ;;
esac
#
#	Scan for flags
#
#		-d1		Pass 1 debug on
#		-d2		Pass 2 debug on
#		-d3		Pass 3 (theorem prover and VCG) debug on
#		-dvcg		VCG logging only on
#		-d		All debug on
#		-f<procname>	Attempt to prove named routine only (UNIMPL)
#
DEBUG1=
DEBUG2=
DEBUG3=
DEBUGVCG=
DEBUGTHM=
SINGLE=
VCDUMP=p3-vcdump
VCLOG=p3-vcs
for ARG do
    case $ARG in
    -d1)	DEBUG1=-d;;
    -d2)	DEBUG2=-d;;
    -dvcg)	DEBUGVCG=$VCLOG;;
    -d3)	DEBUGTHM=$VCDUMP; DEBUGVCG=$VCLOG;;
    -d)		DEBUG1=-d
		DEBUG2=-d
		DEBUGVCG=$VCLOG
		DEBUGTHM=$VCDUMP;;
    -f*)	SINGLE=$ARG;;
    -*)		echo "Illegal flag: $ARG"; exit 1;;
    *)		FILE=$ARG;;
    esac
done

case $FILE in
*.pf) ;;
*)    echo "File name ${FILE} does not end in .pf"
      exit 1 ;;
esac

if test ! -r $FILE 
then echo 'cannot read' $FILE
     exit 1
fi

# if $FILE is of form name.pf, create working directory of
# the form name_d (if necessary), and clean it out (if necessary).
#
workdir=`basename $FILE .pf`_d
mkdir $workdir 2> /dev/null
cd $workdir
rm -f pasf.lp pas-* p2a-* p3-*

# Execute pass 1 in calling directory
#
cd ..
echo "Pass 1:"
if pasver1 $DEBUG1 $FILE
then : nothing
else echo 'Pass 1 error abort.'
     rm pasf-*
     exit 1
fi

# Move files created by pass 1 to working directory
#
cd $workdir
mv ../pasf-* .

# Execute pass 2 in the working directory
#
echo "Pass 2:"
if pasver2 $DEBUG2
then : nothing
else echo 'Pass 2 error abort.'
     exit 1
fi
# Check output of pass 2
#
if test -r p2jcode
then : nothing
else echo 'Pass 2 detected errors.'
     exit 1
fi
if jcheck <p2jcode
then : nothing
else echo "INTERNAL ERROR: bad jcode generated."
     exit 1
fi

# Create an empty history file if there is none
#
if test ! -f history 
then echo "Creating empty history file." 2>history
fi
 
# Execute pass 3 components
#
echo "Pass 3:"
pasver3a
pasver3b $DEBUGVCG $DEBUGTHM | tee p3-diags

# Save the status returned by pasver3b --
# (This only works because tee returns a zero.)
rc=$?

mv newhistory history
exit $rc
