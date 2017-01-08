
#
#	putrules  --  shell procedure to install new rules file for
#		      given program being verified
#
#	Call is		newrules BoyerMooreLib VerifierWorkingDir
#
#	If a definition has been altered or deleted in the rule file,
#	then we have to assume that verifications against the previous
#	database are no longer valid and we delete the history file.
#
LIB=$1
DIR=$2
#	Check file status
if test ! -r $LIB
then echo "Cannot read $LIB" ; exit 1 
fi
if test ! -d $DIR
then echo "$DIR not a directory." exit 1
fi
#	Clean up directory
rm -f $DIR/rulediffs $DIR/newdatabase
#	Create new database
echo "Processing database $LIB"
getrules < $LIB  | sort > $DIR/newdatabase
case $? in
    0) ;;
    *) echo "Trouble processing database $LIB"; exit 1  ;;
esac
#	Check for old database
if test -r $DIR/ruledatabase
then 	# check for change to existing DEFN, which invalidates history
    diff $DIR/ruledatabase $DIR/newdatabase > $DIR/rulediffs 
    grep -s '^<.*DEFN' $DIR/rulediffs
    case $? in
    1) echo "No definitions altered or deleted." ;;
    *) echo "Definition altered or deleted - will reverify entire program."
       rm -f $DIR/history ;;
     esac
fi
echo "Installing new database in $DIR"
mv $DIR/newdatabase $DIR/ruledatabase
