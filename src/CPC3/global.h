(* Pascal-F VCG global declarations file *)

const
  GlobalId = 'global.h 1.36';
  AlphaRadix = 65;
  BitBlockSize = 3;
  EnvPoolSize = 3000;
  KeywordTableSize = 23;
  LabelTableSize = 50;
  MaximumAllowedVariables = 500;
  MessageLength = 60;
  PointerNameTableSize = 187; (* number is weird because of hashing *)
  ShortStringLength = 12;
  StringPoolSize = 50000;
  VariableTableSize = 150;
  VCthreshold = 60;  (* Lines sent to the VC don't get much longer than this *)

type
  (* Simple type definitions *)
  EnvIndex = 1 .. EnvPoolSize;
  FilePosition = integer;
  GetVariableOption = (Create, GetActual, GetShadow, GetEither);
  Instance = 0 .. 3843; (* 3843 = 62*62-1 *)
  Jtags = (RenewN, NewN, SplitN, WhenN, BranchN, JoinN, ProclaimN,
	   RequireN, BreakN);
  NewTag = (Singleton, BeginCluster, MiddleCluster, EndCluster);
  Keyword = (EmptySlot, AssignK, BeginK, BranchK, BreakK, EndK, HangK,
             JoinK, NewK, ReinK, RenewK, ReoutK, ProclaimK, RequireK,
	     SplitK, WhenK);
  LabelInt = 0 .. 9999;
  LabelType = (SplitWhen, BranchJoin);
  Message = packed array [1..MessageLength] of char;
  PointerToJnode = ^Jnode;
  PointerToLabelNode = ^LabelNode;
  PointerToReStack = ^ReStack;
  PointerToStatusNode = ^StatusNode;
  PointerToVariable = ^Variable;
  PointerToVariableList = ^VariableList;
  ShortString = packed array [1..ShortStringLength] of char;
  Status = (Dead, Live);
  StringIndex = 1..StringPoolSize;
  VariableIndex = 0 .. MaximumAllowedVariables;

  (* Record definitions *)

  Jnode = record
    Next, Previous, SimLink: PointerToJnode;
    case Jtag: Jtags of

    NewN, RenewN:
    (ClusterMark: NewTag;  (* Indicates whether this is part of a split new *)
    NewLiveVars, NewDeadVars: PointerToVariableList;
    NewState: StringIndex);  (* assertion from NEW instruction *)

    SplitN:
    (SplitEnv: EnvIndex);

    WhenN:
    (WhenMate: PointerToJnode;
    WhenLiveVars: PointerToVariableList;
    Constraint: StringIndex); (* assertion from WHEN instruction *)

    BranchN:
    (BranchMate: PointerToJnode; (* used to link successors *)
    PathName: StringIndex;       (* string from BRANCH instruction *)
    Visited: boolean);           (* used by traversal *)

    JoinN:
    (JoinEnv: EnvIndex;
    TraverseCount: integer;     (* used by NewBalance *)
    InDegree: integer;          (* number of predecessors *)
    DirectDominator: PointerToJnode;
    Marked: boolean);           (* Used by traversal *)

    ProclaimN:
    (Proclamation: StringIndex;        (* Assertion that is supposed to hold *)
    ProcLiveVars: PointerToVariableList); (* Variables in Proclamation *)

    RequireN:
    (Requirement: StringIndex;          (* Assertion that is supposed to hold *)
    ErrMsg: StringIndex;                (* Message to print if it doesn't *)
    ReqLiveVars: PointerToVariableList);(* List of variables in Requirement *)

    BreakN:
    (PathStart: StringIndex);   (* String from instruction *)
  end;

  LabelNode = record
    LabelVal: integer;               (* The label in question *)
    Class: LabelType;                (* The "type" of the label *)
    One, Many: PointerToJnode;       (* Pointers to Jnodes with this label *)
    HowMany: integer;                (* The length of the Many list *)
    BucketMate: PointerToLabelNode;  (* Hash bucket list *)
  end;

  ReStack = record
    ReNew: PointerToJnode;          (* The NEW node in the current context *)

    ModList: PointerToVariableList; (* A list of variables that have been
				       modified in the current context *)

    Pop: PointerToReStack;          (* A pointer to the record for the
				       surrounding context *)
  end;

  StatusNode = record
    (* Used to maintain a stack of bits using linked blocks *)
    BitCount: 1..BitBlockSize;
    BitBlock: packed array [1..BitBlockSize] of Status;
    Pop: PointerToStatusNode;
  end;

  Variable = record
    Name: StringIndex;
    Kind: (Actual, DefinedShadow);
    Index: VariableIndex;             (* Used to number variables *)
    OnList: boolean;                  (* Used by develop *)
    BucketMate: PointerToVariable;    (* Used by hash lookup *)
    StatusStack: PointerToStatusNode; (* Used during VC generation *)
  end;

  VariableList = record      (* A linked list of variables *)
    Head: PointerToVariable;
    Tail: PointerToVariableList;
  end;

var
  (* The following variable are conceptually constants.  They are
   * initialized by InitGlobal and then are never changed by the
   * program.
   *)
    (* Used to build identifier postifixes *)
    AlphaString: packed array [0..AlphaRadix] of char;

    (* Characters that can appear immediately after a variable *)
    DelimiterSet: set of char;

    (* The "constant" 'END' it is needed because true contants cannot
     * be subscripted
     *)
    EndString: array [1..3] of char;

    (* A hashed table used to recognize keywords *)
    KeywordTable: array[0..KeywordTableSize] of record
      KeyCode: Keyword; Spelling: StringIndex end;

    (* NextString should be initialized to this value before every unit
     * is processed.
     *)
    NextStringBase: StringIndex;

    (* Some string constants *)
    NullString,  QuoteBoolean, QuoteBREAK, QuoteNew,
    QuoteDefined, QuoteTrue: StringIndex;

    (* The initial portion of StringPool is a constant; the part that starts
     * with NextStringBase is reused for each Junit.
     *)
    StringPool: array [StringIndex] of char;

    (* pipes used to talk to theorem prover *)
    ToProver, FromProver: text; 

  (* This variable indicates whether CleanUp is needed before verifying
   * a unit.  Initially it is set to true, and is set to false after
   * every verification.
   *)
    Dirty: boolean;

  (* The following variable is "serially reused."  It is initialized
   * in InitGlobal, changed during the processing of a J-unit, but restored
   * to its original value in CleanUp.
   *)
    VariableTable: array[0..VariableTableSize] of PointerToVariable;

  (* The following variables must be initialized (by InitUnit) before each
   * Junit is processed.
   *)
    (* The current character of the scan *)
    CurrentChar: char;

    (* Class of the current char *)
    CurrentClass: (Ordinary, TabChar, EndLine, EndStatement, EndFile);

    (* The highest used slot in LabelTable *)
    MaxLabelBucket: 0..LabelTableSize;

    (* The index of the next free slot in EnvPool *)
    NextEnv: EnvIndex;

    (* Used to pleasantly print out J-code *)
    NextPointerName: integer;
    PointerNameTable:
      array [0..PointerNameTableSize] of record
	p: PointerToJnode;
	n: integer; end; 

    (* The next free location in StringPool *)
    NextString: StringIndex;

    (* The file of J-units to be verified, sorted by their first lines *)
    p3jcode: text; 

    (* Used to match up corresponding instances of REIN, REOUT, and RENEW. *)
    ReStackTop: PointerToReStack;

    (* The file of good jcode that existed last time around, and the
     * one we create for the next time.
     *)
    history, newhistory: text;
     
    (* The file to which trace output is written, and a flag indicating
     * whether this file is in use.
     *)
    TraceFile: text;
    TraceBeingDone: boolean;

    (* The file to which a dump of the verification conditions in the
     * form fed to the theorem prover is written, and a flag indicating
     * whether this file is to be generated.
     *)
    SaveFile: text;
    SaveBeingDone: boolean;

    (* Line on which current statement begins *)
    StatementLine: integer;

  (* The following variables are modfied in VerifyJunit, but do not
   * require initialization.
   *)
    (* The number of variables (seen so far) *)
    EnvLength: VariableIndex;

    (* Modificaition information about each variable is kept here *)
    EnvPool: array[1..EnvPoolSize] of Instance;

    (* Used to link up references to labels *)
    LabelTable: array [1..LabelTableSize] of PointerToLabelNode;
    VariableWithIndex: array[VariableIndex] of PointerToVariable;

    (* Number of lines in current statement *)
    LineCount: integer;

    (* The node at which the program begins *)
    StartNode: PointerToJnode; 

    (* Length of current line being sent *) 
    VClineLength: integer;

    (* Every node in the J-graph is on one of these four lists,
     * which are linked through the SimLink fields.
     *)
    JoinList,    (* All the JOIN nodes *)
    BranchList,  (* All the BRANCH nodes *)
    RequireList, (* All the REQUIRE nodes *)
    SplitList,   (* All the SPLIT nodes *)
    OtherList:   (* All the other nodes *)
      PointerToJnode;

procedure Abort;
(* Input: none.
 * Output: The standard output file.
 * Effect: Print a message on the standard output and exit.
 *)
  external;

procedure AddLabel(L: LabelInt; J:PointerToJnode);
(* Input: L, J, LabelTable.
 * Output: LabelTable.
 * Assumption: J^.Jtag is one of SplitN, WhenN, BranchN, JoinN.
 * Effect: Find or create the LabelNode for the label L,
 *   and update the label table to reflect the fact that J
 *   references L.
 *)
  external;

function AllocateEnv: EnvIndex;
(* Input: none.
 * Output: through function name.
 * Effect: Allocate and clear a new environment, returning the
 *   index of the new environment.
 *)
  external;

procedure CleanUp;
(* Input and Output:  
 *   VariableTable, RequireList, JoinList, SplitList, OtherList.
 * Effect:
 *   Frees all the heap storage used by the J-graph and VariableTable,
 *   and sets all of the above variables to nil.
 *)
 external;

procedure CopyEnv(Org, Cpy: EnvIndex);
(* Input: Org.
 * Output: Cpy.
 * Effect: copy the enviornment indicated by Ord to the enviornment
 *   indicated by Env.
 *)
  external;

procedure PrintUnitId;
(* Input: through ReadChar.
 * Output: through Write.
 * Effect: Write the message 'Verifying' on the terminal, followed by
 *   a copy of the current statement (where white space is replaced by a
 *   single blank).  Then go to the next statement.
 *)
  external;

procedure DismantleLabelTable;
(* Input: Label Table, J-graph.
 * Output: Label Table, J-graph.
 * Effect: Transfer relevant information from the Label Table
 *   to the J-graph, and dispose of all the LabelNodes in the Label Table.
 *)
  external;

procedure DumpJgraph;
(* debug routine *)
  external;

procedure DumpJnode(J: PointerToJnode);
(* debug routine *)
  external;

procedure DumpJoin;
(* debug routine *)
  external;

procedure DumpStringPool;
(* debug routine *)
  external;

procedure DumpVariables;
(* debug routine *)
  external;

procedure FinishGraph;
(* Input: J-graph
 * Assumptions: The J-graph is acyclic; TraverseCounts are zero
 * Output: J-graph
 * Effects: NewN nodes of the form NEW (X) (equal! (X++) (X)) are inserted
 *   so that the J-graph has the property that at for every J-node
 *   N, and every variable V, all paths from the start node to N have
 *   the same number of NEW Ns.  This balancing is always possible in
 *   acyclic graphs.
 *
 *   The DirectDominator field of each JoinN node is set to its
 *   direct dominator.
 *
 *   The JoinEnv field of each JoinN node N is set to a vector
 *   indicating for each variable V the number of NEW Vs that occurs
 *   along any path from the dominator of N to N.  The extra NewN nodes
 *   inserted ensure that this vector is well-defined.
 *   The SplitEnv field of each Split node is smashed.
 *
 *   The assertions in WhenN, NewN, and RequireN nodes are updated
 *   as follows.  Let k be the number of NEW V nodes along any path from
 *   the start node to the node containing the assertion.  Variables of
 *   the form (V@@), (V++), and (V--) are replaced by variables of the
 *   form (Vnn), where nn is the base AlphaRadix representation of
 *   k, k+1, and k-1 respectively.  This substitution is performed for
 *   every program variable in every assertion.
 *)
  external;

function GetVariable(VarName:StringIndex;
                     Option: GetVariableOption): PointerToVariable;
(* Input: V, Option, the variable table, DelimiterSet.
 * Output: through function name.
 * Effect: Search the variable table a variable with name VarName.
 *   If Option is GetActual or GetShadow, return the variable of
 *   the appropiate kind, aborting if none is found.
 *   If Option is GetEither, use the delimiter to determine which
 *   of the two variables to return:  return the actual if the delimimiter
 *   is '@', and otherwise return the shadow.
 *   If Option = Create, then create two variables, one of each kind.
 *   All fields of created variables, except for the type field, are set.
 *   Both variables go into the same bucket.  The address of the actual
 *   variable is returned; the address of the shadow variable will be in
 *   the BucketMate field of the actual variable.
 *
 * Assumption: If Option = Create, then the variable is delimited in the string
 *   pool by /).  Otherwise, the variable may by delimited by an element of
 *   DelimiterSet; there must be at least one character after the delimiter.
 *)
  external;

procedure InitKeywordTable;
(* Input: None.
 * Output: The keyword table.
 * Effect: Builds the Keyword table.
 *)
  external;

procedure InitPointerName;
(* debug routine *)
  external;

procedure InitReadChar;
(* Input: through ReadChar.
 * Output: none.
 * Effect: Initialize the character-reading sequence
 *)
  external;

procedure InitGlobal;
(* Input: none.
 * Output: variables described above.
 * Effect: variables are initialized.
 *)
 external;

procedure InitUnit;
(* Input: none.
 * Output: variables described above.
 * Effect: variables are initialized.
 *)
 external;

procedure InitVariableTable;
(* Input: none.
 * Output: VariableTable.
 * Effect: VariableTable is initialized to empty.
 *)
 external;

function PathTrace: boolean;
(* Input: The J-graph
 * Output: through writeln and function name.
 * Effects: Verification conditions are generated and sent to the
 *   theorem prover.  Diagnosistics are sent to the standard output
 *   when conditions cannot be proved.  A boolean is returned indicating
 *   whether all the conditions were proved.
 *)
  external;

procedure pipein(var t: text; d: integer);
(* Effects: associates the file t with the file descriptor
 *   whose numeric value is d.
 * Assumption: d must be open for input.
 *)
external;

procedure pipeout(var t: text; d: integer);
(* Effects: associates the file t with the file descriptor
 *   whose numeric value is d.
 * Assumption: d must be open for output.
 *)
external;

function PointerName(ptr: PointerToJnode): integer;
(* debug routine *)
  external;

procedure PostMortem;
(* debug routine *)
  external;

procedure PutStringEnd;
(* Input: none.
 * Output: the string pool;
 * Effect: put the characters '/' and ')' onto the string pool,
 *   which by convention ends the string being built.
 *)
  external;

procedure PutStringPool(C: char);
(* Input: C.
 * Output: the string pool;
 * Effect: Put C in the next available position of the string
 *   pool, incrementing NextString.  If the StringPool overflows,
 *   abort the entire program.
 *
 *   Note: this routine is used for the sake of convenience, not
 *   abstraction.  Statements outside this routine are allowed
 *   to reduce NextString directly to de-allocate characters in the
 *   String Pool.  It is not safe to increment NextString without
 *   performing the checking done by this routine.
 *)
  external;

procedure ReadChar;
(* Input: The standard input file.
 * Output: CurrentChar, CurrentClass.
 * Effect: If there are no characters left to be read, CurrentClass is set to
 *   EndFile.  If the next character to be read is an end-of-line character,
 *   CurrentClass is set to LineEnd.  If the next character to be read is a 
 *   tab, CurrentClass is set to TabChar.  Otherwise the next character in the
 *   file is read and put in CurrentChar and CurrentClass is set to Ordinary.
 *   Whenever CurrentClass is set to something other than Ordinary, CurrentChar
 *   is set to a blank.
 *)
  external;

function ReadExpression: StringIndex;
(* Input: through ReadChar.
 * Output: String Pool, through function name.
 * Effect: If we are already stalled, simply return a default
 *  expression.  Otherwise, read an expression and any blanks
 *  that follow the expression.  An expression is
 *  defined to be an open paren, a string balanced with regard to
 *  parentheses, followed by a close paren.
 *  If we do not find an expression (that is, if CurrentChar is not an
 *  open paren on input, or no matching close paren is found)
 *  return a default expression and stall.
 *  If we do find one, put the expression in the string pool, and return its
 *  index.  At the first non-blank character after the closing paren of the
 *  expression.
 *)
  external;

function ReadIdentifier: StringIndex;
(* Input: through ReadChar.
 * Output: through function name.
 * Effect: If we are already stalled, return DefaultIdentifier.
 *  Otherwise, read an identifier and any blanks following it.
 *  If a variable is found, put it in the string pool and return
 *  its index.  If no variable is found, stall the scan and
 *  return DefaultIdentifier.
 *)
  external;

function ReadKeyword: Keyword;
(* Input: through ReadChar.
 * Output: through function name.
 * Effect: If we are already stalled, return EmptySlot.
 *   Otherwise, read a keyword and any blanks following it.
 *   If the string read is in the keyword table, return the
 *   token associated with it.  Otherwise, stall the scan.
 *)
  external;

function ReadLabel: LabelInt;
(* Input: through ReadChar.
 * Output: through function name.
 * Effect: If we are already stalled, return 0.
 *  Otherwise, read a label and any blanks following the label.
 *  A label is defined to be a string of one to MaxDigits
 *  digits that does not begin with a zero.
 *  If a label is found, the integer value of the label is returned.
 *  If a label is not found, stall the scan and return 0.
 *  CurrentChar is left at the first nonblank following the label.
 *)
  external;

procedure ReadStatementEnd;
(* Input: LineCount, StatementLine.
 * Output: Stalled, ErrFile.
 * Effect: Prepare to process the next statement.  If we are not at the
 *   end of the current statement, read the rest of it an print a
 *   diagnostic.  Maintain the invariant that StatementLine is the line
 *   number of the current statement and LineCount is the number of lines
 *   in the current statement.  Unstall the scan if necessary.
 *)
  external;

function ReadString: StringIndex;
(* Input: through ReadChar.
 * Output: String Pool, through function name.
 * Effect: If we are already stalled, simply return a default
 *  string.  Otherwise, read a string and any trailing blanks after
 *  it.  A string is defined to be (/ followed by a sequence of zero
 *  or more string elements followed by /).  A string element is defined
 *  to be either a character (other than an end-of-line character) or
 *  a string break.  A string break is defined to be an end-of-line
 *  character, followed by some white space (possibly including comments)
 *  followed by a slash.  A string is defined to be an open paren
 *  followed by a sequence of string elements, followed by a close
 *  paren.  The sequence of string elements must contain at least two
 *  characters, and the first and last characters in the sequence must
 *  be slashes.  The value of the string is obtained by deleting the
 *  string breaks and the first and last slash in the sequence.
 *  Note that one cannot have a string whose value contains and end-of-line
 *  character or the substring \).  It is possible to have a string whose
 *  value contians the comment symbol --.
 *
 *  If a string is found, its value is returned and CurrentChar
 *  is left at the first nonblank character after the string.
 *  the delimeters are returned.  If no string is found, the
 *  scan is stalled, and a default string is returned.
 *)
  external;

function ReadType: StringIndex;
(* Input: through ReadChar.
 * Output: String Pool, through function name.
 * Effect: If we are already stalled, simply return a default
 *  expression.  Otherwise, read a type expression and any blanks
 *  that follow the expression.  A type expression is
 *  defined to be an open paren, a string balanced with regard to
 *  parentheses, followed by a close paren.
 *  If we do not find a type expression (that is, if CurrentChar is not an
 *  open paren on input, or no matching close paren is found)
 *  return a default expression and stall.
 *  If we do find one, put the type expression in the string pool,
 *  and return its index.  At the first non-blank character after the
 *  closing paren of the expression.
 *)
  external;

procedure ReadUnit;
(* Input: through readln
 * Output: the J-graph, EnvLength.
 * Effect: read a J-unit from the input and build the corresponding
 *  level-0 J-graph.  Set EnvLength to the number of variables
 *  in the J-unit.
 *)
  external;

function ReadVariableList: PointerToVariableList;
(* Input: through ReadChar.
 * Output: through function name.
 * Effect: If we are already stalled, return nil.
 *  Otherwise, read a variable list and any blanks following it.
 *  followed by a run of blanks.  If a variable list is found,
 *  build a linked structure corresponding to the list.
 *  If a syntax error is found,
 *  return nil and stall the scan.  Note: the order in the variables
 *  in the returned list is reversed from the order in the input.
 *)
  external;

(* The following two declarations are used to bring in library routines to
 * do direct access I/O on text files.
 *)

procedure OurSeek(var f: text; n: FilePosition);
(* This routine moves f^ to the place returned by OurTell *)
  external;

function OurTell(var f: text): FilePosition;
(* This routine returns the position of f^ in f. *)
  external;

procedure SkipToNonBlank(BeginFudge: boolean);
(* Input: through ReadChar.
 * Output: CurrentChar.
 * Assumption: CurrentChar is either not part of white space,
 *   or it is at the beginning of a run of white space.  (It should
 *   not be in the middle of some white space.)  BeginFudge should
 *   indicate whether CurrentChar is the very first character of
 *   the file.  Before doing anything else, SkipToNonBlank should
 *   be called to read any initial comments that might be in
 *   the file.
 * Effect: Advance CurrentChar until it is not at the beginning
 *   of some white space.  (If CurrentChar is not at the beginning of
 *   white space, do nothing).  White space is defined as blanks, tabs,
 *   end-of-lines, and comments.  Comments begin with -- and extend
 *   to the end of the line.
 *
 *   Every time an end-of-line is skipped over, increment LineCount
 *   by one.
 *
 *   For the sake of flexibility, we would like to make as few
 *   assumptions about the J-code language as possible.  To avoid
 *   two-character lookahead, however, we make the assumption that
 *   statements cannot begin with a -.  The routine flags and ignores
 *   non-comment lines that begin with -.
 *)
  external;

procedure StringCopy(A: StringIndex);
(* Input: A, the string pool.
 * Output: the string pool.
 * Assumption: The string A is properly delimited by a /).
 * Effect: A copy of A, including the /), is placed at the
 *   end of the string pool.
 *)
  external;

procedure StringAppend(A: StringIndex);
(* Input: A, the string pool.
 * Output: the string pool.
 * Assumption: The string A is properly delimited by a /).
 * Effect: A copy of A, up to but not including the /),
 *   is placed at the end of the string pool.
 *)
  external;

function StringCreate(S: ShortString): StringIndex;
(* Input: S.
 * Output: the string pool, through function name.
 * Assumption: S contains the substring /).
 * Effect: put the intial substring of S up to and including the
 *   /) into the string pool, returning the index of the inserted
 *   string.
 *)
  external;

procedure ShortAppend(S: ShortString);
(* Input: S.
 * Output: the string pool.
 * Assumption: S contains the substring /).
 * Effect: put the intial substring of S up to but not including the
 *   /) into the string pool.
 *)
  external;

function StringEqual(A, B: StringIndex): boolean;
(* Input: A, B, the string pool.
 * Output: through function name.
 * Assumption: the string '/)' appears on or after postions A and B
 *   in the string pool.
 * Effect: Compare the strings beginning at A and B in the string pool,
 *   returning true if they are the same, and false if they are different.
 *)
  external;

procedure SyntaxError;
(* Input: StatementLine.
 * Output: ErrFile.
 * Effect: Print a diagnostic and abort.
 *)
  external;

(* The following routines are used to communicate with the theorem prover.
 * Before any of the other routines are called, InitTP must be called.
 * 
 * To attempt a proof of a verification condition, first call VCbegin.  Then
 * call a sequence of VCstringOut, VCshortOut, and VCcharOut to send
 * pieced of the verification condtions.  After the last piece has
 * been sent, call the function VCproved, which will return a value
 * indicating whether a proof was found.
 *
 * Every variable appearing in a VC must be declared using TPdeclare
 * By enclosing each unit between calls to TPunitBegin and TPunitEnd,
 * we ensure that variables declared in one unit will be forgotten
 * in the next.
 *)

procedure InitTP;
(* Input: through argv.
 * Output: none.
 * Assumptions: We are running under Unix.  argc = 3.  argv(2) is a one-digit
 *   file descriptor that can be used to send characters to the theorem
 *   prover, and argv(1) is a one-digit file descriptor that can be used
 *   to get characters back.
 * Effect: Set up the linkage to the theorem prover. 
 *)
  external;

procedure TPunitBegin;
(* Input: none.
 * Output: none.
 * Effect: Inform the theorem prover that we are beginning a J-unit.
 *)
  external;

procedure TPunitEnd;
(* Input: none.
 * Output: none.
 * Effect: Inform the theorem prover that we are ending a J-unit.
 *)
  external;

procedure TPdeclare(VarName, VarType: StringIndex);
(* Input: VarName, VarType.
 * Output: none.
 * Effect: Inform the theorem prover that the variable named VarName
 *   has the type VarType.
 *)
  external;

procedure VCbegin;
(* Input: none.
 * Output: VC area
 * Effect: Inform the VC area that it is about to receive a VC.
 *)
  external;

function  VCproved: boolean;
(* Input: none.
 * Output: through function name.
 * Effect: Complete the transmission of a VC.  Return a boolean
 *    indicating whether the theorem prover was successful.
 *)
  external;

procedure VCstringOut(S: StringIndex);
(* Input: S, (and the string pool).
 * Output: none.
 * Effect: Send the string indicated by S to the prover.
 *)
  external;

procedure VCshortOut(S: ShortString);
(* Input: S, (and the string pool).
 * Output: VC area.
 * Assumption: the string contains an instance of /).
 * Effect: Send the initial substring of S, up to but not including
 *    the /) to the prover.
 *)
  external;

procedure VerifyChangedUnits;
(* Input: p3jcode, history.
 * Output: newhistory.
 * Effect:  Attempt to verify the program represented by p3jcode.
 * Read and compare the files p3jcode and history.  The history 
 * file is used to screen the j-units in p3jcode; any j-unit that matches
 * one in history is assumed to be correct.  For every j-unit in p3jcode
 * that has no match in history, verification conditions are generated
 * and sent to the simplifier.  The j-unit is considered to be correct
 * only if all the conjectures simplify to "true."  All the correct
 * units in p3jcode are copied into newhistory (preserving the order).
 *)
  external;

procedure VCcharOut(C: char);
(* Input: C.
 * Output: VC area.
 * Effect: Send the character C to the prover.
 *)
  external;

procedure WriteEnv(E: EnvIndex);
(* debug routine *)
  external;

procedure WritePointer(P: PointerToJnode);
(* debug routine *)
  external;

procedure WriteString(var F: text; X: StringIndex);
(* Input: X, the string pool;
 * Output: through F.
 * Assumption: X points to a properly terminated string.
 * Effect: The string pointed to by X is output (the terminating
 *   slash-paren is not output) to file F.
 *)
  external;

procedure WriteVarList(var F: text; V: PointerToVariableList; C: char);
(* Input: V, C.
 * Output: through F .
 * Effect: The list V is output as a lisp list.  The characters '@' or
 *   '+' are appended to the variable name to indicate whether the variable
 *   is actual or a shadow.  After that, if C is nonblank, two copies of
 *   C is appended as well.
 *)
  external;

procedure WriteVariable(V: PointerToVariable);
(* debug routine *)
  external;

(* routines to write the version ids of source files *)
procedure WriteChangedId;  external;
procedure WriteCharinId;   external;
procedure WriteEnvId;      external;
procedure WriteErrorId;    external;
procedure WriteFinishId;   external;
procedure WriteInitId;     external;
procedure WriteMainId;     external;
procedure WriteOutputId;   external;
procedure WriteParseId;    external;
procedure WritePathId;     external;
procedure WriteStringId;   external;
procedure WriteTableId;    external;
procedure WriteUnixioId;   external;
