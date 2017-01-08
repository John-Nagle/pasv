#include "global.h"

procedure WriteOutputId;
begin
  writeln('output.p 1.28') end;

procedure InitPointerName;
var j: 0..PointerNameTableSize;
begin
  for j := 0 to PointerNameTableSize do 
    PointerNameTable[j].p := nil;
  NextPointerName := 1 end;

function PointerName;
const
  RetryLimit = 12;
var
  ConvertBuffer: record case integer of
    1: (pval: PointerToJnode);
    2: (ival: integer);
    end;

  HashVal: 0..PointerNameTableSize;
  RetryCount: 0..RetryLimit;
  Done: boolean;
begin
  if ptr = nil then
    PointerName := 0
  else begin
    ConvertBuffer.pval := ptr;
    HashVal := ConvertBuffer.ival mod PointerNameTableSize;
    RetryCount := 0;
    repeat
      with PointerNameTable[HashVal] do begin
      if p = ptr then begin
        PointerName := n;
        Done := true end
      else if PointerNameTable[HashVal].p = nil then begin
        p := ptr;
        n := NextPointerName;
        PointerName := NextPointerName;
        NextPointerName := NextPointerName + 1;
        Done := true end
      else begin
        if RetryCount = RetryLimit then begin
          writeln('pointer table overflow');
	  Abort end;
        RetryCount := RetryCount + 1;
        if HashVal = PointerNameTableSize then
          HashVal := 0
        else
          HashVal := HashVal + 1;
        Done := false end end;
    until Done end end;

procedure WritePointer;
begin
  write(PointerName(P):1) end;

procedure WriteEnv;
var V: VariableIndex;
begin
  write('[', E:1, '] ');
  for V := 0 to EnvLength-1 do write(EnvPool[E+V]:3) end;

procedure WriteString;
var Y, Z: StringIndex;
begin
  (* Set Y to the index of the / that delimits the end of the string *)
  Y := X;
  while not ((StringPool[Y] = '/') and (StringPool[Y+1] = ')')) do
    Y := Y + 1;

  (* Now write out the string *)
  for Z := X to Y-1 do write(F, StringPool[Z]) end;

procedure WriteVarList;
begin
  write(F, '(');
  while V <> nil do begin
    WriteString(F, V^.Head^.Name);
    case V^.Head^.Kind of
    Actual: write(F, '@');
    DefinedShadow: write(F, '+')
    end;

    if C <> ' ' then begin
      write(F, C);
      write(F, C) end;

    V := V^.Tail;
    if V <> nil then
      write(F, ' ')
    end;
  write(F, ')') end;

procedure WriteLine;
begin
  writeln('===========================================================') end;

procedure DumpList(Jlist: PointerToJnode);
begin
  if Jlist = nil then
    writeln('<empty>')
  else while Jlist <> nil do begin
    DumpJnode(Jlist);
    Jlist := Jlist^.SimLink end end;

procedure DumpJnode;
begin
  WritePointer(J); write(': ');
  with J^ do begin
    case Jtag of

    RenewN, NewN: begin
      if Jtag = NewN then
	write('NewN node ')
      else
	write('RenewN node ');

      case ClusterMark of
      Singleton:     write('(single)');
      BeginCluster:  write('(begin)');
      MiddleCluster: write('(middle)');
      EndCluster:    write('(end)');
      end;
      writeln;

      write('NewLiveVars: '); WriteVarList(output, NewLiveVars, ' '); writeln;
      write('NewDeadVars: '); WriteVarList(output, NewDeadVars, ' '); writeln;
      write('NewState: '); WriteString(output, NewState); writeln end;
    SplitN: begin
      writeln('SplitN node');
      write('SplitEnv: '); WriteEnv(SplitEnv); writeln end;
    WhenN: begin
      writeln('WhenN node');
      write('WhenMate: '); WritePointer(WhenMate); writeln;
      write('WhenLiveVars: '); WriteVarList(output, WhenLiveVars, ' '); writeln;
      write('Constraint: '); WriteString(output, Constraint); writeln end;
    BranchN: begin
      writeln('BranchN node');
      write('BranchMate: '); WritePointer(BranchMate); writeln;
      write('PathName: '); WriteString(output, PathName); writeln;
      write('Visited: '); write(Visited); writeln end;
    JoinN: begin
      writeln('JoinN node');
      write('JoinEnv :'); WriteEnv(JoinEnv); writeln;
      write('TraverseCount: '); write(TraverseCount:1); writeln;
      write('InDegree: '); write(InDegree:1); writeln;
      write('DirectDominator: '); WritePointer(DirectDominator); writeln;
      write('Marked: '); write(Marked:1); writeln end;
    ProclaimN: begin
      writeln('ProclaimN node');
      write('Proclamation: '); WriteString(output, Proclamation); writeln;
      write('ProcLiveVars: '); WriteVarList(output, ProcLiveVars, ' '); writeln;
      end;
    RequireN: begin
      writeln('RequireN node');
      write('Requirement: '); WriteString(output, Requirement); writeln;
      write('ReqLiveVars: '); WriteVarList(output, ReqLiveVars, ' '); writeln;
      write('ErrMsg: '); WriteString(output, ErrMsg); writeln end;
    BreakN: begin
      writeln('BreakN node') end
    end;
    write('Next: '); WritePointer(Next); writeln;
    write('Previous: '); WritePointer(Previous); writeln;
    write('SimLink: '); WritePointer(SimLink); writeln end;
    WriteLine end;

procedure DumpJgraph;
begin
  writeln('Dump J-graph');
  writeln('Start Node is: ', PointerName(StartNode):1); 
  writeln('Dump SplitList');
  WriteLine;
  DumpList(SplitList);
  writeln('Dump Require');
  WriteLine;
  DumpList(RequireList);
  writeln('Dump OtherList');
  WriteLine;
  DumpList(OtherList);
  writeln('Dump JoinList');
  WriteLine;
  DumpList(JoinList);
  writeln('Dump BranchList');
  WriteLine;
  DumpList(BranchList);
  writeln('End of J-graph dump') end;

procedure DumpJoin;
begin
  writeln('Dump JoinList');
  WriteLine;
  DumpList(JoinList);
  writeln('End of JoinList dump') end;

procedure DumpStringPool;
var N: 0..50;  (* Number of chars on current line *)
    J: StringIndex;
begin
  N := 0;
  if NextString-1 >= 1 then		(* avoid compiler bug *)
  for J := 1 to NextString-1 do begin
    write(StringPool[J]);
    N := N + 1;
    if N = 50 then begin
      N := 0;
      writeln end end;
  if N <> 0 then
    writeln end;

procedure DumpVariables;
var j: VariableIndex;
begin
  writeln('Dump of Symbol Table');
  for j := 0 to EnvLength-1 do begin
    WriteVariable(VariableWithIndex[j]);
    writeln end;
  writeln('End dump') end;

procedure WriteVariable;
var
  S: PointerToStatusNode;
  J: 1..BitBlockSize;
begin
  with V^ do begin
    WriteString(output, Name);
    case Kind of
    Actual: write('v');
    DefinedShadow: write('d')
    end;
    write(' [');
    write(Index:1);
    write('] ');
    S := StatusStack;
    write(' Status(');
    while S <> nil do with S^ do begin
      for J := BitCount downto 1 do begin
	case BitBlock[J] of
	Dead: write('Dead ');
	Live: write('Live ')
	end end;
      S := Pop end;
    write(')');
    if OnList then write(' On List') end end;
  
procedure PostMortem;
begin
  DumpStringPool end;
(*
	ASCII to integer conversion (for call param)
*)
function AtoI(s: array[1..2] of char): integer;
var total: integer;
    pos: 1..2;
begin
    total := 0;
    for pos:=1 to 2 do begin
	if (s[pos] >= '0') and (s[pos] <= '9') then	(* if digit *)
	begin
		total := total * 10 + (ord(s[pos]) - ord('0'));
		end;
	end;
    AtoI := total;			(* return total *)
end {AtoI};

procedure InitTP;
var arg: array[1..2] of char;		(* arg from call line *)
begin
  (* Highly Unix-dependent code that sets up pipelines *)
  (* arg 1 is input (to prover) pipe file descriptor *)
  (* arg 2 is output (from prover) pipe file descriptor *)
  (* arg 3 is trace file descriptor *)
  (* arg 4 is VC dump file descriptor *)
  if not (argc in [3,4,5]) then begin
    writeln('Arg count');
    Abort end;
  argv(1, arg);
  pipein(FromProver, AtoI(arg));	
  argv(2, arg);
  pipeout(ToProver, AtoI(arg));
  TraceBeingDone := argc >= 4;
  SaveBeingDone := argc >= 5;
  if TraceBeingDone then begin
    argv(3, arg);
    pipeout(TraceFile, AtoI(arg)); end;		
  if SaveBeingDone then begin
    argv(4, arg);
    pipeout(SaveFile, AtoI(arg)); end; end;

(*
	Actual VC writing, and saving if turned on
*)
procedure VCwriteln;
begin
  if SaveBeingDone then writeln(SaveFile);
  writeln(ToProver);			(* prover write line *)
end;

procedure VCflush;
begin
  if SaveBeingDone then flush(SaveFile);
  flush(ToProver);			(* prover flush *)
end;

procedure VCcharWrite(ch: char);	(* all chars go through here *)
begin
  if SaveBeingDone then write(SaveFile,ch);
  write(ToProver,ch)			(* prover write char *)
end;

procedure TPunitBegin;
begin
  VCcharOut('(');
  VCshortOut('begin-decl/)');
  VCcharOut(')');
  VCwriteln;
  VCflush;
  readln(FromProver) end;

procedure TPunitEnd;
begin
  VCshortOut('(end-decl)/)'); VCwriteln;
  VCflush;
  readln(FromProver) end;

procedure TPdeclare;
begin
  VCshortOut('(vardecl ''/)');
  VClineLength := 10;
  VCstringOut(VarName);
  VCshortOut(' ''/)');
  VCstringOut(VarType);
  VCcharOut(')'); VCwriteln;
  VCflush;
  readln(FromProver) end;

procedure VCbegin;
begin
  VCshortOut('(prove ''/)');
  VClineLength := 8 end;

function VCproved;
var TorN: char;
begin 
  (* matches the paren sent in VCbegin *)
  VCcharOut(')');
  VCwriteln;
  VCflush;
  readln(FromProver, TorN);
  if TorN in ['t', 'n'] then
    VCproved := TorN = 't'
  else begin
    writeln('Prover out of sync');
    Abort end end;

procedure VCcharOut;
begin
  if (C = ' ') and (VClineLength = 0) then begin
    (* Throw away leading blanks *) end

  else if VClineLength < VCthreshold then begin
    VCcharWrite(C);
    VClineLength := VClineLength + 1 end

  else begin
    (* Make a half-hearted attempt to break the VCs over multiple lines.
     * The only reason to do this at all is a fear of stumbling over some
     * system limit somewhere, and to make the use of editors convenient.
     * We make no promises about how long lines will get.
     *
     * However, we are careful to insert line breaks in places that do not
     * change the meaning.  We will:
     *   replace a space with a line break
     *   put a line break before a (
     *   put a line break after a )
     *)
    if C = ' ' then begin
      VCwriteln;
      VClineLength := 0 end

    else if C = '(' then begin
      VCwriteln;
      VCcharWrite(C);
      VClineLength := 1 end

    else if C = ')' then begin
      VCcharWrite(C);
      VCwriteln;
      VClineLength := 0 end

    else begin
      VCcharWrite(C);
      VClineLength := VClineLength + 1 end end end;

procedure VCstringOut;
var Y, Z: StringIndex;
begin
  (* Set Y to the index of the / that delimits the end of the string *)
  Y := S;
  while not ((StringPool[Y] = '/') and (StringPool[Y+1] = ')')) do
    Y := Y + 1;

  (* Now write out the string *)
  for Z := S to Y-1 do VCcharOut(StringPool[Z]) end;

procedure VCshortOut;
var Y, Z: 1..ShortStringLength;
begin
  (* Set Y to the index of the / that delimits the end of the string *)
  Y := 1;
  while not ((S[Y] = '/') and (S[Y+1] = ')')) do
    Y := Y + 1;

  (* Now write out the string *)
  for Z := 1 to Y-1 do VCcharOut(S[Z]) end;
