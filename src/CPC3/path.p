#include "global.h"

procedure WritePathId;
begin
  writeln('path.p 1.16') end;

function PathTrace;	(* Returns false if error detected *)

type
  PointerToJstack = ^Jstack;
  Jstack = record
    Top: PointerToJnode;
    Pop: PointerToJstack;
    end;

var
  LiveEnv: EnvIndex;      (* Variables that are currently live  *)
  Path: PointerToJstack;  (* The current partial path (a stack) *)
  ErrorCount: integer;    (* The number of diagnostics generated so far *)

procedure PushStatus(VL: PointerToVariableList; NewStatus: Status);
(* Input: Variable Status data structure, VL, NewStatus
 * Output: Variable Status data structure
 * Effect: For each variable on VL, push NewStatus onto its
 *   status stack, modifying LiveEnv as appropriate
 *)
var MustAllocate: boolean;       (* Whether a new block must be allocated *)
    NewNode: PointerToStatusNode;(* Temp storage for pointer shuffling *)
begin
  (* For each variable on VL execute the loop body *)
  while (VL <> nil) do with VL^.Head^ do begin
    (* Decide the current status of the variable and whether or not
     * a new node must be allocated.  By convention, a variable with
     * no status nodes is considered to be Dead
     *)
    if StatusStack = nil then 
      MustAllocate := true
    else
      MustAllocate := StatusStack^.BitCount = BitBlockSize;

    (* Allocate a new StatusNode if necessary *)
    if MustAllocate then begin
      new(NewNode);
      NewNode^.Pop := StatusStack;
      NewNode^.BitCount := 1;
      StatusStack := NewNode end
    else 
      StatusStack^.BitCount := StatusStack^.BitCount + 1;

    (* Push the new status *)
    StatusStack^.BitBlock[StatusStack^.BitCount] := NewStatus;

    (* Indicate the New status in LiveEnv *)
    EnvPool[LiveEnv + Index] := ord(NewStatus);
    VL := VL^.Tail end end;

procedure PopStatus(VL: PointerToVariableList);
(* Input: Variable Status data structure, VL.
 * Output: Variable Status data structures
 * Assumption: For every variable V, there are at least as many entries
 *   on V's status stack as there are occurrences of V on VL.
 * Effect: For each variable on VL, pop its status stack,
 *   modifying LiveList as appropriate.
 *)
var NewStatus: Status; 
    DeleteNode: PointerToStatusNode;
begin
  while (VL <> nil) do with VL^.Head^ do begin
    if StatusStack^.BitCount = 1 then begin
      DeleteNode := StatusStack;
      StatusStack := StatusStack^.Pop;
      dispose(DeleteNode) end
    else StatusStack^.BitCount := StatusStack^.BitCount - 1;

    if StatusStack = nil then
      NewStatus := Dead
    else
      NewStatus := StatusStack^.BitBlock[StatusStack^.BitCount];

    EnvPool[LiveEnv + Index] := ord(NewStatus);
    VL := VL^.Tail end end;

function CommonEnvLive(E: EnvIndex): boolean;
(* Input: E, LiveEnv.
 * Output: through the function name
 * Effect: Indicates whether any of the variables with a nonzero
 *  entry in E are live.
 *)
var X: VariableIndex;
    MatchFound: boolean;
begin
  MatchFound := false;
  X := 0;
  while (X < EnvLength) and not MatchFound do begin
    MatchFound := (EnvPool[E + X] <> 0)
      and (EnvPool[LiveEnv + X] = ord(Live));
    X := X + 1 end;
  CommonEnvLive := MatchFound end; 

function CommonListLive(L: PointerToVariableList): boolean;
(* Input: L, LiveEnv.
 * Output: through the function name
 * Effect: Indicates whether any of the variables on the list L are live.
 *)
var MatchFound: boolean;
begin
  MatchFound := false;
  while (L <> nil) and not MatchFound do begin
    MatchFound := (EnvPool[LiveEnv + L^.Head^.Index] = ord(Live));
    L := L^.Tail end;
  CommonListLive := MatchFound end; 

procedure GenerateVC(BreakNode: PointerToJnode);
(* Input: The J-graph, Path.
 * Output: ErrorCount, through writeln.
 * Assumption: The Path stack represents a complete path
 *  from a BreakN down to a RequireN.
 * Effect: The VC for that path is generated and sent to the theorem
 *   prover.  If the VC cannot be proved, a diagnostic is printed,
 *   and ErrorCount is incremented.
 *)
var
  P: PointerToJstack;         (* Used to descend down the path stack *)
  Conjunct,
  SavedConjunct: StringIndex; (* Used to generate VC *)
  ParenCount: integer;        (* Number of unbalanced parens output *)

procedure PathOut(var F: text);
(* Input: BreakNode, Path.
 * Output: through F.
 * Effect: write the path implied by BreakNode and Path.
 *)
var Q: PointerToJstack;
begin
  write(F, '   ');
  WriteString(F, BreakNode^.PathStart);
  writeln(F);
  Q := Path;
  while Q^.Pop <> nil do with Q^.Top^ do begin
    if Jtag = BranchN then begin
      write(F, '   ');
      WriteString(F, PathName);
      writeln(F) end;
    Q := Q^.Pop end end;

begin
  (* Output the verification condition.
   * The VC is of the form:
   * (implies! (and! P1 (and! P2 ... (and! Pn-1 Pn)...) Q) 
   * Where Pi's come from Requires, News, and Whens, and the
   * Q comes from the Require at the bottom of the stack
   *)

  VCbegin;
  VCshortOut('(implies!/)');
  ParenCount := 1;

  (* Set SavedConjunct to something that cannot be a conjunct *)
  SavedConjunct:= NullString;

  (* The last conjunct (Pn) is special in that it is not preceded
   * by an and.  We do not print a conjuct until we see if it is
   * followed by another conjuct.
   *)
  P := Path;
  while P^.Pop <> nil do with P^.Top^ do begin

    (* Determine if this node contains a conjunct to be printed *)
    if Jtag = RequireN then
      Conjunct := Requirement
    else if Jtag = ProclaimN then
      Conjunct := Proclamation
    else if Jtag in [NewN, RenewN] then
      Conjunct := NewState
    else if Jtag = WhenN then
      Conjunct := Constraint
    else
      Conjunct := NullString;

    if Conjunct <> NullString then begin
      if SavedConjunct <> NullString then begin
        (* Now we know that SavedConjunct is a conjunct, but not
         * the last one.
         *)
        VCshortOut(' (and! /)');
        ParenCount := ParenCount + 1;
        VCstringOut(SavedConjunct) end;
      SavedConjunct := Conjunct end;

    P := P^.Pop end;

  if SavedConjunct = NullString then 
    (* The VC has no conjucts, but we have already output an implication
     * that needs a hypothesis.
     *)
    VCshortOut(' (true!)/)')
  else begin 
    (* We output one fewer conjunct than and! operator *)
    VCcharOut(' ');
    VCstringOut(SavedConjunct) end;

  (* Output one parenthesis for every and! operator output *)
  while ParenCount <> 1 do begin
    VCcharOut(')');
    ParenCount := ParenCount - 1 end;

  VCcharOut(' ');
  VCstringOut(P^.Top^.Requirement);
  VCcharOut(')'); (* this paren closes the VC *)

  if TraceBeingDone then begin
    write(TraceFile, 'Verification condition for');
    WriteString(TraceFile, P^.Top^.ErrMsg);
    writeln(TraceFile);
    writeln(TraceFile, 'Path:');
    PathOut(TraceFile);
    flush(TraceFile) end;

  if not VCproved then begin
  (* Output the path trail *)
    ErrorCount := ErrorCount + 1;
    write('Could not prove');
    WriteString(output, P^.Top^.ErrMsg);
    writeln;
    writeln('for path:');
    PathOut(output);
    writeln end end;

function TraverseGraph: boolean;
var
  R:           PointerToJnode;   (* The RequireN being processed *)
  BranchFound: boolean;          (* Flag used to avoid a goto *)
  TempNode:    PointerToJstack;  (* Used to locally shuffle pointers *)
  PushNode:    boolean;
  StackMark:   PointerToJstack;
  CurrentNode: PointerToJnode;
  T:           Jtags;
begin
  { DumpJgraph; }
  LiveEnv := AllocateEnv;

  (* For every element R of the require list,
   * traverse the graph, starting at R, generating a VC
   * for each path traced.
   *)
  ErrorCount := 0;
  R := RequireList;
  while R <> nil do begin
    (* Initialize the Path to just the Require *)
    { writeln('Begin tracing for Require ', PointerName(R):1); }
    new(Path);
    Path^.Pop := nil;
    Path^.Top := R;
    PushStatus(R^.ReqLiveVars, Live);

    (* begin the traversal *)
    CurrentNode := R^.Previous;

    repeat
      { writeln('Current node is ', PointerName(CurrentNode):1); }
      T := CurrentNode^.Jtag;
      case T of

      BranchN, WhenN:
        PushNode := true;

      SplitN, JoinN, BreakN:
	PushNode := false;

      ProclaimN:
	(* We naively push the ProclaimN node only if it contains a variable
	 * that is currently relevant, ignoring the possibility of a variable
	 * becoming relevant after including more assertions.  This can be
	 * considered a bug, and is justifiable only because Proclaims are
	 * never absolutely necessary; they are only an aid theorem prover.
	 *)
	PushNode := CommonListLive(CurrentNode^.ProcLiveVars);

      RequireN:
	(* Requires are treated just like proclaims *)
	PushNode := CommonListLive(CurrentNode^.ReqLiveVars);

      NewN, RenewN: begin
        PushNode := CommonListLive(CurrentNode^.NewDeadVars);

	(* If this is the end of a cluster of NewN nodes, we must
	 * remember where the top of the path stack was so that when we
	 * find all the members of the cluster that were pushed on the
	 * stack, we can process them all.
	 *)
	if (CurrentNode^.ClusterMark = EndCluster) then
	  StackMark := Path end end;
        
      if PushNode then begin
        (* Push CurrentNode onto the stack *)
	{ writeln('Push node ', PointerName(CurrentNode):1); }
        new(TempNode);
        with TempNode^ do begin
          Top := CurrentNode;
          Pop := Path end;
        Path := TempNode;

        (* Reflect in the live variable data structure the effects
         * of the node just pushed onto the Path stack
         *)
        with CurrentNode^ do begin
          if Jtag in [NewN, RenewN] then begin
            PushStatus(NewDeadVars, Dead);
	    (* For single nodes, we can update the live variables right
	     * now.  In a cluster, however, we must wait until all the
	     * dead variables in the cluster have been marked before 
	     * we mark any variables live.
	     *)
            if ClusterMark = Singleton then 
	      PushStatus(NewLiveVars, Live) end
          else if Jtag = WhenN then
            PushStatus(WhenLiveVars, Live) end end;

      (* Set CurrentNode to the next node to process.  How this is done
       * depends on the node type, which is in T.
       *)
      if T in [NewN, RenewN] then begin
	(* If we are processing the last element of a cluster of NewN nodes,
	 * update all the live variables now.
	 *)
	if (CurrentNode^.ClusterMark = BeginCluster) then begin
	  TempNode := Path;
	  while TempNode <> StackMark do begin
	    assert(TempNode^.Top^.Jtag in [NewN, RenewN]); 
	    PushStatus(TempNode^.Top^.NewLiveVars, Live);
	    TempNode := TempNode^.Pop end end;

	CurrentNode := CurrentNode^.Previous end

      else if T = JoinN then begin
        if CommonEnvLive(CurrentNode^.JoinEnv) then
          CurrentNode := CurrentNode^.Previous
        else
          CurrentNode := CurrentNode^.DirectDominator end

      else if T = BreakN then begin
	{ writeln('Generate VC'); }
        GenerateVC(CurrentNode);

        (* Pop the stack, searching for a branch node with alternatives
         * remaining or the bottom of the stack
         *)
        BranchFound := false;
        repeat
          (* Undo the whatever effects the top of the stack had on
           * the live variable data structure, leaving BranchFound indicating
           * whether or not we have found the BranchN node we are looking
           * for.
           *)
          with Path^.Top^ do begin
            case Jtag of
            NewN, RenewN: begin
	      (* Warning: something really sleazy is going on here.
	       * To properly undo the effects of an instruction, you undo
	       * each instruction in reverse order.  However, the instructions
	       * in a NewN cluster are not executed in sequence; first all
	       * the NewDeadVars lists are processed, and then all the 
	       * NewLiveVars lists are processed.  The inversion of this
	       * process cannot be a mirror image of the process because there
	       * is indication of where the clusters are on the path stack.
	       * The only reason this code works is that (1) we never stop
	       * processing in the middle of a NewN cluster, and (2) the
	       * effect of a series of PopStatus calls does not depend on
	       * their order.
	       *)
              PopStatus(NewLiveVars);
              PopStatus(NewDeadVars) end;
            WhenN:
              PopStatus(WhenLiveVars);
            BranchN:
              BranchFound := BranchMate <> nil;
            ProclaimN, RequireN:
              begin (* nothing *) end
            end end;
          
          if not BranchFound then begin
            (* Pop the stack *)
	    { writeln('Pop node ', PointerName(Path^.Top):1); }
            TempNode := Path^.Pop;
            dispose(Path);
            Path := TempNode end
            
        until BranchFound or (Path = nil);

        if Path <> nil then with Path^ do begin
          Top := Top^.BranchMate;
	  { writeln('Shift to node ', PointerName(Top):1 ); }
          CurrentNode := Top^.Previous end end
      else 
        (* Current Node is something mundate like SplitN or RequireN *)
        CurrentNode := CurrentNode^.Previous

    until Path = nil;
    PopStatus(R^.ReqLiveVars);
    R := R^.SimLink end;
    
    TraverseGraph := ErrorCount = 0;

    if ErrorCount = 0 then 
      write('No errors')
    else if ErrorCount = 1 then
      write('1 error')
    else
      write(ErrorCount:1, ' errors');
    
    writeln(' detected') end;

begin
  PathTrace := TraverseGraph end;
