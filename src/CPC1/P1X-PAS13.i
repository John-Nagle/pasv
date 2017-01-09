{===============================================================}
{	The following routines parse the body of a block	}
{===============================================================}



procedure statelist (stopper: symbol);
    forward;
{
	nullstatement  --  used when icode syntax requires a statement
}
procedure nullstatement;
begin
    genbyte(8 {NULL});			{ null statement }
end {nullstatement};		
{
	seqend  --  generate SEQ operator if required

	Note that the operand of SEQ is one less than the
	number of statements covered by the SEQ.
}
procedure seqend(n: compoundtally);	{ number of statements }
begin
    if n = 0 then nullstatement;	{ for 0, must have one }
    if n > 1 then begin			{ unnecessary for 1 }
	genbyte(152 {SEQ});		{ generate SEQ operator }
	genbyte(n-1);			{ number of statements }
	end;
end {seqend};
{
	seqcount  --  count part of compound statement or assertion
}
procedure seqcount(var n: compoundtally); { compound part tally }
begin
    n := n + 1;				{ increment count }
    if n >= maxinseq then begin		{ if max per SEQ operator reached }
	seqend(maxinseq);		{ end this sequence }
	n := 1;				{ we now have one statement }
	end;
end {seqcount};	
{
	assertion  --  parse one assertion
}
procedure assertion;
begin
    valueexpression;				{ parse the expression }
    if not comptypes(gattr.atype, boolptr) then	{ if expression not Boolean }
	error(135 {operand must be Boolean-valued}); { diagnose }
end {assertion};
{
	statement  --   parse one statement (recursively)
}
procedure statement;
var
  p: itp;
  lclstmntnmr: longint;	{save the startement number locally}
  genstmntnmr: boolean;	{ TRUE if we must generate stmt nr (~NULL)}


procedure compound;
  {parse a compound statement}

begin {compound}
  statelist(endsy);
  if sym.sy = endsy
  then insymbol	{ gobble up 'end' }
  else error(13)
end {compound};



procedure assignment(fp: itp);
var
  q:stp;
begin {assignment}
if (fp^.klass = proc) and (fp^.itype <> nil) then
 begin		{assignment to a function name}
  if blkname <> fp then	{ .. assigning to THIS function?? }
    error( 169  { assign to fn not allowed outside function } );
  fp := fp^.next	{ dummy variable for returned value }
 end;
selector(fp,change);		{ parse left side, which will be changed }
q := gattr.atype;
if sym.sy = becomes then begin
  insymbol;
  valueexprconverted(q);	{ get compatible value with LHS or error }
  if (q <> nil) and (gattr.atype <> nil) then with q^ do
   begin
    if form = arrayt then
     begin
      genbyte(135 {MOVEM});
      if typsize(aeltyp) < 2 then genbyte(1) else genbyte(2);
      genword(size)
     end
    else if (form = recordt) or (form = devicet)  then
     begin
      if size <= 2 then
	genbyte(10 {STOL})
      else
       begin
  	genbyte(135 {MOVEM});
  	if odd(size)
  	then begin genbyte(1); genword(size) end
  	else begin genbyte(2); genword(size div 2) end
       end
     end
    else
      genbyte(10 {STOL})
   end
  end
else error(51)
end {assignment};


procedure raisestatement;
  var p:itp;
begin
  if sym.sy = ident then
    begin
      p := searchid([vars,field]);
      insymbol;
      selector(p,referenceandchange);
      if comptypes(gattr.atype,xcptnptr) then
	genbyte(165 { RAISE } )
      else
	error (22  {"expected exception designator"} )
    end
  else
    error(22  {"expected exception designator"})
end;	{raisestatement}



procedure initstatement;
  var p: itp;
begin  	{initstatement}
  if sym.sy = ident then
    begin  	{check for valid monitor/module name}
      p := searchid([modul]);
      insymbol;	{eat the identifier}
      if p = udptrs[modul] then
	error(175  {"expected identifier declared as monitor/module"} )
      else
	begin	{identifier is a monitor/module}
	  if p^.exclusion = montyp then	{ if init of monitor }
	    if blktype <> main then 	{ from non-main program }
	      error(1044 {INIT of monitor from non-main-program});
	  if foreign then 			{ if imported module }
	    error(1043 {INIT of imported module forbidden});
	  genbyte(224 + p^.mlev  {ICALL} );
	  genbyte(p^.maddr)
	end	{good identifier}
    end	{sym = ident}
  else
    error( 2  {"identifier expected"})

end;	{initstatement}


procedure ifstatement;
begin {ifstatement}
valueexpression;
if not comptypes(gattr.atype, boolptr) then error(135);
if sym.sy = thensy then insymbol else error(52);
statement;
if sym.sy = elsesy
then begin insymbol; statement end
else genbyte(8 {NULL});
genbyte(144 {IF})
end {ifstatement};


procedure casestatement;
var
  q: stp;
  lmin, lmax, nrent, nrval: longint;
  statementsparsed, labelsparsed: boolean;  {parser loop control}
begin {casestatement}
valueexpression;	{ parse case selector expression }
q := gattr.atype;
getbounds(q, lmin, lmax);
if lmin <> 0 then
  begin		{bias selector expr to match label bias }
    genlit( lmin );
    genbyte( 33 {ISUB} )	{should be optimized out by pass2}
  end;
if sym.sy = ofsy then insymbol else error(8);
nrent := 1;
statementsparsed := false;	{goes TRUE when all statements in case parsed}
repeat			{ parse case statements }
  nrval := 0;		    {initialize count of labels on this statement}
  labelsparsed := false;    {TRUE when done parsing labels}
  repeat			{ parse labels for current statement }
    expression;			    { next label }
    if gattr.akind = cst	    { it must be a constant }
    then begin
      if q <> nil
      then begin
	if not assignable(gattr.atype, q) then error(147);
	genlit(gattr.avalue.ival - lmin);
	lmax := imax(lmax, gattr.avalue.ival);
	nrval := succ(nrval)	    {count numb of labels for this case}
	end
      end
    else error(106);
    if sym.sy <> comma then
      labelsparsed := true
    else
      insymbol			{get past the comma}
  until labelsparsed;
  if sym.sy = colon then insymbol else error(5);
  statement;
  genbyte(146 {ENTRY});
  genbyte(nrval);
  nrent := succ(nrent);
  if sym.sy <> semicolon then
    statementsparsed := true
  else
    insymbol	{get past semicolon to next statement in case}
until statementsparsed;
if sym.sy = endsy then insymbol else error(13);
lmax := lmax - lmin;
if lmax > 255 then error(173);
genbyte(145 {CASE});
genbyte(nrent);
genword(lmax)
end {casestatement};


procedure repeatstatement;
begin {repeatstatement}
statelist(untilsy);
if sym.sy = untilsy
then begin
  insymbol;
  valueexpression;
  if not comptypes(gattr.atype, boolptr) then error(135);
  genbyte(8 {NULL});	{ no exit stub }
  genbyte(148 {EXIT})
  end
else error(53);
genbyte(8 {NULL});	{ no code after exit }
genbyte(147 {LOOP});
genbyte(2)		{ arg count - 2 }
end {repeatstatement};


procedure whilestatement;
begin {whilestatement}
genbyte(8 {NULL});	{ no code before exit }
valueexpression;
if not comptypes(gattr.atype, boolptr) then error(135);
if sym.sy = dosy then insymbol else error(54);
genbyte(96 {NOT});
genbyte(8 {NULL});	{ no exit stub }
genbyte(148 {EXIT});
statement;
genbyte(147 {LOOP});
genbyte(2)		{ arg count - 2 }
end {whilestatement};


procedure forstatement;
var
  p: itp;
  lsy: symbol;
begin {forstatement}
if sym.sy = ident
then begin
  p := searchid([vars]);
  if p^.vlev <> level then
    error( 151  {for loop variable must be local } );
  selector(p,referenceandchange);
  if gattr.atype^.form > longintt  then
    error( 143   {"illegal type of loop control variable"} );
  insymbol
  end
else error(2);
if sym.sy = becomes
then begin insymbol; valueexpression end
else error(51);
if (sym.sy = tosy) or (sym.sy = downtosy)
then begin
  lsy := sym.sy;
  insymbol;
  valueexpression;
  if lsy = tosy then genlit(1) else genlit(-1)
  end
else error(55);
if sym.sy = dosy then insymbol else error(54);
statement;
genbyte(149 {FOR})
end {forstatement};


procedure withstatement;
var
  p: itp;
  nrwiths: longint;
  done: boolean;
begin {withstatement}
nrwiths := 0;
repeat
  if sym.sy = ident
  then begin
    p := searchid([vars,field,konst]);
    insymbol
    end
  else begin
    error(2);
    p := udptrs[vars]
    end;
  selector(p,reference);		{ WITH is considered a reference }
  if gattr.atype <> nil then
   if gattr.atype^.form in [recordt, devicet]  then
    begin
      if top < maxdis
      then begin
	top := succ(top); nrwiths := succ(nrwiths);
	with display[top] do
	 begin
	  fname := gattr.atype^.fstfld;
	  scope := rcrd;
	  rtype := gattr.atype;
	  dvclass := lastvclass;	{ record verclass of WITH arg }
	  daccess := lastaccess;	{ record accessabilty of WITH arg }
	  if ( gattr.access = direct )  then
	   begin	{ record variable has a constant address }
	    occur := crec;
	    dlev := gattr.alevel;
	    daddr := gattr.addr;
	    genbyte(2 {DEL})	{ don't need the reference we already gen'ed }
	   end
	  else if gattr.access = absolute then
	   begin	{ record variable is a device }
	    occur := cdev;
	    dvaddr := gattr.addr;
	    genbyte( 2 {DEL} )
	   end
	  else
	   begin
	    occur := vrec;
	    tc := succ(tc);	{ need new temp to store 'with' pointer }
	    tnum := tc;
	    genbyte(9 {REFER})
	   end
	 end { of with }
	end
      else error(250)
      end
    else error(140);
  if sym.sy = comma then
    begin done := false; insymbol  end
  else  done := true
until done;
if sym.sy = dosy then insymbol else error(54);
statement;
for nrwiths := nrwiths downto 1 do begin
  if display[top].occur = vrec
  then begin
    genbyte(141 {DTEMP}); genword(tc);
    tc := pred(tc)
    end;
  top := pred(top)
  end
end {withstatement};
{
	Verifier-language statements
}
{
	PROOF statement

	syntax:   proof <statement>
}
procedure proofstatement;
begin
    if vmode = proofmode then error(1020 {PROOF statement in PROOF mode});
    entervmode(proofmode);		{ enter proof mode }
    statement;				{ parse statement }
    exitvmode;				{ leave proof mode }
    if not verifier then nullstatement;	{ replaces ommitted code for compiler }
end {proofstatement};
{
	MEASURE statement

	syntax:	measure ( <expression> )

	The MEASURE statement is only meaningful in loops; verifier pass 2 check
}
procedure measurestatement;
begin
    entervmode(proofmode);		{ enter proof mode }
    if sym.sy <> lparen then begin	{ if not left paren }
	error(9 {expected '(' });	{ diagnose }
    end else begin			{ begin expression }
	insymbol;			{ discard ( }
        valueexpression;		{ parse the expression }
        if not assignable(gattr.atype, intptr) then { if bad expr type }
	    error(1041 {longint expression required}); { diagnose }
	if sym.sy = rparen then begin	{ if ) found }
	    insymbol;			{ skip over it }
	end else begin			{ if no ) after expr }
	    error(4 {expected ')' });	{ diagnose }
	    end;			{ end no ) }
	end;				{ end found ( }
    genbyte(247 {MEAS});		{ MEASURE statement operator }
    exitvmode;				{ leave proof mode }
    if not verifier then nullstatement;	{ replaces ommitted code for compiler }
end {measurestatement};
{
	assertionstring  -- parse forms of

	<assertions> ::=  <assertion> |
			  <assertion> ',' <assertions>

        <assertionstring> ::=  '(' <assertions> ')'

	This handles ASSERT, STATE, and SUMMARY statements.
}
procedure assertionstring(subcode: longint);	{ STATE, ASSERT, etc. }
var cnt: longint;				{ count of ASSERT parts }
begin
    if sym.sy <> lparen then begin		{ if not left paren }
	error(9 { expected '(' });		{ diagnose }
    end else begin				{ '(' }
	entervmode(assertmode);			{ enter proper mode }
	cnt := 0;				{ count of asserts }
	repeat					{ for string }
	    insymbol;				{ to beginning of assertion }
	    if verifier then genlineid;		{ location in source }
	    assertion;				{ parse one assertion }
	    cnt := cnt + 1;			{ count of assertions }
            until sym.sy <> comma;		{ continue if not comma }
	if cnt > maxargs then 			{ if too many args }
	    error(1036 {Too many arguments in assertion statement})
	else begin				{ OK to generate }	
	    genbyte(254 {ASERT});		{ assertion sequence }
	    genbyte(cnt);			{ arg count }
	    genbyte(subcode);			{ kind of assertion }
	    end;
	if sym.sy = rparen then begin		{ expecting ')' }
	    insymbol;				{ skip over it }
	end else begin				{ if not right paren }
	    error(4 { expected ')' });		{ diagnose }
	    end;
	exitvmode;				{ return to previous mode }
	end;
	if not verifier then nullstatement;	{ in compiler, generate dummy }
end {assertionstring};

begin {statement}
  if verifier then genlineid;		{ line number for debug aids }
{ check for labels and put out error message: }
if sym.sy = intconst then begin
  error(notyetimpl);
  insymbol;
  if sym.sy = colon then insymbol else error(5)
  end;
if not (sym.sy in			{ tokens which may start statements }
     [ident, beginsy, raisesy, initsy, ifsy, casesy,
	whilesy, repeatsy, forsy, withsy,
	semicolon, endsy, elsesy, untilsy, whensy,
	measuresy,
	assertsy, statesy, summarysy,
	proofsy]) then
    begin error(6); insymbol; end 	{ error }
else
case sym.sy of
  ident: begin
    p := searchid([vars,field,proc]);
    insymbol;
    if (p^.klass = proc) and (p^.itype = nil)
    then call(p)	{ procedure call }
    else
      begin
	if foreign then error(170 {assignment to imported/exported variables not allowed} );
	assignment(p)
      end
    end;
  beginsy:
    begin insymbol; compound end;
  raisesy:
    begin insymbol; raisestatement end;
  initsy:
    begin insymbol; initstatement  end;
  ifsy:
    begin insymbol; ifstatement end;
  casesy:
    begin insymbol; casestatement end;
  whilesy:
    begin insymbol; whilestatement end;
  repeatsy:
    begin insymbol; repeatstatement end;
  forsy:
    begin insymbol; forstatement end;
  withsy:
    begin insymbol; withstatement end;
  measuresy:
    begin insymbol; measurestatement end;
  proofsy:
    begin insymbol; proofstatement end;
  assertsy:
    begin insymbol; assertionstring(1); end;
  statesy:
    begin insymbol; assertionstring(2); end;
  summarysy:
    begin insymbol; assertionstring(3); end;	
  semicolon, endsy, elsesy, untilsy, whensy:
    genbyte(8 {NULL});
  end;

		{generate the statement number meta operator}
  genbyte( 0 {META} ); genbyte( 1 {meta type = stmt nr } );
  genword( lclstmntnmr ); {saved from start of stmnt }

end {statement};


procedure statelist(stopper: symbol);
var
    scnt: compoundtally;	{ count of statements }
begin {statelist}
  if (sym.sy <> endsy) and (not vmodes.bodyallowed) then { if in RULE function }
    error(1055 { Not allowed in RULE function definition });
statement;
scnt := 1;			{ there is now one statement }
while (sym.sy <> stopper) and (sym.sy <> endsy) and (sym.sy <> eofsy) do
  if sym.sy = semicolon
  then begin
    insymbol;	{ gobble up ';' }
    statement;
    seqcount(scnt);		{ count and generate sequence }
    end
  else begin
    error(14);
    skip(stopper)
    end;
seqend(scnt);			{ terminate statement sequence }
end {statelist};


procedure  exception;
 			{parse an exception handler  }
			{****************************}

  var  namecount: longint;

  procedure  exceptionname;
				{scan current symbol for exception name}
  var  p: itp;

  begin
    if sym.sy = ident then
      begin  {look it up and check type}
	p := searchid([vars]);
	{ ***NOT SURE WHAT TO DO ABOUT EXTRA/FREE re EXCEPTIONS ***}
	insymbol;	{rid ourselves of the identifier}
	if (p = udptrs[vars]) or (p^.itype^.form <> xcptnt)  then
	  error (22 {"expected exception name"})
	else
	  begin		{emit code to label handler for this exception}
		{the variable descriptor for the exception is used }
		{to mark the handler				}
	    genbyte(176 {VARBL} + p^.vlev);
	    genbyte(p^.itype^.size);
	    genword(p^.vaddr)
	  end
      end
    else  if sym.sy = othersy then
      begin  {the reserved ident "others" is compiled as LIT 1  (TRUE) }
	genlit( 1 )
      end
    else
      error(22 {"expected exception name"})

  end; 	{exception_name}


begin	{exception}

  insymbol;	{eat the "WHEN" which invoked this routine}
  namecount := 0;	{initialize count of exceptions attached to this handler}
  exceptionname;	{get the first exception}
  namecount := namecount+1;

  while sym.sy = comma do
    begin	{more exceptions for this handler}
      insymbol;	  	{eat the comma}
      exceptionname;	{and get the next exception}
      namecount := namecount+1;
    end;

  if sym.sy = dosy then insymbol else error(54 {"do expected"});
  statement; 	{parse the body of the handler}

  genbyte( 146 {ENTRY} );
  genbyte( namecount );

  if sym.sy = semicolon then insymbol; {incase there are more}

end;	{exception}
{
	Verification declarations
}
{
	extradecl  --  EXTRA PROCEDURE or EXTRA FUNCTION declaration
}
procedure extradecl;
var isfunction: boolean;		{ true if function }
begin
     if sym.sy in [proceduresy, functionsy] then begin { if proc or fn }
	isfunction := sym.sy = functionsy; { set function switch }
	entervmode(proofmode);		{ begin compiling proof code }
	insymbol;			{ get procedure name }
	procdecl(true,isfunction); 	{ compile it }
	exitvmode;			{ end compiling proof code }
     end else begin			{ bad statement }
	error(18); skip(semicolon); insymbol; { skip statement }
        end;
end {extradecl};
{
	ruledecl  --  RULE FUNCTION declaration
}
procedure ruledecl;
begin
    if sym.sy = functionsy then begin	{ if function }
	entervmode(rulemode);		{ begin compiling proof code }
	insymbol;			{ eat "rule" }
	procdecl(true,true);		{ compile function }
	exitvmode;			{ end compiling proof code }
    end else begin			{ bad statement }
	error(18); skip(semicolon); insymbol; { skip statement }
        end;
end {ruledecl};
{
	assertionsequence  --  parse set of assertions separated by
	semicolons.

	used for ENTRY, EXIT, EFFECT, and INVARIANT

	Code generated here is diverted to the diversion file for
	this block (headericode) to be copied to the main file
	after the block header has been completely processed.
	When running as the compiler, code generation is turned off
}
procedure assertionsequence(subcode: longint);	{ class of assertion }
begin
    diverticode := true;		{ diver this to holding file }
    while sym.sy in			{ while possible beginning of expr }
      [addop, ident, intconst, fixconst,
	stringconst, lparen, notsy, returnsy, lbrack]
    do begin
	if verifier then genlineid;	{ line number for error messages }
	assertion;			{ parse assertion }
	genbyte(252 {VDECL});		{ VDECL }
	genbyte(subcode);		{ indicated subclass }
        seqcount(headerasserts);	{ generate SEQ if needed }
	if sym.sy <> semicolon then error(14 {expected ';'});
	insymbol;			{ pass ';' }
	end;				{ end expr loop }
    diverticode := false;		{ end diversion }
end {assertionsequence};
{
	entrydecl  --  ENTRY declaration
}
procedure entrydecl;
begin
    entervmode(entrymode);			{ begin entry mode }
    if blktype in [montyp, modtyp] then { if INIT entry/exit }
        assertionsequence(16)			{ parse string }
    else					{ if proc/fn }
        assertionsequence(11);			{ parse string }
    exitvmode;
end {entrydecl};
{
	exitdecl  --  EXIT declaration
}
procedure exitdecl;
begin
    entervmode(exitmode);			{ begin exit mode }
    if blktype in [montyp, modtyp] then { if INIT entry/exit }
        assertionsequence(17)			{ parse string }
    else					{ if proc/fn }
        assertionsequence(12);			{ parse assertion string }
    exitvmode;
end {exitdecl};
{
	effectdecl  --  EXIT declaration
}
procedure effectdecl;
begin
    entervmode(effectmode);			{ begin effect mode }
    assertionsequence(13);			{ parse assertion string }
    exitvmode;
end {effectdecl};
{
	invariantdecl --  INVARIANT declaration
}
procedure invariantdecl;
begin
    entervmode(invariantmode);			{ begin invariant mode }
    case blktype of				{ fan out on type of block }
    proctyp:					{ routine }
	assertionsequence(15);			{ entryexit type assertions }
    montyp, modtyp:				{ static block }
	assertionsequence(14);			{ invariant type assertions }
    main: assert(false);			{ no invariants in outer block }
    end;
    exitvmode;
end {invariantdecl};
{
	depthdecl  --  DEPTH declaration
}
procedure depthdecl;
begin
    entervmode(assertmode);		{ assertion mode }
    depthdecls := depthdecls + 1;	{ count depth declarations }
    if depthdecls > 1 then 		{ if more than one in block }
	error(1042 {Multiple DEPTH declarations for proc/fn});
    diverticode := true;		{ diver this to holding file }
    if verifier then genlineid;		{ line number for error messages }
    valueexpression;			{ parse depth expression }
    if not assignable(gattr.atype, intptr) then { if bad expr type }
	error(1041 {longint expression required}); { diagnose }
    genbyte(248 {DEPTH} );		{ DEPTH operator }
    seqcount(headerasserts);		{ generate SEQ if needed }
    if sym.sy <> semicolon then error(14 {expected ';'});
    insymbol;				{ pass ';' }
    diverticode := false;		{ end diversion }
    exitvmode;				{ end assertion mode }
end {depthdecl};
{
	verheader  --  mark assertions from header

	The verification header of a block consists of a sequence
	of ASRT ``statements'' combined into a compound statement
	with SEQ operators.

	The icode forming the verification header is diverted into
	the diversion file for the current block as it is generated.
	In verheader, the diversion file is copied to the main file,
	so that all the header code for the block will be together.
}
procedure verheader;
var icodebyte: byte;			{ actually 0..255 }
begin
    assert(verifier);			{ called only in verifier }
    assert(not diverticode);		{ done diverting }
    reset(headericode);			{ rewind diversion file }
    while not eof(headericode) do begin { for entire diversion file }
	    read(headericode,icodebyte);	{ read a byte }
	    genbyte(icodebyte);		{ generate into main file }
	    end;
    seqend(headerasserts);		{ combine into one ``statement'' }
end {verheader};
{
	headerpart  --  parse one declaration part of block head
}
procedure headerpart;
var s: symbol;				{ name of command }
begin
    s := sym.sy; insymbol;		{ advance past name of part }
    case s of				{ select appropriate statement }
	constsy:  	constdecl;
	typesy:  	typedecl;
	valuesy:  	valuedecl;
	varsy:  	vardecl;
	proceduresy:  	procdecl(false,false);
	functionsy:  	procdecl(false,true);
	rulesy:		ruledecl;
	extrasy:  	extradecl;
	modulesy:  	moddecl(false,false);
	monitorsy:  	moddecl(true,false);
	entrysy:  	entrydecl;
	exitsy:  	exitdecl;
	effectsy:  	effectdecl;
	invariantsy:  	invariantdecl;
	depthsy:	depthdecl;
	end;
end {headerpart};
{
    blockheader  --  parse header part of block

    Only header parts appropriate to the current type of block
    are accepted.

    Code for routines embedded in block headers appears in the
    icode in sequence with the source, but verification headers
    (ENTRY, EXIT, EFFECT, and INVARIANT) are diverted and output
    just before the icode representing the body of the block.
    (In the compiler, verification headers are suppressed).
}
procedure blockheader;
var valid: boolean;			{ true if symbol valid }
begin
    headerasserts := 0;			{ clear count of header asserts }
    depthdecls := 0;			{ number of DEPTH declarations }
    if verifier then begin		{ if verifier only }
        assign(headericode,GetTempFileName('','TMPver'));   { pick temporary file name }
	    rewrite(headericode);		{ open temp file for this block }
	end;
    while not (sym.sy in [beginsy, eofsy]) do begin { until BEGIN }
    case blktype of		{ different types of block }
    proctyp: valid := sym.sy in
	[constsy, typesy, valuesy, varsy, proceduresy, functionsy,
	depthsy,
	extrasy, modulesy, entrysy, exitsy, effectsy, invariantsy];
    montyp: valid := sym.sy in
	[constsy, typesy, valuesy, varsy, proceduresy, functionsy,
	entrysy, exitsy,
	extrasy, modulesy, invariantsy];
    modtyp:  valid := sym.sy in
	[constsy, typesy, valuesy, varsy, proceduresy, functionsy,
	entrysy, exitsy,
	extrasy, modulesy, invariantsy];
    main:   valid := sym.sy in
	[constsy, typesy, valuesy, varsy, proceduresy, functionsy,
	rulesy,
	extrasy, monitorsy, modulesy];
    end;
    if not valid then begin		{ if invalid }
	error(18);			{ diagnose }
	skip(semicolon);		{ skip construct }
	insymbol;			{ start next construct }
    end else begin			{ if valid header part }
	if not vmodes.bodyallowed then	{ if in RULE function }
	    error(1055 { Not allowed in RULE function definition });
	headerpart;			{ proceed with parsing }
	assert(not diverticode);	{ must exit in normal mode }
	end;
    end;				{ end header loop }
    assert(not diverticode);		{ must not still be diverting }
    genbyte(5 {ident});  genid(blkname^.name); { identify the block }
    if verifier then verheader;		{ mark end of assertions if verifier }
					{ current symbol is BEGIN or eof }
end {blockheader};
                         	
begin {block}
if not ( (blktype=modtyp) or (blktype=montyp) ) then
  begin
    mark( hmarker );
    lc := 0
  end;
tc := 0;

fwptr := nil;  varlst := nil;
diverticode := false;			{ icode to normal icode file }
blockheader;				{ parse block header }

		{ end of declarations - initialize stmnt numbers for this block }
  stmntnmr := 0;

insymbol;	{ gobble the 'begin' }
genbyte(5 {ident});   genid(blkname^.name);
			{ if this is the main program then }
			{ emit a signal initialization operator }
if blktype = main then
  if signalcount > 0 then 	{some signals were declared}
    begin	{output signal initialization operator}
      for i := 1 to signalcount do
	begin	{generate an argument to ISGNL for th ith signal}
	  genbyte( 176 {VARBL}  + signallist[i].varlev  );
	  genbyte( usize[signalt] );
	  genword( signallist[i].varaddr );
		  	{ generate intervening node for hardware signals}
	  if signallist[i].hardwired then
	    begin
	      genbyte( 130 {SIGNL} );
	      genword( signallist[i].vecaddr )
	    end
	end;

      genbyte( 159  {ISGNL} );	{ generate the initialization operator}
      genbyte( signalcount );	{  .. number of arguments }
    end
  else	{ no signals declared }
    genbyte( 8 {NULL} )
else	{ not in the main program }
  genbyte( 8 {NULL} );

statelist(whensy);			{parse the body of this block}
					{exception handlers, if any}
{	TEMP turn off exception handlers, which don't work yet	
while sym.sy = whensy do	
  exception;
	TEMP turn off end	}
genbyte(8 {NULL} );			{ ***TEMP*** placeholder for exceptions}
if sym.sy = endsy then insymbol
else  error(13  {"expected END"});

		{***** FUTURE FEATURE !!! *****}
	{ Must put in code to generate the exception exits
	  required for this block !!			  }

genbyte( 150  {BLOCK} );
genbyte( 3	{ nrargs - TEMPORarily = three } );

if verifier then genbyte(253 {VHEAD});	{ blocks ver header and block together }
if odd(dc) then gendbyte(0);	{ round dc to word boundary }
genbyte(7 {end});  {  generate END psuedo operator  }
  if blkname^.klass = proc then
    genbyte (blkname^.paddr {proc number} )
  else   { assume module/monitor }
    genbyte (blkname^.maddr {module number} );
  rvsize := typsize(blkname^.itype);
  genbyte(rvsize);
  if blkname^.klass = proc then
    genaddressword(lc)			{ lc (local counter) is negative }
  else
    genword(0);		{module variables belong to containing scope - allocate none here}
  genword(ac - rvsize);  genword(dc);
	{output the symbol table for this block}
writeln( symfil );
writeln( symfil,'* ',blkname^.name^.s:ord(blkname^.name^.l),' ',top:3 ); {*TEMP*}
printident( display[top].fname );

if option['T'] then printtables(false);
if verifier then begin      	 	{ generate variable file for verifier }
    varfilegen(top);			{ tables for current block }
    paramfilegen(blkname,blktype); 	{ args for proc/fn }
    end;
if not ( (blktype=modtyp) or (blktype=montyp) ) then release( hmarker );
end {block};
