{
	PROCEDURE/FUNCTION CALL PROCESSING
}
{
	valueexprconverted  --  get value expression of known type

	No type check requested if desired type is nil.
}
procedure valueexprconverted(q: stp);		{ type desired }
begin
  expression;
  if q <> nil then with q^ do begin		{ if type check/coerce, do }
    if not assignable(gattr.atype, q) then	{ check assignment legal }
       error( 129 {"type conflict of operands"} )
    else
      if form = sett then setcoerce(q);		{ to make empty sets behave }
						{ if fixed := int, convert }
    if (q^.form = fixedt) and (gattr.atype^.form = integert) then
      converttofix( gattr );
    end;					{ type check requested }
  if gattr.akind = cst then begin		{ if constant expression }
    if verifier then begin			{ verifier only }
      if not gattr.cstnamed then begin		{ if unnamed constant }
        if gattr.avalue.kind = data then begin	{ if in constant data space }
	  {  Put unnamed constant data object in varfile }
	  vdataconst(gattr.avalue.daddr, gattr.atype,
		lastfilenumber, lastlinenumber);
	  end;					{ if constant occupies space }
        end;					{ end unnamed constant }
      end;					{ end verifier only }
    gencon(gattr);
    gattr.akind := exp
    end;
  assert(gattr.atype <> nil);			{ ***DEBUG*** type must exist }
end {valueexprconverted};
{
	valueexpression  --  convert current expression to value expression
}
procedure valueexpression;
begin {valueexpression}
    valueexprconverted(nil);		{ get without type check }
end {valueexpression};

procedure call (fp: itp);
var
  lattr: attr;
  ptem: stp;
  pid: itp;			{ name pointer, for module test for defined }
  subsctyp: stp;		{ type of subscript for defined }


procedure calluser (fp: itp);
var
  p: itp;
  q: stp;
  nrofparm, retvsize: integer;
  formalextra: boolean;		{ true if formal arg is EXTRA }
  want: verclass;		{ desired verclass of formal arg }
begin {calluser}
  formalextra := false;		{ assume formal arg is not extra }
  if not vmodes.variableref[fp^.pvclass] then begin { check for EXTRA routine }
      error(1022 {Call to EXTRA routine in non-PROOF code});
      end;
  nrofparm := 0;
  p := fp^.next;	{ head of formal parameter list }
  if fp^.itype <> nil then
    begin	{ its a function }
      retvsize := typsize(p^.itype);
      p := p^.next		{ skip over returned value dummy }
    end
  else retvsize := 0;
  if sym.sy = lparen then
   begin	{ parse actual parameter list }
   repeat
    if p <> nil then begin	 	{ if next formal arg exists }
	assert(p^.klass = vars);	{ must be a var }
					{ is formal arg EXTRA? }
	formalextra := (p^.vclass = extravar) or (fp^.pvclass = extravar);
	end;
{
	Code suppression when compiling code with verification statements

	When the compiler (not the verifier) is in proof mode, no code
	is generated.  Since some arguments to a non-PROOF procedure
	may be EXTRA arguments, the actual arguments to procedures
	are compiled in PROOF mode if the formal argument is EXTRA.
	This suppresses code generation for the EXTRA arguments
	when compiling.  The argument count generated in the icode
	is always the number of arguments present in the icode;
	i.e. in the compiler the EXTRA args don't count.

	Note that if the routine itself is EXTRA, we must be in PROOF
	code for the call to take place, so the entire call is suppressed.
}
    if formalextra then
	entervmode(proofmode); 		{ enter correct mode for arg }
    if not verifier then		{ if compiler, not verifier }
	if formalextra then nrofparm := nrofparm - 1; { do not count EXTRA}
    insymbol;
    if p <> nil then begin			{ if formal param exists }
      q := p^.itype;	{ type of formal parameter }
      {
	   Actual/Formal Argument Compatibility Checks
      }
      if p^.vkind = formal then
       begin				{ VAR parameter }
	valueexpression;		{ get the expression }

				{ 1.  Must be variable, not expression }
	if gattr.akind <> ref then error(154)
	else if    (gattr.access = subfield)
	        or (gattr.access = absolute) then
	  error( 155 {"actual parameter may not be component of packed structure"} );
				{ 2.  Types must match }
	if q <> gattr.atype  then
	  error( 156 {"type of VAR parameter differs from type of actual"});

				{ 3.  EXTRA status must match }
        if formalextra then want := extravar else want := executablevar;
        if lastvclass <> want then begin	{ if wrong class }
          if formalextra then error(1007{Formal is EXTRA, actual is not})
	                 else error(1008 {Actual is EXTRA, formal is not});
          end;

				{ 4.  Must be writable re module rules }
	if lastaccess <> referenceandchange then { if not fully accessable }
	  error(1004 {Variable is not writable});
	genbyte(9 {REFER})
       end else begin			{ value parameter }
	valueexprconverted(q);	{ get, check, and convert if required }
	end;
      p := p^.next
      end
    else begin			{ too many actuals }
	valueexpression;	{ eat extra actual }
	error(126);	{ nr actuals > nr formals }
	end;
    nrofparm := succ(nrofparm);
    if formalextra then exitvmode;	{ pop EXTRA mode if needed }
   until sym.sy <> comma;
   if sym.sy = rparen then insymbol else error(4)
   end;
  if p <> nil then error(126);	{ nr formals > nr actuals }
  if fp^.paddr = 0 then error(401);	{ illegal call on main program }
  genbyte(208 + fp^.plev {CALL});
  genbyte(retvsize);
  genbyte(nrofparm);
  genword(fp^.paddr);

  if fp^.itype <> nil then		{ if function call }
  if fp^.itype <> nil then
    if fp^.itype^.form = fixedt then	{generate scaling info for those who follow}
      genscl( 3 {FIX}, fp^.itype );

  if fp^.restrictor <> nil then
    begin	{exported procedure - check the rules and emit LOCK}
      if fp^.restrictor = blkname then
	error( 146  {procedure may not be called within exporting monitor} )
      else if fp^.restrictor^.mprio < prioritycontext then
	error( 150 {cannot call a lower priority routine} )
      else
	begin	{generate a LOCK operator}
	  genbyte( 157 {LOCK} );
	  genbyte( fp^.restrictor^.mprio );  {priority at which to call}
	end;
    end;

  gattr.akind := exp;	{the result of a function call is an expression (not a reference)}
  gattr.atype := fp^.itype
end {calluser};







begin {call}
with fp^ do begin
  if pkind = stnd then begin
    if hasarg in doarg[psinx] then begin
      if sym.sy = lparen then begin
	insymbol;
	if getarg in doarg[psinx] then expression
	end
      else error(9)
      end;
    case psinx of
      0: 				{ pred }
	begin
	  if gattr.atype^.form > longintt then error( 125 {error in std function param});
	  if gattr.akind = cst then
	    begin
	      {  ***  check legality  *** }
	      gattr.avalue.ival := pred(gattr.avalue.ival)
	    end
	  else
	    genbyte(17 {PRED});
	end;
      1:				{ succ }
	begin
	  if gattr.atype^.form > longintt then error( 125 {illegal parameter type} );
	  if gattr.akind = cst then
	    gattr.avalue.ival := succ(gattr.avalue.ival)
	  else
	    genbyte(16 {SUCC});
	end;
      2: begin				{ ord }
	if (gattr.atype <> nil) and (gattr.atype^.form > integert) then
	  error(125);
	if gattr.atype^.form <> integert then
	  begin  {change type to (subrange of) integer}
	    ptem := gattr.atype;
	    talloc( gattr.atype, integert, true );
	    with gattr.atype^ do
	      begin
	    	size := 2;
		form := integert;
		assert(ptem^.form in[scalar,booleant,chart,integert,longintt]);
		maxvalue := ptem^.maxvalue;
		minvalue := ptem^.minvalue
	      end
	  end
	end;
      3: begin				{ chr }
	if not comptypes(gattr.atype, intptr) then error(125);
	gattr.atype := charptr
	end;
      4: begin				{ trunc }
	   if gattr.atype^.form <> fixedt then error(125)
	   else if rmax( abs(gattr.atype^.rlow), abs(gattr.atype^.rhigh) ) > maxfix then
	     error( 207   {fixed point overflow in integer conversion});
	   if gattr.akind = cst then
	    begin
	     gattr.avalue.ival := trunc(gattr.avalue.rval);
	     gattr.avalue.kind := lit
	    end
	   else
	     genbyte(75 {TRUNC});
	   gattr.atype := intptr
	 end;
      5: begin				{ round }
	   if gattr.atype^.form <> fixedt  then error(125 {illegal parameter type})
	   else if rmax( abs(gattr.atype^.rlow), abs(gattr.atype^.rhigh) ) > maxfix then
	     error( 207   {fixed point overflow on conversion to integer});
	   if gattr.akind = cst then
	     begin
	       gattr.avalue.ival:=round(gattr.avalue.rval);
	       gattr.avalue.kind:=lit
	     end
	   else
	     genbyte(76 {ROUND});
	   gattr.atype := intptr
	 end;
      6, 7: begin			{ max - min }
	lattr := gattr;
	if sym.sy = comma then insymbol else error(20);
	expression;
	if psinx = 6 then binop(maxop, lattr) else binop(minop, lattr)
	end;
8, 9: error(notyetimpl);		{ceil , floor}
      10:				{ abs }
	if gattr.atype <> nil then
	  if comptypes(gattr.atype, intptr) then
	    if gattr.akind = cst then
	      gattr.avalue.ival := abs(gattr.avalue.ival)
	    else
	      genbyte(41 {IABS})
	  else if gattr.atype^.form = fixedt then
	    if gattr.akind = cst then
	      gattr.avalue.rval := abs(gattr.avalue.rval)
	    else
	      genbyte(54 {SABS})
	  else error(125);
      11: error(notyetimpl);		{ sqr }
      12:				{ sqrt }
        begin
	  error( notyetimpl );    {*** temp *}
	  if not comptypes(gattr.atype, realptr) then error(125);
	  genbyte(138 {INVOK});
	  genbyte(1   {nr of args});
	  genword(psinx + 68);
	  gattr.atype := realptr
	end;
      13:				{ wait }
        begin
	  if not comptypes(gattr.atype,signalptr) then error(144 {"type of expression mus be signal"});
	  if (top > 1 ) and ( display[top].scope = blck ) then
	    error( 145  {WAIT may not appear in procedure} );
	  genbyte( 9 {refer} );		{WAIT and SEND use VAR parameters}
	  genbyte(154  {WAIT} )
	end;
      14:				{ send }
	begin
	  if not comptypes(gattr.atype,signalptr) then error(144 {"expression type must be signal"} );
	  genbyte( 9 {REFER} );		{WAIT and SEND use VAR parameters}
	  genbyte(155  {SEND} )
	end;
      15:				{awaited}
	begin
	  if not comptypes(gattr.atype,signalptr) then error(144 {"expression is not type signal"} );
	  genbyte(156 {TSIG} );
	  gattr.atype := boolptr	{result is boolean}
        end;
      16:				{ odd }
	if gattr.atype^.form =  integert then
	 begin
	  if gattr.akind = cst then
	    gattr.avalue.ival := ord(odd(gattr.avalue.ival))
	  else
	    genbyte( 42  {IODD} );
	  gattr.atype := boolptr	{change result type to boolean}
	 end
	else
	  error( 125  {"illegal parameter type for std function"} );

      17:				{ enable }
	begin
	  if not comptypes( gattr.atype, signalptr ) then
	    error( 144 {"type of expression must be signal"} );
	  genbyte( 9 {REFER} );
	  genbyte( 158 {ENABL} )
	end;

      18: 				{ defined }
	begin
	{
		Permissible forms are

		defined(<selector>)
		defined(<modulename>)
		defined(<arrayselector>,<subscriptbound>,<subscriptbound>);
	}
					{ we have only insymbol, not expr }
	  if not (vmode in [assertmode, exitmode, entrymode, effectmode,
	    invariantmode]) then 	{ if not in an assertion }
	    error(1039 {defined not permitted outside an assertion});

	  pid := nil;			{ assume no find }
	  if sym.sy = ident then begin	{ begin check for module }
					{ look up ident }
	    pid := searchid([types,konst,vars,modul,field,proc]);
	    if (pid <> udptrs[types]) and { if ident found }
	      (pid^.klass = modul) then begin { if found as module }
	      genbyte( 246 {VINIT} );	{ initialization test for module }
	      genbyte(pid^.maddr);	{ number of module }
	      insymbol;			{ use up module name }
	      gattr.akind := exp;	{ consider this an expr }
	      if sym.sy = rparen then insymbol else error(4); { expect ) }
	    end else begin		{ if non-module }
	      pid := nil;		{ force expression scan }
	      end;			{ end non-module ident }
	    end;			{ end ident }
	  if pid = nil then begin	{ if not found as module }
	    expression;			{ parse expression }
	    if gattr.akind <> ref then 	{ if not accessable by reference }
	      error(154 {actual parameter must be a variable});
	    if sym.sy = rparen then begin { if one-arg form }
	        genbyte( 249 {DEFND} );	{ defined operator }
		genword(gattr.atype^.size); { size of object }
		insymbol;		{ eat ) }
	    end else if sym.sy = comma then begin { 3-arg form }
					{ need low and high bounds }
		lattr := gattr;		{ save elt type }
		subsctyp := gattr.atype^.inxtyp;{ save array type }
					{ if not an array }
      		if gattr.atype^.form <> arrayt then begin
		    error(138 {type of variable is not array});
		    subsctyp := notypeptr;	{ use dummy val }
        	    end;		{ end error recovery }
		insymbol;		{ eat comma }
		valueexprconverted(subsctyp);
		if sym.sy = comma then begin { for last arg }
		    insymbol; 		{ eat comma }
		    valueexprconverted(subsctyp);
		    if sym.sy = rparen then begin { if proper 3-arg }
			insymbol;	{ eat ) }
		        genbyte(244 {DEFAR}); { defined-array operator }
			genword(lattr.atype^.size); { size in bytes }
		    end else error(4);	{ expected ) }
		end else error(20);	{ expected , }
	    end else error(4);		{ not , or ) }
	    end;			{ end not found as module }
	  gattr.atype := boolptr;	{change result type to boolean}
	end
      end {case};
    if getarg in doarg[psinx] then begin
      if sym.sy = rparen then insymbol
      else error(4)
      end
    end {pkind = stnd}
  else
    calluser(fp)
  end {with}
end {call};
