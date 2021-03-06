procedure procdecl (procextra: boolean;  isfunction: boolean);
var
  p1, p2, p3, p4: itp;
  oldfwptr, oldvarlst: itp;
  oldac, olddc, oldlc, oldpin: longint;
  rvsize: longint;
  oldlevel, oldtop: disprange;
  wasforward, isexported: boolean;
  ptype: unittype;


procedure parmlist(var fp: itp);
var
  p1, p2, p3, p4: itp;
  q: stp;
  lvkind: varkinds;
  done: boolean;	{ loop control }
  argextra: boolean;			{ current arg set is EXTRA flag }
begin {parmlist}
p1 := nil;
if sym.sy = lparen then begin
  insymbol;	{ gobble up '(' }
  repeat
    if sym.sy = varsy then begin
      insymbol; lvkind := formal
      end
    else lvkind := param;
    p2 := nil;
    repeat
      if sym.sy = ident then begin
	newid(vars,nil,p2,p4);
	with p4^ do begin
	  vkind := lvkind;  vlev := level
	  end;
	p2 := p4;
	insymbol
	end
      else error(2);
      if sym.sy = comma then
	begin  done := false; insymbol  end
      else  done := true
    until done;
    if sym.sy = colon then begin
      insymbol;
      argextra := false;		{ assume not EXTRA }
      if sym.sy = extrasy then begin	{ if EXTRA formal argument }
	if procextra then error(1009 {EXTRA within EXTRA block});
	argextra := true;		{ mark as extra }
	insymbol;			{ advance to type }
	end;
      typ(q);	{ parse type }
      p3 := p2;
      while p2 <> nil do begin	{ assign type }
	with p2^ do begin	{ using identifier item }
	    assert(klass = vars); { was just created }
 	    itype := q;		{ associate type }
	    if procextra or argextra then begin { if extra mode }
		vclass := extravar;	{ is extra formal arg }	
	    end else begin		{ if not }
		vclass := executablevar;{ otherwise normal }	
		end;
	    end;
        p4 := p2;  p2 := p2^.next
        end;
      p4^.next := p1;  p1 := p3
      end
    else error(5);
    if sym.sy = semicolon then
      begin  done := false;  insymbol  end
    else  done := true
  until done;
  if sym.sy = rparen then insymbol else error(4)
  end;
fp := p1
end {parmlist};


procedure parmaddr(fp: itp);
begin {parmaddr}
with fp^ do begin
  assert(klass = vars);		{ must be var }
  if (not verifier) and (vclass = extravar) then begin { if EXTRA in compl }
      vaddr := illegaladdress;	{ purely for internal debug }
  end else begin		{ not dummy, will assign address }
      if odd(ac) then ac := succ(ac);
      vaddr := ac;
      if vkind = formal then
        ac := ac + usize[pointer]
      else
        ac := ac + typsize(itype)
      end
  end;
end {parmaddr};


begin {procdecl}
			{ preserve state of current procedure }
			{*************************************}
oldfwptr := fwptr;  oldvarlst := varlst;
oldlc := lc;  lc := 0;
oldac := ac;  ac := 0;
{ olddc := dc;  dc := 0; ***anticipating version 3 int code*** }
oldpin := pin;
p4 := nil;  p2 := nil;  p3 := nil;
wasforward := false;		{ assume its not until shown otherwise }
isexported := false;		{  .. assume this also.   }

			{ parse procedure name }
			{**********************}
if sym.sy = ident then
 begin
    p4 := searchlevel(display[top].fname);
    if p4 <> nil then
     begin	{ check to see if previous declared forward }
      if (p4^.klass = proc) and (p4^.pkind = forw) then wasforward := true
      else error(160)		{ ident has already been used }
     end;
    if not wasforward then
     begin	{ enter ident }
      newid(proc,nil,nil,p4);
      if procextra then p4^.pvclass := extravar { record if EXTRA proc }
	else p4^.pvclass := executablevar;
      p4^.restrictor := nil;  {will point to Monitor exporting this proc}
      if exporter(p4) and (blkname^.exclusion = montyp) then
	begin	{ This procedure is exported from a monitor }
	  p4^.restrictor := blkname;
	  isexported := true
	end;
      maxpin := succ(maxpin);  pin := maxpin;
      with p4^ do
	begin
	  plev := level;  paddr := pin
	end
     end
    else
     begin	{ must restore parameter list to p2 }
      if procextra then error(1021 {EXTRA illegal at FORWARD resolution});
      procextra := p4^.pvclass = extravar;	{ extra status comes from original decl }
      p1 := p4^.next;
      if (p4^.itype <> nil) then
        p2 := p1^.next	{ skip over dummy variable for returned value }
      else
        p2 := p1
     end;
   insymbol		{ gobble up the ident }
 end
else
  begin
    error(2  {"expected to see an identifier"} );
    p4 := udptrs[proc]
  end;

			{ increment lex level & plant new identifier tree }
			{*************************************************}
oldlevel := level;
if level < maxlevel then level := succ(level) else error(251);
with blockstack[level] do begin		{ vars at this level belong }
    blockpin := pin;			{ to this pin }
    end;
oldtop := top;
if top < maxdis then begin
  top := succ(top);
  with display[top] do begin
    scope := blck;  fname := p2
    end
  end
else error(250);

			{ parse parameter list and result type }
			{**************************************}
if wasforward then begin	{ param list and func type already avaiable }
  p3 := p4^.next;  p1 := nil;
  while p3 <> nil do begin	{ reverse links }
    p2 := p3^.next;
    p3^.next := p1;
    p1 := p3;  p3 := p2
    end
  end
else parmlist(p1);		{ must parse paramter list and function type }
p3 := nil;
while p1 <> nil do begin	{ reverse order and assign addresses }
  p2 := p1^.next;  p1^.next := p3;
  parmaddr(p1);
  p3 := p1;  p1 := p2
  end;
if not wasforward then begin
  p4^.next := p3;		{ in a proc, next points to parameter list }
  p4^.itype := nil;		{ assume its not a function }
  if isfunction then begin	{ parse function type }
    if sym.sy = colon then begin
      insymbol;		{ gobble up the colon }
      typ(p4^.itype)	{ parse the function type }
      end
    else error(5);
    if p4^.itype <> nil then begin
      if p4^.itype^.form > pointer then begin
	error(120);
	p4^.itype := nil
	end;
      id.l := 3; id.s := '.rv            ';  { enter dummy ident for returned value }
      newid(vars, p4^.itype, p3, p2);
      with p2^ do begin
	vkind := param;  vlev := level;
	vclass := executablevar;{ assume normal }
	if procextra then vclass := extravar; { but extra if proc is }
	end;
      p4^.next := p2;
      parmaddr(p2)
      end
    else error(123)
    end
  end;
if sym.sy = semicolon then insymbol else error(14);

			{ parse the body of the procedure }
			{*********************************}
if sym.sy = forwardsy then
  begin
    if wasforward then error(161)
    else if isexported then error(180 {exported procedures may not be forward} )
    else p4^.pkind := forw;
    insymbol	{ gobble up 'forward' }
  end
else if sym.sy = externalsy then begin
  if procextra then error(1023 {EXTRA routine may not be EXTERNAL});
  p4^.pkind := extn;
  genbyte(6 {proc});  genbyte(1 {extern});  genbyte(1 {ptype=proc});
  genbyte(5 {ident}); genid( p4^.name );
  genbyte(7 {end});   genbyte( p4^.paddr {proc nr});
  rvsize := typsize( p4^.itype );
  genbyte( rvsize );  genword( 0 );  genword( 0 );
  genword( dc );
  insymbol	{ gobble up 'extern' }
  end
else begin	{ parse procedure definition }
  if procextra then entervmode(proofmode);	{ beginning of PROOF code }
  p4^.pkind := decl;
  genbyte(6 {proc});  genbyte(0 {not extern});
  if p4^.restrictor <> nil then  {exported procedure }
    ptype := xproctyp	{exported procedure}
  else			{ vanilla procedure }
    ptype := proctyp;	{vanilla procedure}
  genbyte( ord(ptype) );	{ proc = 1 ; export proc = 2 }
  if isfunction then block(p4,proctyp)	{ parse function block }
		else block(p4,proctyp);  { parse procedure block }
  if procextra then exitvmode;			{ end of PROOF code }
  end;
if sym.sy = semicolon then insymbol else error(14);
level := oldlevel;  top := oldtop;
lc := oldlc;  ac := oldac;  {dc := olddc;}
pin := oldpin;
fwptr := oldfwptr;  varlst := oldvarlst;
assert(display[level].scope = blck); 	{ insure valid var declaring scope }
end {procdecl};



procedure moddecl( ismonitor, standalone : boolean );
  	 {-------		  parse a module declaration }
				{****************************}
		{ is_monitor is TRUE if this is a monitor rather than
		  a module.  stand_alone is true if this is an
		  externally compiled module.  Both parameters cannot
		  have the value TRUE on the same call 		}

const exprt = true;  imprt = false;

  var p: itp;
      wasforward: boolean;   	{true if forward decl exists for module}
      exl, iml: chain;	{pointers to export and import lists resp.}
      erlist, rx: chain; 	{ points to list of undeclared exports}
      badlist: chain;		{ list of exports misused in extrn mod }
      saveid: idtype; 	{for export undeclared check}
      oldtop: disprange;
      oldfwptr: itp;
      oldpin: longint;
      oldprio: 0 .. maxprio;	{priority of containing context}
      ptype: unittype;

  function getportlist( isexport: boolean): chain;
	    {parse the identifiers on the module's export list}
    var  p: itp;  c, c1: chain;
	 endoflist: boolean;

    begin
      c := nil;
      repeat
     	if sym.sy = ident then
	  begin {record it}
	    if isexport then
	      begin	{ declare an export shell }
		newid( xports, nil, nil, p );
		p^.enclosedident := nil	{ initialize }
	      end
	    else 	{we're doing an import list}
	      begin
		{
		    The Verifier needs to allow the importation of monitor
		    and module names so that one can make assertions about
		    a monitor/module being defined.  This requires that
		    the Verifier check for attempts to init an imported
		    monitor or module.
		}
		if not isexport then   		{ if import in verifier }
		    p := searchid([konst,types,vars,proc,modul])
		else				{ if compiler }
		    p := searchid([konst,types,vars,proc]);
			{look it up. Note that this allows the importation
			 of imported variables! 	}
				{ monitor scope enforcement for verifier }
		if ismonitor then
		    monitorscopechk(p, false);	
	      end;
	    new(c1);		{new link for chain}
	    c1^.next := c;	{link to previous}
	    c1^.this := p;	{may be undefined (UDPTR) for import list}
	    c := c1;

	    insymbol;		{eat this identifier}
	  end
	else error (2 {"expected identifier"} );
	if sym.sy = comma then
	  begin  endoflist := false;  insymbol  end
	else  endoflist := true
      until endoflist;

      getportlist := c;
      if sym.sy = semicolon then insymbol else error(14 {"expected semicolon"} )
    end;  {get__port_list}

begin  {moddecl}
 		{preserve certain status information}
  oldpin := pin;  oldfwptr := fwptr;
  oldprio := prioritycontext;	{save the priority outside the monitor}

  if ismonitor  and ((level <> 1) or standalone) then
    error (178  {"monitor must be declared on outer level"});

  if sym.sy = ident then
    begin		{is it declared on this level?}
      p := searchlevel(display[top].fname);
      if p <> nil then
	begin		{it's either forward or an error}
	  if (p^.klass=modul) and (p^.mkind = forw) then
	    wasforward := true
	  else
	    error(101  {"identifier previously declared"} )
	end
      else
	begin		{it's a brand new module}
	  newid(modul,nil,nil,p);
	  with p^ do
	    begin
	      mlev := level;
	      maxpin := succ(maxpin);  pin := maxpin;
	      maddr := pin;
	      if ismonitor then exclusion:=montyp else exclusion:=modtyp;
	      mprio := 0;	{default priority - for clean response to error}
	    end
	end;
      insymbol;		{eat the module name for breakfast}
    end
  else
   begin
    p := udptrs[modul];
    error (2 {"identifier expected"});
   end;

  if ismonitor then	{attempt to parse priority}
    if sym.sy = prioritysy then
      begin		{check priority type and range}
	insymbol;	{eat the keyword}
	expression;		{parse priority value}
	if gattr.akind <> cst then  error( 106 {constant expected})
	else if gattr.avalue.kind <> lit then error( 15  {longint expected})
	else if (gattr.avalue.ival<0) or (gattr.avalue.ival>maxprio) then
	  error( 172 {invalid priority level})
	else
	  begin		{set up priority}
	    p^.mprio := gattr.avalue.ival;
	    prioritycontext := p^.mprio		{priority at which we compile (needed by calluser) }
	  end
      end
    else
      error( 171  {"expected priority"});

  if sym.sy = semicolon then
    insymbol		{eat the semicolon for lunch}
  else
    error (14 {"expected semicolon"});

					{get export & import lists}
					{*************************}
  if sym.sy = exportsy then
    begin
      insymbol;		{eat the keyword for dinner}
      exl := getportlist(exprt);   	{.. and the export list for dessert}
    end
  else  exl := nil;

  if sym.sy = importsy then
    begin
      insymbol;			{Burp!}
      iml := getportlist(imprt);
    end
  else iml := nil;

				{.. now prepare to plant another tree
				    in the identifier forest         }
  oldtop := top;	{remember current display location}
  if top < maxdis then
    begin
      top := succ(top);
      with display[top] do
        begin
	  fname := nil;		{ident tree is empty so far}
	  scope := modyul;
	  exportlist := exl;
	  importlist := iml
	end
    end;

			{also attach port lists to ident in outer tree}
  p^.exportlist := exl;
  p^.importlist := iml;

					{now parse the body (or whatever)}
					{********************************}
  if ismonitor then  ptype := montyp  else ptype := modtyp;
  if sym.sy = forwardsy then
    begin	{the real module will be declared later}
      error(1053 {FORWARD unimplemented in verifier}); {***UNIMPLEMENTED***}
      if wasforward then
	error (161 {"already declared forward"} )
      else
	p^.mkind := forw
    end
  else if sym.sy=externalsy then
    begin
      error(1054 {EXTERN unimplemented in verifier}); {***UNIMPLEMENTED***}
      genbyte (4 {MONIT})  ;
      genbyte (1 {loadtype=extern});
      if ismonitor then
	genbyte(4 {ptype=monitor}  +  16*p^.mprio  )
      else genbyte(3 {ptype=module});
      genbyte (5 {IDENT});  genid(p^.name);
      genbyte (7 {END});  genbyte(p^.maddr);

      insymbol;		{eat the "extern"}
    end
  else
    begin        	{ parse module definition }
      genbyte (4 {MONIT})  ;
      genbyte (0 {not extern});
      if ismonitor then
	genbyte(4 {ptype=monitor}  +  16*p^.mprio  )
      else genbyte(3 {ptype=module});
      block(p,ptype);
    end;

		{ check export list to be sure that all identifiers on it
		  were declared within the module  }
  saveid := id;
  erlist := nil;
  badlist := nil;
  while exl <> nil do
    begin
      with exl^.this^ do
	begin
	  id := name^;
	  p := searchlevel( display[top].fname );
	  if p = nil then
	    begin
	      new(rx);
	      rx^.this := exl^.this;
	      rx^.next := erlist;
	      erlist := rx
	    end
	  else if standalone and (p^.klass <> proc) then
	    begin	{ ident is illegally exported in this context }
	      new(rx);
	      rx^.this := exl^.this;
	      rx^.next := badlist;  { link list to this id }
	      badlist := rx
	    end;
	end;	{with}
      exl := exl^.next;		{move to next ident}
    end;	{while exl <> NIL}

  if erlist <> nil then
    begin
      printlist( erlist, 112 {exported ident undeclared on inner level}  );
    end;

  if badlist <> nil then
    begin
      printlist( badlist, 114 {only procedures/functions may be exported here});
    end;

  id := saveid;		{restore input symbol}


  if sym.sy = semicolon then insymbol  else error(14 {"expected semicolon"} );
			{ restore the state of the containing scope}
  top := oldtop;
  prioritycontext := oldprio;
  pin := oldpin; fwptr := oldfwptr

end {moddecl};


