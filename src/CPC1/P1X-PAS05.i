				{ symbol table manipulation routines }
				{************************************}

function searchlevel(fp: itp): itp;
var
  p: itp;
  i: integer;
  found: boolean;
begin {searchlevel}
p := fp;
found := false;
while not found and (p <> nil) do with p^.name^ do begin
  i := match(s, l, id.s, id.l);
  if i = 0 then
    found := true
  else if i > 0
    then p := p^.llink
    else p := p^.rlink
  end;
searchlevel := p
end {searchlevel};


function searchid ( fs: setclass ) : itp;
	{--------  			** identifier  monkey  ** }
  var p: itp;  il: chain;  m, errf: integer;
	
	{This function searches trees in the identifier forest for the
	 ident in the global variable "id".  If id is not found, the routine
	 jumps to the tree of the enclosing scope and searches
	 that tree.  However, if the current tree represents a monitor/module,
	 the routine is not allowed to jump to a new tree, but must
	 slide down the import chain if one exists.   }

	{Any identifier found by the identifier monkey must be of a
	 class contained in the parameter FS unless it is XPORTS class.
	 In this case, the monkey retrieves the enclosed identifier
	 and compares its class with FS.		}

	{Note that the tree containing the enclosed identifier is still
	 in the forest, but that the monkey cannot climb it since it only
	 climbs trees starting at display[].fname.  In other words,
	 the only identifiers which remain accessible on this tree are
	 those which have been exported.}

  begin		{searchid}
    disx := top;  errf := 0;  foreign := false;
    repeat	{ each cycle searches one tree }
      p := searchlevel(display[disx].fname);
      if p <> nil then
	begin   { found a lexical match on this level }
	  if  not(p^.klass in fs)  then
	    if p^.klass = xports then
	      begin  {examine enclosed identifier}
		p := p^.enclosedident;
		foreign := true;
	    	if p = nil then
		  begin		{ ident was exported but not declared }
		    errf := 109;
		    p := udptrs[vars]
		  end
	  	else if not (p^.klass in fs)  then
		  errf := 103
	      end
	    else
	      begin  { not exported .. hence, wrong class error }
	  	errf := 103;
		p := nil
	      end
	end	{ of lexical match found }
      else
       begin  {no match at this level}
        if  (disx = 0) and (p = nil)  then
	  errf := 104
        else if display[disx].scope <> modyul then
	  disx := pred(disx)	{jump to outer level tree}
        else
	  begin  {jump to outer tree NOT allowed; search import list, if any}
	    il := display[disx].importlist;
	    while il <> nil do with il^.this^.name^ do
	      begin
	        m := match(s, l, id.s, id.l );
	        if m=0 then
		  begin p:=il^.this; foreign := true; il:=nil end
	        else
		  il := il^.next
	      end;
	    if p = nil  then disx := 0   {if we haven't got it by now, it's
					  either a built-in identifier or undeclared.}
	  end	{import list search}
       end  {no match}

    until  (p <> nil)  or  (errf <> 0);

    if (prterr) and (errf<>0) then
      begin
	error(errf);
	p := udptrs[firstclass(fs)]
      end;
    searchid := p
  end;	{searchid}



procedure newid(fc: classes; fq: stp; fn: itp; var fp: itp);
var
  p, p1, p2: itp;
  i: integer;
  lleft: boolean;
begin {newid}
case fc of
  { Free Pascal does not support the variant form of new }
  types: begin new(p); p^.klass := types; end;
  konst: begin new(p); p^.klass := konst; end;
  vars:  begin new(p); p^.klass := vars; end;
  field: begin new(p); p^.klass := field; end;
  proc:  begin new(p); p^.klass := proc; end;
  modul: begin new(p); p^.klass := modul; end;
  xports:new(p)   {allocate a record with unspecified class - variant filled in later}
end;
if verifier then stampid(p);		{ stamp with source line number }
with p^ do
 begin
  idserial := idserial+1;	{make a new serial number}
  iserial  := idserial;		{and assign it to this identifier}
  klass := fc;
  new(name);				{ REMOVED variable-size dynamic array }
  with name^ do begin
    l := id.l;
    for i := 1 to ord(id.l) do s[i] := id.s[i]
    end;
  llink := nil; rlink := nil;
  itype := fq; next := fn
  end;
p2 := display[top].fname;
if p2 = nil then display[top].fname := p
else begin
  repeat
    p1 := p2;
    with p2^.name^ do begin
      i := match(s, l, id.s, id.l);
      if i <= 0 then begin
	if i = 0 then error(101);
	p2 := p2^.rlink;
	lleft := false
	end
      else begin
	p2 := p2^.llink;
	lleft := true
	end
      end
  until p2 = nil;
  if lleft then p1^.llink := p else p1^.rlink := p
  end;
fp := p
end {newid};


function  exporter (candidate: itp ): boolean;
	 {--------	
			This function  links an exported
			identifier with its declaration in the enclosed
			scope.  It is called each time an identifier of
			exportable class is declared.  If the current scope
			is a monitor or module, it searches the export
			list for a match with the identifier being declared.
			If a match is found, the "enclosed_ident"
			field of the matched identifier is made to point
			to the identifier being declared.              }
  var p: chain;  m: integer;

  begin
    exporter := false;	{initial assumption is not exported}
    if display[top].scope = modyul then
      begin  { this is a module, so check the export list}
      	p := display[top].exportlist;
	m := 1;
	while (p <> nil) and (m <> 0) do
	  begin	{match the candidate against items on export list}
	    m := match( candidate^.name^.s,candidate^.name^.l,
			p^.this^.name^.s, p^.this^.name^.l  );
	    if  m<>0 then  p := p^.next
	  end;
	if p<>nil then
	  begin		{p points to exprt lst link which points to outer id }
	    exporter := true;	{ it IS exported }
	    p^.this^.enclosedident := candidate
	  end

      end;  {of case that scope is a module}
  end;   {exporter }

	{***********************************************************
			SYMBOL  TABLE  ROUTINES
	 ***********************************************************

	   The following routines are used to output the symbol table.
	It is written as an ascii file with one record (line) for each
	identifier in the program and one line for each type.  Identifier
	and type records are distinguished by having "I" and "T" as the
	as the first field of the record.  Fields  within a record are
	separated by one or more blanks.  Both identifiers and ytpes are
	serialized by the compiler and the second field of each type of
	record is the serial number for the identifier or type being
	defined.
	**************************************************************
	*************************************************************}

procedure formatflit( x: valu; var m,s: integer);  forward;


procedure printyp( t:stp );
	 {-------			}
	 {output a type definition to the symbol table, including all of
	  its subtypes which have not been printed.	}
  var tman, tskal: integer;		{temps for fixed point output}
  begin
    if t <> nil then with t^ do
      if not marked then
	begin		{ this type has not yet been output }
	  marked := true;	{ .. we've seen it now }
			{ first print any unmarked subtypes}
	  case form of
	    sett: printyp( settyp );
	    arrayt: begin
			printyp( aeltyp ); printyp( inxtyp );
		    end;
	    tagfield: begin
			printyp( fstvar ); printyp( tagtyp );
		      end;
	    variant:  begin
			printyp( subvar );  printyp( nxtvar );
		      end;
	    scalar,booleant,chart,integert,longintt,xcptnt,
	    fixedt,signalt,recordt,devicet:

  	  end;
			{ now print the definition for THIS type}
	  write( symfil, 'T ',tserial:5,' ',size:5,' ',ord(form):2,' ');
	  case form of
	    scalar,booleant,chart,integert,longintt:
	      begin
		if form <> scalar then
		  write(symfil, 0:5)	{ maxconst is undefined }
		else if maxconst <> nil then
		  write(symfil,maxconst^.iserial:5)
		else
		  write(symfil,0:5);
		write (symfil,' ',1:1);	{ all numbers are subranges }
		  write(symfil,' ',maxvalue:6,' ',minvalue:6);
	      end;
	    fixedt:
	      begin
		{
		format_flit(rlow,tman,tskal);
		write(symfil,tskal:3,' ',tman:6);
		format_flit(rhigh,tman,tskal);
		write(symfil,tskal:3,' ',tman:6);
		format_flit(precsn,tman,tskal);
		write(symfil,tskal:3,' ',tman:6);
		}
		write(symfil,rlow,rhigh,precsn);
	      end;
	    signalt:
	      begin
		write(symfil,ord(addresspresent):1);
		if addresspresent then
		  write(symfil,' ',trapvec:5);
	      end;
	    sett:
	      if settyp = nil then
		write(symfil, 0:5)
	      else
	        write(symfil, settyp^.tserial:5);
	    arrayt:
	      write(symfil, aeltyp^.tserial:5,' ',inxtyp^.tserial:5);
	    recordt:
	      if fstfld = nil then
		write(symfil, 0:5)
	      else
		write(symfil,fstfld^.iserial:5);
	    devicet:
	      begin
		if fstdfld = nil then
		  write(symfil, 0:5)
		else
		  write( symfil, fstdfld^.iserial:5);
		write(symfil,' ',ord(addressed));
		if addressed then
		  write(symfil,' ',devaddr:5);
	      end;
	    tagfield:
	      begin
		if fstvar = nil then write(symfil,0:5,' ')
		else write(symfil,fstvar^.tserial:5,' ');
		if tagfld = nil then write( symfil,0:5,' ')
		else write(symfil,tagfld^.iserial:5,' ');
		if tagtyp = nil then write(symfil,0:5)
		else write(symfil,tagtyp^.tserial:5);
	      end;
	    variant:
	      begin
		write(symfil,varval:5,' ');       {tag value for this vrnt}
		if nxtvar = nil then write(symfil,0:5,' ')
		else write(symfil,nxtvar^.tserial:5,' ');
		if subvar = nil then write(symfil,0:5)
		else write(symfil,subvar^.tserial:5);
	      end
	  end;	{ case form }
	  writeln(symfil);
	end	{ not marked }
  end;	{ Printyp }


procedure printident( idtree: itp );
	 {----------		This procedure prints out a block
			of identifiers 		}

  const sfw = 4;	{field width for serial numbers of types}
  var iman,iskal: integer;	{temps for fix pt mantissa, scale}
  begin
    if idtree <> nil then
      with idtree^ do
	begin  { print left subtree, then this node, then right subtree }
	  printident( llink );

	  		{ output a type record for this ident's type }
	  printyp( itype );

	  write(symfil,'I ',iserial:5,' ');	{ident rcrd + name of this id}
	  write(symfil, name^.s:name^.l);
	  if name^.l < 15 then write(symfil,' ':15-name^.l);
	  if itype = nil then write(symfil,0:5,' ')
	  else write(symfil,itype^.tserial:5,' ');
	  if next = nil then write(symfil,0:5,' ')
	  else write(symfil,next^.iserial:5,' ');
	  write( symfil, ord( klass):2 );
	  case klass of
	    types: ;
	    vars:  write( symfil,' ',ord(vkind):2,
			  ' ', vlev:2,
			  ' ', vaddr:5);
	    konst: with kvalue do
		    begin
		      write( symfil, ord(kind):2 );
		      case kind of
			lit:  write(symfil, ' ', ival:6);
			data: write(symfil, ' ', daddr:5);
			reel: begin
				formatflit(kvalue, iman, iskal );
				write(symfil,' ',iskal:3,' ',iman:6);
			      end;
			setc: write(symfil, '***NYI***');
		      end;
		    end;
	    field: begin
		    write(symfil,' ',ispacked:1,' ');
		    if ispacked then write(symfil,bdisp:5)
		    else write(symfil, fdisp:5)
		   end;
	    proc:  begin
		    if restrictor = nil then write(symfil,' ',0:5)
		    else write(symfil,' ',restrictor^.iserial:5);
		    write(symfil,' ',ord(pkind):1,' ');
		    case pkind of
			decl: write(symfil,plev:2,' ',paddr:5);
			stnd: write(symfil,psinx:3);
			extn: write(symfil,pxinx:5);
		    end
		   end;
	    modul: begin
		    if exportlist = nil then write(symfil,' ',0:5)
		    else write(symfil,' ',exportlist^.this^.iserial:5);
		    if importlist = nil then write(symfil,' ',0:5)
		    else write(symfil,' ',importlist^.this^.iserial:5);
		    write(symfil,' ',ord(exclusion):1,' ',mprio:2,' ');
		    if mkind = decl then
			write(symfil, mlev:2,' ',maddr:5)
		    else
			write(symfil, mxinx:5)
		   end;
	    xports:begin
		    write(symfil,' ',enclosedident^.iserial:5,' ');
		    write(symfil,ord(fromclass):3)
		   end;
	  end;	{ .. of case class }
	  writeln(symfil);


	  printident( rlink );

	end;
  end;	{ PrintIdent }


{
	printtables  --  print variable and type tables

	This is a a debugging aid for the compiler.
}
procedure printtables (fb: boolean);
var
  i,lim: disprange;

procedure markctp (fp: itp); forward;

{
	markstp  --  mark type nodes

	The object of marking is to cause each node to be printed
	only once.
}
procedure markstp (fp: stp);
  begin {markstp}
    if  fp <> nil then with fp^ do begin
      writeln(lst,'s:',ord(fp):8); break(lst); {lst}
      marked := true;
				{ CASE with OTHERS converted to IF statements }
      if form = sett then
        markstp(settyp)
      else if form = arrayt then
        begin markstp(aeltyp); markstp(inxtyp) end
      else if form = recordt then
        begin markctp(fstfld); markstp(recvar) end
      else if form = tagfield then
        markstp(fstvar)
      else if form = variant then
        begin markstp(nxtvar); markstp(subvar) end
      else if form = devicet then	{ device type }
	begin markctp(fstdfld); markstp(devvar); end
      else if (form = scalar)
	   or (form = booleant)
	   or (form = chart)
	   or (form = integert)
	   or (form = longintt)
           or (form = fixedt)
	   or (form = signalt) then
	begin { no subitems } end
      else 			{ OTHERS case }
        writeln(lst,'**internal error in markstp,  form= '
            ,ord(form),linenr:8);
      end {with}
  end {markstp};
{
	markctp  --  mark identifier nodes
}

procedure markctp(fp: itp);
begin {markctp}
  if  fp <> nil then with fp^ do begin
    writeln(lst,'c:',ord(fp):8,ord(llink):8,ord(rlink):8,ord(itype):8); break(lst); {lst}
    markctp(llink);
    markctp(rlink);
    markstp(itype)
    end {with}
end {markctp};
{
	followctp  --  print definition of identifier
}

procedure followctp (fp: itp); forward;

{
	followstp --  print type chain
}
procedure followstp (fp: stp);
begin {followstp}
  if fp <> nil then with fp^ do begin
    if marked then begin
      marked := false;
      case form of
	scalar, booleant, chart, integert, longintt: begin
	  if maxconst <> nil then begin	{ if enumeration type }
		write(lst,' maxconst: ');	{ will print high constant }
		followctp(maxconst);		{ of enumeration def }
		end;
	  write(lst,' ',minvalue:1,'..',maxvalue:1); { print bounds }
	  end;
	fixedt: begin
	  write(lst,' fixed ',rlow:1,'..',rhigh:1,' precision ',precsn:1);
	  end;
	signalt: begin
	  write(lst,' signal');
	  if addresspresent then	{ if interrupt }
	    write(lst,'[',trapvec:1,']'); { print address }
	  end;
	pointer: begin
	  write(lst,' to');
	  followstp(eltype)
	  end;
        sett:begin
          followstp(settyp)
          end;
        arrayt:begin
	  write(lst,' [');
          followstp(inxtyp);
	  write(lst,'] of');
          followstp(aeltyp)
          end;
        recordt:begin
	  writeln(lst,' of');
          followctp(fstfld);
          followstp(recvar);
          end;
	devicet: begin				{ device variable }
	  if addressed then write(lst,' [',devaddr:1,']'); { address }
	  writeln(lst,' of ');			{ similar to record }
	  followctp(fstfld);
	  followstp(devvar);
	  end;
        tagfield:begin
          followstp(fstvar)
          end;
        variant:begin
          followstp(nxtvar);
          followstp(subvar)
          end
	end {case}
      end {if marked}
  end {if <> nil}
end {followstp};

  procedure followctp(fp: itp);
  begin {followctp}
    if  fp <> nil then with fp^ do begin
      followctp(llink);
      write(lst, '  ');
      if name^.l <> 0 then
	write(lst, name^.s:ord(name^.l));
      write(lst,' ':16-ord(name^.l),ord(llink):8,ord(rlink):10,ord(itype):10,ord(next):10,' ':4);
      case klass of
        types:write(lst,'type');
        konst:begin
          write(lst,'constant':13);
          end;
        vars:begin
	  case vkind of
	    formal: write(lst, 'parm by ref':13);
	    param: write(lst, 'parm by val':13);
	    local: write(lst, 'variable':13)
	  end;
	  case vclass of			{ verification class }
	    executablevar: begin end;		{ normal }
	    extravar: write(lst, 'EXTRA ');
	    end;
          write(lst,vlev:6,vaddr:10)
          end;
        field:write(lst,'field':13,fdisp:10);
        proc:begin
          write(lst,'procedure');
          if pkind = extn then write(lst, ' ext')
              else write(lst, '    ')
          end;
	modul: begin
	  write(lst,'module');
	  case exclusion of
	    modtyp: begin write(lst,'module'); end;
	    montyp: begin write(lst,'monitor[',mprio:1,']'); end;
  	    end
	  end;
	xports: begin
	  write(lst,'export');
          end
      end; {case}
      followstp(itype);
      writeln(lst);
      followctp(rlink)
    end {with}
  end {followctp};

begin {printtables}
  writeln(lst); writeln(lst);
  if fb then lim:=0 else begin lim:=top; write(lst,'Local ') end;
  writeln(lst,'Tables');
  for i:=top downto lim do markctp(display[i].fname);
  for i:=top downto lim do followctp(display[i].fname);
  writeln(lst);
  if chcnt <> 0 then write(lst,' ':chcnt+8)
end {printtables};
