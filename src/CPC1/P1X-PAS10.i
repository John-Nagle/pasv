
procedure typ (var fq: stp);	{ parse type definitions }
var
  fldoffset, bitoffset: integer;
  p: itp;
  q, q1, q2 : stp;
  oldtop, n, lmin, lmax: integer;
  packrequest : boolean;
  pw : integer;  	{temporary for adjusting fixed point precision}
  endoflist: boolean;


procedure subrange(var fq: stp);
var
  q, q1: stp;
  lmin: integer;
begin {sub_range}
expression;
if gattr.akind <> cst then error(106);
q1 := gattr.atype;
lmin := gattr.avalue.ival;
if sym.sy = colon then insymbol else error(5);
expression;
if gattr.akind <> cst then error(106);
if gattr.avalue.ival < lmin then error(102);
q := nil;
if (q1 <> nil) and (gattr.atype <> nil) then begin
  if comptypes(q1, gattr.atype) then begin
    if gattr.atype^.form in [scalar, booleant, chart, integert, longintt] then 
    case gattr.atype^.form of
      scalar:
       begin
        talloc(q, scalar, true);
        q^.maxconst := gattr.atype^.maxconst
	  {*****  See func assignable before you monkey with this stmt ***}
       end;
      booleant: talloc(q, booleant, true);
      chart:    talloc(q, chart, true);
      integert: talloc(q, integert, true);
      longintt: talloc(q, longintt, true);
    end else begin			{ illegal type }
      error( 148 {"subrange bounds must be ordinal type"} )
      end;
    if q <> nil then with q^ do begin
      size := typsize(q1);  form := q1^.form;
      maxvalue := gattr.avalue.ival;
      minvalue := lmin
      end
    else error(148)
    end
  else error(107)
  end;
fq := q
end {sub_range};


function checklegal( adr: integer ): boolean;
	 {-----------		check the validity of a device or signal
				address.  At present this only implies
				checking that the value is a valid
				address}
  begin
    if (adr < 0) or (adr > numlimit) then
     begin
      checklegal := false;
      error( 183  {"invalid device or signal address"} )
     end
    else
      checklegal := true;
  end;


procedure fieldlist (var fq: stp; var ffp: itp; packit: boolean );
	 {---------		parse a field list.
			ffp is a pointer to first ident in the fieldlist
			pack_it = true if record is packed (or device)
	 }
var
  p, p1, p2, p3: itp;
  q, q1, q2, q3, q4, q5: stp;
  maxbits, minbits,
  maxsize, minsize: integer;
  lid: idtype;
  needsemi: boolean;    	{used to ensure correct parsing for semicolons}
  continue: boolean;		{used to parse a list}
  extrafield: boolean;		{ true if field is EXTRA }

procedure fieldaddr(fp: itp);
begin {fieldaddr}
if fp <> nil then with fp^ do
 begin
  assert(class = field);			{ must be field }
  assert(ispacked = packit);			{ check rec/state sync }
  if (fvclass <> executablevar) and (not verifier) then
    begin					{ if EXTRA field in compiler }
						{ do not allocate space }
    if packit then bdisp := illegaladdress else fdisp :=illegaladdress;
  end else   	  				{ will assign address }
  if not packit then
    begin	{unpacked field allocation }
      fldoffset := ceil(fldoffset , typalign(itype));
      fdisp := fldoffset;
      fldoffset := fldoffset+typsize(itype)
    end
  else
    begin  {field must be packed}
      bdisp := bitoffset;
      bitoffset := bitoffset + typwidth(itype);
      if ( bdisp div bitswd ) <> ( (bitoffset-1) div bitswd ) then
	error( 23 { field of device or packed record will cross word boundary } );
      if (bitoffset div 8) - fldoffset > bitsau then
	fldoffset := fldoffset+2
      else if (bitoffset div 8)-fldoffset > 0 then
	fldoffset := fldoffset+1;
    end;
 end
end {fieldaddr};


begin {fieldlist}
p3 := nil;
needsemi := false;

while sym.sy = ident do
 begin
  p2 := p3;
  if needsemi then error( 14 {"expected semicolon"});
  repeat
    if sym.sy = ident then
      begin newid(field,nil,p2,p2);
	    if packit then p2^.ispacked :=true  else p2^.ispacked:=false;
	    insymbol
      end
    else error(2);

    if  sym.sy = comma  then
      begin
	continue := true;	{there are more identifiers in the list}
	insymbol		{  .. so get the next one 	}
      end
    else			{its not a comma}
      continue := false

  until not continue;
  if sym.sy = colon then insymbol else error(5);

  extrafield := false;		{ assume not EXTRA }
  if sym.sy = extrasy then begin{ if EXTRA field set }
    extrafield := true;		{ field set will be EXTRA }
    insymbol;			{ advance to begininng of type }
    end;	
  typ(q);		{parse type of fields }

  if q^.form = devicet then error (181 {record component may not be device});
  if packit and (q^.form = arrayt) then
    error( 182 );    {** we can't support arrays as components of packed structures **}
  p := p2;
  while p2 <> p3 do begin
    with p2^ do begin		{ for fields of this field set }	
	itype := q;		{ set field }
	assert(class = field);	{ must be field }
				{ note if EXTRA }
	if extrafield then fvclass := extravar else fvclass := executablevar;
	end;	
    p2^.itype := q;
    p2 := p2^.next
    end;
  p3 := p;
  if sym.sy = semicolon then
    begin
      insymbol;
      needsemi := false
    end
  else
    needsemi := true
 end;	{ while sym.sy = ident }
p2 := nil;
while p3 <> nil do begin	{ reverse links }
  p := p3^.next;
  p3^.next := p2;
  p2 := p3; p3 := p
  end;
p := p2;
ffp := p;	{return pointer to field list}
while p <> nil do begin		{ assign offsets of fields }
  fieldaddr(p);
  p := p^.next
  end;
if sym.sy = casesy then
 begin
  if needsemi then error(14 {" ';' expected"});
  insymbol;
  talloc(q, tagfield, false);
  with q^ do
   begin
    form := tagfield; fstvar := nil; tagfld := nil; tagtyp := nil;
    if sym.sy = ident then
     begin
      lid := id;
      insymbol;
      if sym.sy = colon then
       begin
	newid(field, nil, nil, p);
	if packit then p^.ispacked:=true else p^.ispacked:=false;
	tagfld := p;
	insymbol;
	if sym.sy = ident then
	 begin
	  lid := id;
	  insymbol
	 end
	else error(2)
       end
      else p := nil;
      id := lid;
      p1 := searchid([types]);
      q5 := p1^.itype;
      if q5^.form > longintt then begin error(110); q5 := nil end;
      tagtyp := q5
     end
    else error(2);
    if p <> nil then begin
      p^.itype := q5;
      fieldaddr(p)
      end;
    size := fldoffset		{ min size of variants }
   end;
  if sym.sy = ofsy then insymbol else error(8);
  q1 := nil;
  minsize := fldoffset; maxsize := fldoffset;
  minbits := bitoffset; maxbits := bitoffset;
  repeat		{ parse variants }
			{ Note on list construction:
			   the nxtvar list (q1) will link all
			   variants in this fieldlist.  The subvar
			   field (q2) is used to link those
			   variants declared together
			   (e.g., a & b in ... a,b:(x:integer);
						 c:(y:real);      )
			   Once the fieldlist for a given set of
			   variants is parsed (by calling fieldlist),
			   the field subvar is used to point to any
			   variants if the parsed fieldlist (subvariants)
			}
    q2 := nil;
    repeat
      expression;
      if not comptypes(gattr.atype, q5) then error(111);
      talloc(q3, variant, false);
      with q3^ do begin
	form := variant; nxtvar := q1; subvar := q2;
	varval := gattr.avalue.ival
	end;
      q1 := q3; q2 := q3;
      if sym.sy = comma then
	begin		{more to the list}
	  continue := true;
	  insymbol
	end
      else
	continue := false
    until not continue;
    if sym.sy = colon then insymbol else error(5);
    if sym.sy = lparen then insymbol else error(9);
    fieldlist(q2, p, packit);
			{ assign fieldlist to each variant }
    q4 := q3;
    while q4 <> nil do
      begin  q4^.firstvfld := p;  q4 := q4^.subvar  end;

    if fldoffset > maxsize then maxsize := fldoffset;
    if bitoffset > maxbits then maxbits := bitoffset;
    while q3 <> nil do begin
      q4 := q3^.subvar;
      q3^.subvar := q2;
      q3^.size := fldoffset;
      q3 := q4
      end;
    if sym.sy = rparen then insymbol else error(4);
    if sym.sy = semicolon then
      begin
	insymbol;
	needsemi := false
      end
    else
      needsemi := true;
    bitoffset := minbits;	{reset addressing for next variant}
    fldoffset := minsize
  until  ( sym.sy = endsy ) or needsemi;
  fldoffset := maxsize;
  bitoffset := maxbits;		{allocate for largest variant}
  q^.fstvar := q1;
  fq := q
 end		{ IF sym.sy = casesy  }
else if (q <> nil) and (q^.form = arrayt)
  then fq := q
  else fq := nil
end {fieldlist};


begin {typ}
if sym.sy = packedsy then
  begin
    packrequest := true;  insymbol
  end
else  packrequest := false;
case sym.sy of


ident: begin
  p := searchid([types,konst,proc]);
  if p^.class = types then begin
    q := p^.itype;  insymbol;
    if sym.sy = lbrack then
     if (q^.form <> devicet) and (q^.form <> signalt)  then
	error(6 {"illegal symbol"})	{don't swallow it}
     else   {.. seems ok}
      begin	{ .. a hardware address is being specified}
        insymbol;  	{devour the bracket}
	expression;	{parse the address }
	if (gattr.akind <> cst) or (gattr.atype^.form <> integert ) then
	  error(176 {"illegal format for device address"})
	else
	  if q^.form = devicet then
	    begin  {store away the device address}
	      if q^.addressed then  {warn user}
	      	error(179 {"device address specified twice"} );
	      talloc(q,devicet,false);  {we must allocate a new type because address is different}
	      q^.size := p^.itype^.size;	{.. and remember all we knew about the last one}
	      q^.fstdfld := p^.itype^.fstdfld; q^.devvar := p^.itype^.devvar;
	      if checklegal( gattr.avalue.ival ) then
	        q^.devaddr := gattr.avalue.ival
	      else
		q^.devaddr := 0;  { default value to keep compiler out of trouble }
	      q^.addressed := true
	    end
	  else
	    begin   {it's a signal address}
	      talloc(q,signalt, false);   {allocate a new type for new address}
	      q^.size:=p^.itype^.size;  {remember all we knew about the old}
	      if checklegal( gattr.avalue.ival ) then
	        q^.trapvec := gattr.avalue.ival
	      else
		q^.trapvec := 0;	{keep compiler in line}
	      q^.addresspresent := true
	    end;
	if sym.sy = rbrack then { ..eat it}
	  insymbol
	else
	  error(12 {"expected right bracket"})
      end
    end
  else subrange(q)
  end;

addop, intconst, fixconst, stringconst:
  subrange(q);

lparen: begin
  oldtop := top;
  while  display[top].scope = rcrd  do
    top := pred(top);   { Note: the constants implicitly declared
			  with an anonymous enumerated type
			  in the scope of a record must be available
			  in the containing block		}
  talloc(q, scalar, false );
  with q^ do begin
    size := usize[scalar];  form := scalar;  
    minvalue := 0;			{ minimum value of enumeration }
    maxvalue := 0;			{ initial value of maximum }
    p := nil;  n := 0;
    repeat
      insymbol;	{ gobble up leading '(' or ',' }
      if sym.sy = ident then begin
	newid(konst,q,p,p);
	with p^ do begin
	  kvalue.kind := lit;  kvalue.ival := n
	  end;
	n := succ(n);
	insymbol	{ gobble up ident }
	end
      else error(2)
    until sym.sy <> comma;
    maxconst := p;
    if n>0 then maxvalue := n-1;	{ maximum value of enumeration }
    end;
  top := oldtop;
  if sym.sy = rparen then insymbol else error(4)
  end;

fixedsy:
 begin    			{Fixed point type }
  insymbol;		{gobble up 'fixed'}
  talloc(q,fixedt,false);	{ allocate entry for type entry}
  with q^ do begin

  form := fixedt;
  size := usize[fixedt];	{ size is constant }
  		{now parse range et precision}
  expression;
  with gattr do
    begin    { check low value of range }
      if akind <> cst then  error(106)    {"expected constant"}
      else
        if avalue.kind in [reel,lit] then
	case  avalue.kind of
	  reel: rlow := avalue.rval;
	  lit : rlow := avalue.ival;    { convert integer }
	end else begin			{ if illegal type }
	            error(fxptrange);
		    rlow := minfix  	{a default to stop more errors}
	          end
    end;     {check on low value }
  if sym.sy <> colon then error(fxptsyntx) else insymbol;
  expression;
  with gattr do
    begin    { check high value of range }
      if akind <> cst then error(106)	{"expected constant"}
      else
        if avalue.kind in [reel,lit] then
	case avalue.kind of
	  reel: rhigh := avalue.rval;
	  lit : rhigh := avalue.ival;   	{get a conversion to real}
	end else begin				{ if invalid type }
		    error(fxptrange);
		    rhigh := maxfix  {the largest fixed point value}
		  end
    end; {check on high value}
  if sym.sy <> precisionsy then error(fxptsyntx) else insymbol;
  expression;
  with gattr do
    begin 	{check proffered precision}
      if akind <> cst then error(106)	{"expected constant"}
      else
	if avalue.kind in [reel,lit] then
	case avalue.kind of
	  reel: precsn := avalue.rval;
	  lit : precsn := avalue.ival;
	end else begin
		    error(fxptprcsn);
		    precsn := stdpcn	{ ??? }
		  end;
	if precsn < 0.0 then
	  begin		{ negative precision not allowed }
	    error( fxptprcsn );
	    precsn := abs(precsn)
	  end;
    end;	{ of precision check }
  		{check for consistant range and precision}
  if precsn <= 0.0 then
    begin  {use defaults to avoid induced errors}
      error(fxptprcsn);
      rlow := minfix; rhigh := maxfix; precsn := stdpcn
    end
  else if rlow + precsn >= rhigh  then
    begin  {defaults in here too}
      error(fxptrange);
      rlow := minfix; rhigh := maxfix; precsn := stdpcn
    end
  else if rhigh/precsn > maxfix then
    begin	{ type is unrepresentable in 16 bits }
      error( 63 {given range and precision will not fit in 16 bits});
      rlow := minfix; rhigh := maxfix; precsn := stdpcn
    end

  end		{of with q^ }
 end;			{ * of case fixedsy * }


arraysy: begin
  insymbol;	{ gobble up 'array' }
  if sym.sy = lbrack then insymbol else error(11);
  q1 := nil;
  repeat		{ scan list of subscript bounds, chain backwards! }
    talloc(q, arrayt,false);
    with q^ do
      begin form := arrayt; aeltyp := q1; inxtyp := nil end;
    q1 := q;
    typ(q2);
    {  We allow "array[integer]", but it means "array[-32768..32767]" }
    if q2 <> nil then if q2^.form > integert then begin
      error(113);
      q2 := nil
      end;
    q^.inxtyp := q2;			{ type of index }
    if sym.sy = comma then
      begin  endoflist := false;  insymbol  end
    else  endoflist := true
  until endoflist;
  if sym.sy = rbrack then insymbol else error(12);
  if sym.sy = ofsy then insymbol else error(8);
  typ(q);	{ parse base type of array }
  if q^.form = devicet then error(181  {device may not be element of array});
  repeat				{ reverse list of subscripts! }
    with q1^ do begin
      q2 := aeltyp; aeltyp := q;
      getbounds(inxtyp, lmin, lmax);
      size := (lmax - lmin + 1) * typsize(q); { size is range * elt size }
      end;
    q := q1; q1 := q2
  until q1 = nil
  end;

recordsy: begin
  insymbol;	{ gobble up 'record' }
  oldtop := top;
  if top < maxdis then begin
    top := succ(top);
    display[top].fname := nil;
    display[top].scope := rcrd
    end
  else error(250);
  fldoffset := 0;  bitoffset := 0;
  fieldlist(q1, p, packrequest);
  talloc(q, recordt,false);
  with q^ do begin
    size := fldoffset;	{ maximum size of record }
    if packrequest then
      if (bitoffset mod 8) <> 0 then size:=size+1; {allocate fractional field}
    form := recordt;
    fstfld := display[top].fname;
    recvar := q1
    end;

		{ dump record to the symbol table }
  writeln(symfil);
  writeln(symfil,'* ',display[top].fname^.name^.s:ord(display[top].fname^.name^.l),' ',top:3,ord(display[top].scope):2);
  printident( display[top].fname );

  top := oldtop;
  if sym.sy = endsy then insymbol else error(13)
  end;

devicesy:
  begin		{process device type}
    insymbol;		{first ingest the keyword "device"}

    oldtop := top;	{open a new scope}
    if top < maxdis then
      begin
	top := succ(top);
	display[top].fname := nil;
	display[top].scope := rcrd
      end
    else
      error (250  {"too many levels of identifier scope"});

    talloc(q,devicet,false);	{allocate a type entry}

    if sym.sy = lbrack then	{parse device address, if any}
      begin
	insymbol;	{chew up bracket}
	expression;	{parse the address}
	if (gattr.akind = cst) and (gattr.atype^.form = integert) then
	  begin		{syntactically legal device address }
	    if checklegal( gattr.avalue.ival ) then
	      q^.devaddr := gattr.avalue.ival
	    else
	      q^.devaddr := 0;	{ default to prevent later errors }
	    q^.addressed := true
	  end
	else
	  error(176  {"illegal format for device address"});
	if sym.sy = rbrack then
	  insymbol
	else
	  error(12  {"expected right bracket"})
      end
    else		{no device address given}
      q^.addressed := false;	{check that this is legal}

			{parse the field list of the device}
    fldoffset := 0;  bitoffset := 0;
    fieldlist(q1, p, true);	{devices are always packed}
    with q^ do
      begin
	size := fldoffset;
	if (bitoffset mod 8) <> 0 then	{ must allocate the fractional}
	  size:=size+1;			{  field at this level		}
	form := devicet;
	fstdfld := display[top].fname;
	devvar := q1			{ ???? }
      end;


		{ dump record to the symbol table }
 if display[top].fname <> nil then
 begin		{fname is not NIL uless there was a syntax error}
  writeln(symfil);
  writeln(symfil,'* ',display[top].fname^.name^.s:ord(display[top].fname^.name^.l),' ',top:3,ord(display[top].scope):2);
  printident( display[top].fname );
 end;

    top := oldtop;		{restore the previous scope}
    if sym.sy = endsy then
      insymbol		{consume the "end"}
    else
      error(13 {"expected 'end'"})

  end;		{processing of device type}

setsy: begin
  insymbol;	{ gobble up 'set' }
  if sym.sy = ofsy then insymbol else error(8);	{ gobble up 'of' }
  typ(q1);
  talloc(q, sett,false);
  with q^ do
   begin
    size := setsize(q1);  form := sett;  settyp := q1;
    if size > maxset then error( 115 {illegal base type} )
   end
  end;


notsy, mulop, relop, rparen, lbrack, rbrack, comma, semicolon,
period, colon, becomes, constsy, typesy, varsy, programsy,
proceduresy, functionsy,  forwardsy, beginsy, ifsy, casesy,
repeatsy, whilesy, forsy, withsy,
endsy, elsesy, untilsy, ofsy, dosy, tosy, downtosy, thensy,
externalsy, eofsy, othersy, precisionsy, valuesy, raisesy,
exportsy,importsy,initsy,modulesy,monitorsy,prioritysy
	: {... in other words - default }
 begin
  error(10);
  q := nil
 end

end;		{ of case sym.sy }

if q = nil then q := notypeptr;		{ never return nil }
fq := q
end {typ};

