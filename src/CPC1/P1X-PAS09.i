
procedure expression;
var
  lattr: attr;
  lop: operator;


procedure simpleexpression;
var
  lattr: attr;
  lop: operator;
  issigned: boolean;


procedure term;
var
  lattr: attr;
  lop: operator;


procedure factor;
var
  coffset: addrrange;
  cvalue: integer;
  p: itp;
  q: stp;
  n, nmax: integer;
  s: stndsetptr;
  cstpart, varpart: boolean;
  endoflist: boolean;
begin {factor}
if not (sym.sy in
    [ident,intconst,fixconst,stringconst,lparen,notsy, returnsy,lbrack]) then
begin
    error (6  {illegal symbol});
    errorexpr;			{ enter legal dummy }
end else
case sym.sy of
  ident: begin
    p := searchid([types,konst,vars,field,proc]);
    insymbol;
    if p = udptrs[types] then { ident was not declared }
      if (sym.sy = lparen) or (sym.sy = semicolon) then
	p := udptrs[proc]	{ just for better error recovery }
      else
	p := udptrs[vars];
    case p^.class of
      types: begin
	if sym.sy = lparen then insymbol else error(9);
	expression;
	if sym.sy = rparen then insymbol else error(4);
        transfertype(p^.itype, gattr)
	end;
      vars, field, konst:
	selector(p,reference);
      proc:
	call(p)
      end;
    if gattr.atype = nil then begin	{ if no type determined yet }
      error(105 { Identifier type unknown }); { report problem }
      errorexpr;			{ create dummy expression }
      end;
    end;
  intconst: begin
    gattr.akind := cst;
    talloc( gattr.atype, integert, true );
    with gattr.atype^ do
      begin  {define the type as a single point subrange of integer}
        form := integert;		{ base type is integer }
	size := 2;	{integer size}
	maxconst := nil;		{ not a scalar }
	maxvalue := val.ival;
	minvalue := val.ival
      end;
    gattr.avalue.kind := lit;
    gattr.avalue.ival := val.ival;
    gattr.cstnamed := false;		{ not a named constant }
    insymbol
    end;
  fixconst: begin
    gattr.akind := cst;
    talloc(gattr.atype,fixedt, false );  {allocate a fx.pt. type for this const}
    gattr.cstnamed := false;		{ not named }
    with gattr.atype^ do
      begin  {set range= value, precision=manifest precision}
	form := fixedt;  size := fixsiz;
	rhigh := val.rval; rlow := val.rval; precsn := val.prcsn
      end;
    gattr.avalue.kind := reel;
    gattr.avalue.rval := val.rval;
    gattr.avalue.prcsn := val.prcsn;
    insymbol
    end;
  stringconst: begin
    gattr.akind := cst;
    gattr.cstnamed := false;		{ not named }
    if lgth = 1 then begin {character constant}
      gattr.atype := charptr;
      gattr.avalue.kind := lit;
      gattr.avalue.ival := ord(string[0])
      end
    else begin {string constant}
      talloc(gattr.atype, arrayt, false );
      with gattr.atype^ do begin
	form := arrayt; size := lgth;
	aeltyp := charptr;
	{create a new index type to describe this string constant}
        talloc(inxtyp, integert, true );
	inxtyp^.size :=1;
	inxtyp^.minvalue := 1; inxtyp^.maxvalue :=lgth
	end;
      gattr.avalue.kind := data;
      gattr.avalue.daddr := dc;
      for n := 0 to lgth-1 do
	gendbyte(ord(string[n]))
      end;
    insymbol
    end;
  lparen: begin
    insymbol;
    expression;
    if sym.sy = rparen then insymbol else error(4)
    end;
  notsy: begin
    insymbol;
    factor;
    if gattr.atype = boolptr then
      if gattr.akind = cst then
	gattr.avalue.ival := 1 - gattr.avalue.ival
      else
       begin	{runtime negation}
	genbyte( 96 {NOT} );
        gattr.akind := exp;  {this is now an actual expression}
       end
    else
      error(134);
    end;
  returnsy: begin			{ return value of current fn }
					{ if not in function }
    if (blktype <> proctyp) or (blkname^.itype = nil)  then begin 
	error(1046 {Cannot refer to return value unless in function});
	insymbol;			{ use up return symbol }
    end else begin			{ if in function }
	if vmode <> exitmode then 	{ if not in EXIT decl }
	    error(1045 {RETURN permitted only in EXIT declaration});
	insymbol;			{ use up return symbol }
	selector(blkname^.next,reference); { use return value arg of fn }
	end;				{ end in function }
    end;
  lbrack: begin
    insymbol;
    q := nil;  n := 0;  nmax := 0;
    varpart := false;  cstpart := false;
    new(s);  s^ := [];	{ place to store constant part }
    if sym.sy <> rbrack then begin
      repeat
	expression;
	if gattr.atype <> nil then begin
	  if (q = nil) or comptypes(gattr.atype, q) then
	   begin
	    n := setsize(gattr.atype);
	    if n > 0 then
	     begin
	      if gattr.akind = cst then { constant element }
		if (gattr.avalue.ival >= 0) and (gattr.avalue.ival <= maxset)  then
		 begin
		  s^ := s^ + [gattr.avalue.ival];
		  n := gattr.avalue.ival+1;  {make set just big enough to hold constant}
		  cstpart := true
		 end
		else error(137)
	      else { variable element }
		if varpart then
		  genbyte(118 {SADEL})
		else
		 begin
		  genbyte(117 {SGENS});
		  varpart := true
		 end;
	      q := gattr.atype
	     end
	    else error(136);
	    if n > maxset then error( 133 {set elmtn larger than implementation max });
	    if n > nmax then  nmax := n
	   end
	  else error(137)
	end;
	if sym.sy = comma then
	  begin  endoflist := false;  insymbol  end
	else  endoflist := true
      until endoflist
    end;
    if sym.sy = rbrack then insymbol else error(12);
    talloc(gattr.atype, sett, false);
    with gattr.atype^ do begin
      form := sett;  size := nmax;  settyp := q
      end;
    gattr.akind := cst;
    gattr.avalue.kind := setc;
    gattr.avalue.sval := s;
    if varpart then begin
      if cstpart then begin
	gencon(gattr);
	genbyte(113 {UNION})
	end;
      gattr.akind := exp
      end
    end
  end;
  assert(gattr.atype <> nil);		{ type must exist by now }
end {factor};


begin {term}
factor;
while sym.sy = mulop do begin
  lattr := gattr;
  lop := sym.op;
  insymbol;
  factor;
  binop(lop, lattr)
  end
end {term};


begin {simpleexpression}
issigned := false;
if (sym.sy = addop) and ((sym.op = plus) or (sym.op = minus)) then
  begin issigned := sym.op = minus; insymbol end;
term;
if issigned and (gattr.atype <> nil) then
  case gattr.atype^.form of
    integert:
      if gattr.akind = cst then
	gattr.avalue.ival := - gattr.avalue.ival
      else
	begin    {emit code for runtime negation}
	  gattr.akind := exp;
	  genbyte(40 {INEG})
	end;
    longintt:
      if gattr.akind = cst then error(notyetimpl)
	{ coming attraction }
      else
	genbyte(40 {INEG});
    fixedt:
    	if gattr.akind = cst then
	  gattr.avalue.rval := - gattr.avalue.rval
	else
	  begin    {runtime negate}
	    gattr.akind := exp;
	    genbyte(52 {SNEG})    {** !! **} {???}
	  end;
    scalar, booleant, chart, pointer, sett,
    xcptnt, signalt, arrayt, recordt, devicet:
      error(134)
    end;
while sym.sy = addop do begin
  lattr := gattr;
  lop := sym.op;
  insymbol;
  term;
  binop(lop, lattr)
  end
end {simpleexpression};


begin {expression}
simpleexpression;
if sym.sy = relop then begin
  lattr := gattr;
  lop := sym.op;
  insymbol;
  simpleexpression;
  binop(lop, lattr);
  gattr.atype := boolptr
  end
end {expression};


