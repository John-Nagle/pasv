
		{       BLOCK       }
		{*******************}

procedure block (blkname: itp; blktype: unittype );
	 {=====		PARSE A BLOCK	}
		{ blkname points to proc/module name;    }
		{ blktype indicates type of header on this block }

var
  fwptr, varlst, p: itp;
  headerasserts: compoundtally;		{ count of assertions in header }
  depthdecls: integer;			{ count of DEPTH declarations }
  headericode: fint;			{ holding file for ENTRY, etc. }
  diverticode: boolean;			{ divert to headericode if on }
  ecount,		{ count of exception handlers in this block }
  rvsize : integer;	{ size of returned value }
  hmarker: integer;	{ place holder for mark and release }
{
	code output routines -- here to allow reference to local file
	headericode.
}
{
	signedvalue  --  converts signed integer to target representation

	The representation is twos complement signed, 16 bits.
}
function signedvalue(n: integer)	{ input value }
		: integer;		{ output value }
const negbias = 65536;			{ this - value is representation of neg}
begin
    if n >= 0 then signedvalue := n	{ positive case }
    else signedvalue := negbias + n;	{ negative case }
end {signedvalue};
{
	genbyte  --  generate icode byte

	Goes to icode file unless diverticode is set, in which case
	output goes to headericode file.
	Diversion occurs only for the verifier.
}
procedure genbyte(fi: integer);
begin
if vmodes.generatecode then begin	{ if code generation on }
   if diverticode then putbyte(fi, headericode) else putbyte(fi,int);
  end
end {genbyte};
{
	genword  --  generate icode word

	As with genbyte, diversion applies.
	Note that a word is 0..65535
}
procedure genword(fi: integer);
begin
if vmodes.generatecode then begin	{ if code generation on }
   if diverticode then putword(fi, headericode) else putword(fi,int);
  end
end {genword};
{
	gensignedword  --  generate signed icode word
}
procedure gensignedword(fi: integer);
begin
					{ check for machine overflow }
    if fi > 32767 then error(203 {integer constant exceeds range});
    if fi < -32768 then error(203 {integer constant exceeds range});
    genword(signedvalue(fi));		{ generate signed word }
end {gensignedword};
{
	genaddressword  --  generate address word

	If the value given is negative, the value is a stack-relative
	address and must be treated as signed in the machine sense.
	If the value given is positive, the value may be as large
	as 65535, so normal conversion does not apply.
}
procedure genaddressword(fi: integer);		{ word to generate }
begin
    if fi < 0 then				{ if negative }
	gensignedword(fi)			{ signed form }
    else
	genword(fi);				{ unsigned form }
end {genaddressword};
{
	genflit  --  generate fixed point literal
}
procedure genflit( x: valu);
	 {-------	fixed point literal generator}
  {generate a fixed point literal}
  { note: expects that x.kind = reel }
  var  scldval, pshift : integer;

begin
    formatflit( x, scldval, pshift );  { get a scaled integer}
    begin
      genbyte (pshift+128);	{offset scale factor by 128}
      gensignedword (scldval);
    end
end  {genflit} ;
{
	The use of real numbers in the production compiler cannot
	be countenanced, since the results would be machine dependent.
	This code is therefore TEMPORARY.
}
function log2(r: real): integer;
const expmax = 125;				{ maximum exponent in machine }
var hi, lo, x: -expmax..expmax;			{ bounds }
begin
    r := abs(r);				{ use positive form }
    assert(r > 0);				{ expo(0) undefined }
    hi := expmax;				{ high bound }
    lo := -expmax;				{ low bound }
    repeat					{ binary search }
	x := (hi + lo + 1) div 2;		{ get new midpoint, rounding up}
	if power2(x) <= r then begin		{ if x is not too large }
	    lo := x;				{ use it as new low bound }
	end else begin				{ otherwise }
	    if hi > x then hi := x else hi := hi - 1; { get new high bound }
	    end;				{ end search loop }
	assert(lo <= hi);			{ bounds must not cross }
    until lo = hi; 				{ until bounds meet }
    log2 := lo;					{ return result }
    if power2(lo) > r then log2 := 999;		{ if out of range }
    if power2(lo+1) <= r then log2 := 999;	{ if out of range }
end {log2};
function typsize(fq: stp): integer; forward;




function typalign(fq: stp): integer;
begin {typalign}
if fq <> nil then with fq^ do case form of
  scalar, booleant, chart, longintt, fixedt,
  xcptnt, signalt,
  pointer,  tagfield, variant:
    typalign := ualign[form];
  integert:
 					{ see if it can be stored in one byte}
    if size = 1 then
      typalign := 1
    else			{  ..no, it can't. }
      typalign := ualign[integert];
  sett, recordt, devicet:
    if typsize(fq) < auword then typalign := 1
    else typalign := 2;
  arrayt:
    typalign := typalign(aeltyp)
  end
else typalign := 1
end {typalign};






function setsize(fq: stp): integer;
begin {setsize}
if fq <> nil then with fq^ do
  case form of
    scalar, booleant, chart, integert, longintt: begin  { scalar types }
      setsize := (maxvalue - minvalue) + 1;		{ compute set size }
      assert(maxvalue >= minvalue);			{ checked in type def }
      end;
    fixedt, xcptnt, signalt,
    pointer, sett, arrayt,
    recordt, devicet,  tagfield, variant: {default}
      begin error(115); setsize := 0 end
    end
else setsize := 0
end {setsize};




function typsize(fq: stp): integer;
		{ This function returns the size, in bytes, required
		  for the unpacked representation of the type
		  designated by its argument.			    }
begin {typsize}
if fq <> nil then with fq^ do case form of
  fixedt, xcptnt, signalt, pointer:  begin {fixed-size types }
    assert(size = usize[form]); 	{ size must never change }
    typsize := size;			{ decided at declaration }
    end;
  scalar, booleant, chart, integert, longintt,	{ types with subranges }
  recordt, devicet, tagfield, variant,
  arrayt,sett:
    typsize := size;			{ decided at declaration }
  end
else typsize := 0
end {typsize};


function typwidth (tp:stp) : integer;
		{ This function returns the width required in bits
		  for the packed representation of the type
		  designated by its argument.			  }

  var t : integer;

{
	nbits  --  returns the number of bits required to represent
		   a quantity as a twos complement binary number WITHOUT
		   a sign bit.
}
function nbits(quantity: integer): integer;
var n, twoton: integer;			{ n, 2**n }
begin
    if quantity < 0 then quantity := (-quantity)-1; { handle negatives }
    n := 1; twoton := 2;
    while quantity >= twoton do begin	{ repeat until big enough }
	n := n + 1;  twoton := twoton * 2; { increase }
	end;
    nbits := n;				{ return exponent }
end { nbits };
begin {typwidth}
  if  tp <> nil then with tp^ do
    case form of
      scalar,booleant,chart,integert,longintt: begin	{ types with ranges }
	t := imax(nbits(minvalue),nbits(maxvalue)); { bits for value }
	if minvalue < 0 then begin		{ if negative value }
	    t := t + 1;				{ add space for sign bit }
	    { NOTE: a negative number forces a 16-bit field, as there is
	            no sign-extended field extraction in icode. }
	    t := imax(t,bitswd);			{ ***UNIMPL*** }
	    end;
	typwidth := t;				{ return size in bits }
	end;
      fixedt:
	typwidth := 16;		{*****  TEMP  *****}
      xcptnt, signalt, pointer:
	typwidth := bitswd;
      sett:
	typwidth := size;	{number of bits = number of elements}
      arrayt:
	if typwidth(aeltyp) <= bitswd then
	  begin
	    i := bitswd div typwidth(aeltyp);
	    typwidth := (size div i)*bitswd + (size mod i)*typwidth(aeltyp)
	  end
	else
	  begin
	    t := typwidth(aeltyp);
	    if (t mod bitswd) <> 0 then
	      typwidth := ((t div bitswd)+1)*size
	    else
	      typwidth := (t div bitswd)*size
    	  end;
      recordt,devicet,tagfield,variant:
	typwidth := 8*size
    end
end  {typwidth};
    function getatomsize( z: stp ): integer;
	{ size of var or element size if array }
      begin
	if z^.form = arrayt then
	  getatomsize := getatomsize( z^.aeltyp )
	else
	  getatomsize := typsize( z )
      end;

procedure genlit(fi: integer);
	 {------		generate a literal }
begin
  if fi >= numlimit then error( 203 {integer constant too large});
  genbyte(162); gensignedword(fi)
end {genlit};



procedure genscl( op: integer; targettype: stp);
	 {------		generate FIX & RESCL operators }
	 {	this procedure assumes the first operator has  }
	 {	been generated.				       }
  var xtmp: valu;
begin
  with targettype^ do
    begin
      xtmp.kind :=reel;
      xtmp.prcsn := precsn;		{ force range values to multiple of precision}
      				{  generate the operator}
      genbyte( op  {FIX or RESCL});
				{ .. then the scaling arguments}
      xtmp.rval := rlow;  genflit(xtmp);
      xtmp.rval := rhigh;  genflit(xtmp);
      xtmp.rval := precsn;  genflit(xtmp)
    end
end;	{genscl}






procedure gendat(var fattr: attr);
var
  i, j, n: integer;
  ssize: integer;		{ size of set }
begin {gendat}
if (fattr.akind = cst) and (fattr.avalue.kind = setc) then
  if fattr.atype <> nil then with fattr.atype^ do begin
    assert(form = sett);		{ this must be a set }
					{ base type must be scalar }
    assert(settyp^.form in [scalar, booleant, chart, integert, longintt]);
    ssize := (settyp^.maxvalue - settyp^.minvalue)+1;	{ range of basetype of set }
    if ssize <= maxlit then begin	{ if set will fit in 16-bit literal }
      n := 0;
      j := 1;
      for i := 0 to ssize-1 do begin	{ sets are stored from minvalue }
        if i in fattr.avalue.sval^ then n := n + j;
        j := j * 2
        end;
      fattr.avalue.kind := lit;
      fattr.avalue.ival := n
      end
    else begin
      if odd(dc) then dc := succ(dc);
      error( notyetimpl );
      for i := 0 to ssize div bitswd do
        { do a gendword on each word of set };
      fattr.avalue.kind := data;
      fattr.avalue.daddr := dc
      end
    end
end {gendat};




procedure gencon(var fattr: attr);
begin {gencon}
gendat(fattr);
case fattr.avalue.kind of
  lit: genlit(fattr.avalue.ival);
  reel:
    begin
      genbyte(160 {LITSC});
      genflit(fattr.avalue)
    end;
  data: begin
    genbyte(163 {RDATA});
    genbyte(getatomsize(fattr.atype));	{generate size field}
    genaddressword(fattr.avalue.daddr)
    end
  end;
fattr.akind := exp
end {gencon};




procedure gendif(fi: integer);
begin
if fi <> 0 then if fi > 0
  then begin genlit(fi); genbyte(32 {IADD}) end
  else begin genlit(-fi); genbyte(33 {ISUB}) end
end {gendif};

procedure gendbyte(fi: integer);
begin
if vmodes.generatecode then begin	{ if not in some verifier mode }
   putbyte(fi,dat);
   dc := succ(dc)
   end;
end {gendbyte};
{
	gendword  --  generate an unsigned word in the data file
}
procedure gendword(fi: integer);
begin
if vmodes.generatecode then begin	{ if not in some verifier mode }
  putword(fi,dat);
  dc := dc + 2
  end;
end {gendword};
{
	gendsignedword  --  generate a signed word in the data file

	The signed representation is that of the target machine.
}
procedure gendsignedword(fi: integer);
begin
    gendword(signedvalue(fi));		{ generate in signed form }
end {gendsignedword};





procedure genid(fid: idp);
var
  i: integer;
begin
if fid <> nil then with fid^ do begin
  genbyte(l);
  for i := 1 to l do genbyte(ord(s[i]))
  end
end {genid};



{
	errorexpr  --  set current expression to error value

	Used when errors are detected in processing expressions
	to keep constructs valid.

	The error expression is an integer constant, value
	0, type 0..0
}
procedure errorexpr;
begin
    gattr.atype := notypeptr;		{ use dummy type }
    gattr.akind := cst;			{ kind is constant }
    gattr.avalue.kind := lit;		{ constant class is literal }
    gattr.avalue.ival := 0;		{ value is 0 }
    gattr.cstnamed := false;		{ not named constant }
end { errorexpr };
{
	entervmode  --  enter new vmode
}
procedure entervmode(newmode: vermodes);
begin
   if vmodestacktop >= vmodestackmax then begin	{ if overflow }
	error(1026 {functions nested too deeply});
   end else begin
	vmodestacktop := succ(vmodestacktop);	{ otherwise push }
	vmodestack[vmodestacktop] := newmode;	{ save new mode }
	vmode := newmode;			{ current mode := new mode }
        vmodes := vmodetab[vmode];		{ set switches for everybody }
	if verifier then vmodes.generatecode := true; { verifier always gens }
        end;
end {entervmode};
{
	exitvmode  --  back to previous vmode
}
procedure exitvmode;
begin
					{ entry/exit must match for good progs}
   assert((errinx > 0) or (errtot > 0) or (vmodestacktop > 0));
					{ this is just to keep going }
   if vmodestacktop > 0 then vmodestacktop := pred(vmodestacktop);
   vmode := vmodestack[vmodestacktop];	{ get previous mode }
   vmodes := vmodetab[vmode];		{ set switches for everybody }
   if verifier then vmodes.generatecode := true; { verifier always gens }
end {exitvmode};




function assignable( source, target: stp ): boolean;
	{----------		This function checks whether the type
			denoted by source is assignment compatible with
			the type denoted by target in the sense of ISO
			standard Pascal.  It is more restrictive than
			comptypes.  There is a hole in the check for
			subranges of enumerated types. See below.}
  begin	  {assignable}

    if (source=nil) or (target=nil) then
      assignable := false	{avoid bombouts! shun non-existent types!}

    else if source = target then
      assignable := true	{types are same, hence assignment compatible}

    else if source^.form = target^.form then
      with target^ do
	case form of
	  scalar, booleant, chart, integert, longintt:
	    if (form = scalar) and (source^.maxconst <> maxconst) then
	      assignable := false   {host types differ}
		{Note: Different enumerated types are not compatible.
		      It is assummed that two enumerated types differ
		      iff their maximum value is not the same identifier.
		      This assumption depends on the fact that subrange
		      sets maxconst to the value of the host type for a
		      scalar.  !!!!!!!!!!!!!!!!		}
  	    else 			{ check if subrange will fit }
		assignable :=     (source^.maxvalue <= maxvalue)
			      and (source^.minvalue >= minvalue);
	  fixedt:
	    assignable := true;  {"just leave the scaling to us"}
	  sett:
	    if source^.settyp = nil then
	      assignable := true    {Empty set can be assigned to anything}
	    else
	      assignable := assignable( source^.settyp, settyp );
		{Note: This is a restriction of ISO std. rules}
	  tagfield:
	    assignable := assignable( source^.tagtyp, tagtyp );
	  xcptnt, signalt,
	  arrayt, recordt,
	  devicet, variant:
	    assignable := false   {can't assign any of these unless types
					are identical.}
	end	{ Case form }

    else if target^.form = fixedt then
      assignable := (source^.form = integert)
    else if target^.form = tagfield then
      assignable := assignable( source, target^.tagtyp )
    else if source^.form = tagfield then
      assignable := assignable( source^.tagtyp, target )

    else		{  All the king's horses and all the king's men }
      assignable := false  { couldn't get these two together.           }

  end;	{assignable}

function comptypes(fq1, fq2: stp): boolean;
var
  form2: forms;
begin {comptypes}
comptypes := true;	{ until shown otherwise }
if (fq1 <> fq2) then
 if (fq1 = nil) or (fq2 = nil) then
  comptypes := false
 else begin
  form2 := fq2^.form;
  with fq1^ do begin
    case form of
      scalar:		{ to handle subranges, identifier list must be same }
	{P-compiler note: "indentical scalars declared on different
	 levels are not recognized to be compatible"}
	comptypes := (form2 = scalar) and (maxconst = fq2^.maxconst);
      xcptnt, signalt,
      booleant, chart:
	comptypes := (form2 = form);
      integert, longintt:
	comptypes := (form2 = integert) or (form2 = longintt);
      fixedt:
	comptypes := (form2 = fixedt);
      pointer:
	{P-compiler note: code is much different here}
	comptypes := (form2 = pointer) and comptypes(eltype, fq2^.eltype);
      sett:
	comptypes := (form2 = sett) and ( comptypes(settyp, fq2^.settyp)
					 or (settyp = nil)
					 or (fq2^.settyp = nil)  );
		{Note: the null set matches any set type}
      arrayt: begin
	comptypes := false;		{ assume failure }
	if form2 = arrayt then		{ if 2nd op is array, maybe OK }
	  comptypes := comptypes(aeltyp, fq2^.aeltyp)
		and assignable(inxtyp,fq2^.inxtyp)
		and assignable(fq2^.inxtyp,inxtyp);
	{P-compiler note: sizes must also be equal}
	{P-compiler note: "alternatives:
	 ... indextype must be compatible.
	 ... lowbounds must be the same."}
	  end;
      recordt,devicet:
	comptypes := false;
	{P-compiler note:"identical records are recognized
	 to be compatible iff no variants occur"}
      end
    end
  end
end {comptypes};


procedure converttofix( var at : attr);
  {ASSERT: at.atype^.form=integert  }
  {convert the expression to fixed type}

var
    ftp : stp;
    nval: valu;

begin
  {assume a standard conversion for now}
  talloc(ftp,fixedt, false);  {must always allocate a new type since attributes change}
  with ftp^ do
    begin
      form := fixedt;  precsn := 1.0;
      if at.akind = cst then
	begin  {convert the value now}
	  nval.kind := reel;
	  nval.rval := at.avalue.ival; nval.prcsn := 1.0;
	  rlow := nval.rval; rhigh := nval.rval;
	  at.avalue := nval
	end
      else
	begin  {set fx. pt. range equal to subrange of integer }
	  rlow := at.atype^.minvalue;  rhigh := at.atype^.maxvalue
	end
    end;
  at.atype := ftp

end;    	{  of convert  }



procedure transfertype(target: stp; var obj: attr);
    {Invoked by recognition of a type transfer function}

  var holdconst: real;

  begin
   if (target^.form < fixedt) and comptypes(target, obj.atype) then
    obj.atype := target
   else if (target^.form=fixedt) and ((obj.atype^.form=fixedt) or
				      (obj.atype^.form=integert)) then
    begin
     obj.atype := target;    	{give it the correct type}
     if obj.akind=cst then	{do conversion at compile time}
      begin
	if obj.avalue.kind = lit then
	  holdconst := obj.avalue.ival
	else if obj.avalue.kind=reel then
	  holdconst := obj.avalue.rval
	else
	  error(notyetimpl);	{**********************}
	if (holdconst < target^.rlow) or (holdconst > target^.rhigh) then
	  error( 206  {"magnitude of constant is too large"});
	obj.avalue.kind := reel;
	obj.avalue.rval := holdconst;
	obj.avalue.prcsn := target^.precsn
      end			{of constant case}
     else
      begin			  {and generate the conversion code}
	genbyte( 53 {RESCL} );
	genbyte( 2 );	  {size field}
	genscl(3 {FIX}, target);	{scaling psuedo-op}
      end
    end
   else if (target^.form = arrayt) and comptypes(target,obj.atype) then
    obj.atype := target
   else
    error( 134  {"illegal type of operands"})

  end;




procedure expression; forward;



procedure setcoerce(fq: stp);
begin {setcoerce}
 assert(fq <> nil);			{ ***DEBUG*** }
if (gattr.atype = nil) then
  gattr.atype := fq
else if (gattr.atype^.form <> sett) or
 (gattr.atype^.settyp = nil) then
  gattr.atype := fq
else
  { ***NEED CHECK FOR SUBRANGE INCLUSION*** }
end {setcoerce};


procedure selector (fp: itp;
		    reqkind: refchg);	{ reference, change, or both }
var
  lattr: attr;
  p: itp;
  loffset: integer;
  more: boolean;
  finalload: boolean;			{ is final genload requried? }




procedure genload;
    function getatomsize( z: stp ): integer;
	{ size of var or element size if array }
      begin
	if z^.form = arrayt then
	  getatomsize := getatomsize( z^.aeltyp )
	else
	  getatomsize := typsize( z )
      end;


begin {genload}
if gattr.atype <> nil then
 case gattr.akind of
  cst: case gattr.avalue.kind of
    {this may not be optimum ssd}
    lit:  begin		{note:   generation of literal constants is}
				{postponed to allow folding of constants}
       {genbyte(162 LITER);
       gensignedword(gattr.avalue.ival);
       gattr.akind := cst }  end;
    data: begin
      genbyte(163 {RDATA});
      genbyte( getatomsize(gattr.atype) );	{size field}
      genword(gattr.avalue.daddr);		{ must be positive }
      gattr.akind := ref
      end;
    reel: { coming attraction };
    setc: { coming attraction }
    end;
  ref: begin
    case gattr.access of
      direct:
	genbyte(176 {VARBL} + gattr.alevel);
      byvalue:
	genbyte(192 {PARAM} + gattr.alevel);
      offset:
	genbyte(132 {OFSET});
      indirect:
	genbyte(133 {INDIR});
      indexed:
	genbyte(134 {INDEX});
      absolute:
	genbyte(174 {DVAD});
      subfield:
	genbyte(131 {FIELD})
      end;
    if gattr.access = subfield then
      begin
	genbyte(typwidth(gattr.atype));  {field width in bits}
	genword(gattr.addr)
      end
    else
      begin
	genbyte(getatomsize(gattr.atype));
	genaddressword(gattr.addr)
      end;
    if gattr.atype^.form = fixedt then  {generate scaling info for pass2}
      genscl(3 {FIX}, gattr.atype);
    if (gattr.atype^.form = signalt) and  (gattr.atype^.addresspresent = true) then
      begin	{emit a SIGNL operator}
	genbyte( 130 {SIGNL} );
	genword( gattr.atype^.trapvec )
      end;
    end		{ref: }
  end		{ case gattr.akind }
end {genload};


procedure genfield (size, displace: integer);
		{generate a field modifier}
begin
  genbyte(131 {FIELD});
  genbyte(size);			{size of field in bits }
  genword(displace);           	{ start of field }
end {genfield};





begin {selector}
finalload := true;			{ assume final load required }
with fp^ do begin
  assert(itype <> nil);			{ ***DEBUG*** }
  gattr.atype := itype;
  lastvclass := executablevar;		{ assume result will be executable }
					{ following line should be unnneded }
  if itype = nil then  error( 105  {"identifier type unknown"} );
  case klass of
    konst: begin
      lastaccess := reference;		{ constants are read-only, of course }
      gattr.akind := cst;
      gattr.avalue := kvalue;
      gattr.cstnamed := true;		{ is a named constant already in dict }
      end;
    vars: begin
      if foreign then 			{ if imported/exported }
	lastaccess := reference 	{ then read only }
      else
	lastaccess := referenceandchange; { if not, both are allowed }
      lastvclass := vclass;		{ save vclass of variable }
      gattr.akind := ref;
      if vkind = local then
	gattr.access := direct
      else
	gattr.access := byvalue;
      gattr.alevel := vlev;
      gattr.addr := vaddr;
      if (vkind=local) and (gattr.atype <> nil) then
       if (gattr.atype^.form=devicet)  then
	begin		{set up absolute addressing}
	  gattr.access := absolute;
	  if gattr.atype^.addressed then
	    gattr.addr := gattr.atype^.devaddr
	  else
	    error(177 {"device address not specified"})
	end;
      if vkind = formal then begin
	genload;
	gattr.access := indirect;
	gattr.addr := 0
	end
      end;
    field: begin
      with display[disx] do begin
	gattr.akind := ref;  { doesn't work with record structured constants }
	assert(scope = rcrd);	{ must be in WITH clause }
	lastaccess := daccess;	{ get allowed access modes }
	lastvclass := dvclass;	{ return vclass of WITH expression }
	if (fvclass = extravar) and (lastvclass = executablevar) then
	  lastvclass := fvclass;	{ if field is EXTRA, override }
	if occur = crec then
	 begin { direct reference }
	  gattr.access := direct;
	  gattr.alevel := dlev;
	  if ispacked then
	    begin	{change to subfield}
	      gattr.addr:=daddr;
	      gattr.atype := rtype;	{ .. for now. Need to get VARBL right}
	      genload;
	      gattr.atype:=itype;	{ now put things back }
	      gattr.access:=subfield; {change access for load at end of selector}
	      gattr.addr:=bdisp;
	    end
	  else
	   gattr.addr := daddr+fdisp;
	 end
	else if occur = cdev then
	 begin	{ direct reference to a device }
	  gattr.access := absolute;
	  gattr.addr := dvaddr;
	  {NOTE: in this case we can assume ispacked=TRUE}
	  gattr.atype := rtype;		{ help genload get the SIZE right}
	  genload;			{ generate a dvad}
	  gattr.atype := itype;		{ then back to the field type }
	  gattr.access := subfield;
	  gattr.addr := bdisp
	 end
	else
	 begin { indirect reference }
	  genbyte(140 {RTEMP}); genword(tnum);
	  gattr.access := indirect;
	  gattr.addr := 0;
	  if ispacked then
	    begin
	      gattr.atype := rtype;
	      genload;
	      gattr.atype := itype;
	      gattr.access:=subfield;
	      gattr.addr:=bdisp
	    end
	  else
	    gattr.addr:=fdisp;
	 end;
	end
      end
    end {case}
  end; {with}
{
	Check for violation of monitor import/export rules regarding
	variables.
}
case lastaccess of			{ cases of allowed access }
referenceandchange: begin end;		{ no problem possible }
reference: begin			{ cannot change }
    if reqkind <> reference then
	error(1004 {Cannot change imported/exported variable});	
    end;
change: begin				{ cannot reference }
    if reqkind <> change then
	error(1003 {Cannot reference imported/exported variable});
    end;
  end;
{
	Now parse any trailing '.' or '^' or '[xx]' selector parts
}
repeat
  more := false;
  if sym.sy = period then begin			{ record field or .old marker }
    insymbol;					{ get field id or "old" }
    if sym.sy = oldsy then begin		{ if verifier old }
						{ if misuse of .old }
      {  We catch all misuses in pass 2; this is primarily to prevent use in
	 executable code. }
      if not (vmode in [proofmode, exitmode, effectmode]) then 
	error(1038 {OLD allowed only in EXIT and EFFECT});
      genload;					{ gen address take }
      genbyte(250 {OLD});			{ OLD }
      more := false;				{ ends selector expression }
      finalload := false;			{ bypass final load }
      insymbol;					{ use up "old" }
    end else begin				{ if normal field ref }
      if gattr.access = indexed then begin
        genload;
        gattr.access := offset;
        gattr.addr := 0
        end;
      if (gattr.atype <> nil)
        and  (gattr.atype^.form <> recordt)
        and  (gattr.atype^.form <> devicet)     then
        begin error(140); gattr.atype := notypeptr end;
      if sym.sy = ident then begin
        if gattr.atype <> nil then begin
	  if gattr.atype^.form = recordt then
	    p := searchlevel(gattr.atype^.fstfld)
	  else	{ devicet }
	    p := searchlevel(gattr.atype^.fstdfld);
	  if p <> nil then
	   begin
	    assert(p^.klass = field);	{ must be field }
					  { if EXTRA and not FREE var }
	    if (p^.fvclass = extravar) and (lastvclass = executablevar) then
	        lastvclass := p^.fvclass;	{ change to EXTRA }
	    if  p^.ispacked  then
	      begin
	        genload;		{emit code for base field}
	        gattr.addr := p^.bdisp;	{note subfield displacement}
	        gattr.access := subfield
	      end
	    else
	      gattr.addr := gattr.addr + p^.fdisp;
	    gattr.atype := p^.itype;
	    assert(gattr.atype <> nil);			{* ***DEBUG*** }
	   end
	  else
	   begin
	    error(152);
	    gattr.atype := notypeptr
	   end
	  end;
        insymbol
        end
      else error(2);
      more := true
      end
    end							{ end normal field }
  else if sym.sy = lbrack then begin
      if (gattr.atype <> nil) then
	if  (gattr.atype^.form <> arrayt) then
	  begin error(138); gattr.atype := nil end;
    repeat					{ handle subscripts }
      if gattr.atype = nil then gattr.atype := notypeptr; {use error type}
      insymbol;
      genload;
      lattr := gattr;
      expression;
      assert(lattr.atype <> nil);		{ fixed above }
      if lattr.atype^.form <> arrayt then begin	{ if not an array }
	error(138 {type of variable is not array});
	gattr := lattr;				{ ignore subscript expression }
        end					{ end error recovery }
      else					{ if valid array }
	  with lattr.atype^ do begin
	loffset := 0;				{ assume offset of 0 }
	if inxtyp <> nil then
          if inxtyp^.form in [scalar,booleant,chart,integert,longintt]  then
	    loffset := - inxtyp^.minvalue;
	if not assignable(gattr.atype, inxtyp) then
	if not assignable(gattr.atype, inxtyp) then
	  error( 139 {"index type not compatible with declaration"});
	if gattr.akind = cst then begin
	  gattr.avalue.ival := gattr.avalue.ival + loffset;
	  gencon(gattr);
	  gattr.akind := exp
	  end
	else
	  gendif(loffset);
	if aeltyp <> nil then
	  if aeltyp^.form = arrayt then
	    gattr.addr := aeltyp^.size	{ index multiplier }
	  else
	    gattr.addr := ceil(typsize(aeltyp), typalign(aeltyp));
	gattr.atype := aeltyp;
	assert(gattr.atype <> nil);			{ ***DEBUG*** }
	gattr.akind := lattr.akind;
	gattr.access := indexed
	end
    until sym.sy <> comma;
    if sym.sy = rbrack then insymbol else error(12);
    more := true
    end
until not more;
if finalload then genload;		{ final load except for OLD special }
verclasschk(lastvclass, reqkind);	{ check for misuse of EXTRA/FREE vars}
end {selector};




procedure binop(fop: operatorenum; var fattr: attr);
var
  q: stp;
  s: stndsetptr;
  opcode: 0..255;
begin {binop}
if (fop = inop) and (gattr.atype <> nil) then
  if gattr.atype^.form = sett then
    q := gattr.atype^.settyp
  else
    begin error(130); q := nil end
else
  q := gattr.atype;
if not comptypes(q, fattr.atype) then
  if ( q = nil )  or ( fattr.atype = nil ) then
    error( 134 { incompatible operand types} )
  else if (q^.form = fixedt) and (fattr.atype^.form = integert) then
    converttofix(fattr)
  else if (q^.form=integert) and (fattr.atype^.form=fixedt) then
    converttofix(gattr)
  else
    error(134);    {incompatible operand types}

if gattr.atype <> nil then begin
  opcode := bcode[gattr.atype^.form,fop];
  if opcode = 0 then
    error(134)
  else if gattr.atype^.form = arrayt then begin
    if not comptypes(gattr.atype^.aeltyp, charptr) then error(134);
    if fattr.akind = cst then gencon(fattr);
    if gattr.akind = cst then
      gencon(gattr)
    else
      if fattr.akind = cst then genbyte(1 {XCH});
    genbyte(opcode); genbyte(1);
    genword(imin(gattr.atype^.size, fattr.atype^.size))
    end
  else begin
    if fattr.akind <> cst then begin		{ <expr> <op> ?? }
      if gattr.akind = cst then			{ <expr> <op> <const> }
	gencon(gattr);
      gattr.akind := exp;   {no longer a variable(ref) or constant}
      genbyte(opcode)
      end
    else					{ <const> <op> ?? }
      if gattr.akind <> cst then begin		{ <const> <op> <expr> }
	gencon(fattr);
	genbyte(1 {XCH});
	gattr.akind := exp;  {expression is not variable or constant}
	genbyte(opcode)
	end
      else					{ <const> <op> <const> }
	if not (gattr.atype^.form in		{ OTHERS removal }
		[scalar,booleant,chart,integert,fixedt,sett])
	then
	      begin
		error ( 134 {"Illegal type of operands"})
	      end
	else
	case gattr.atype^.form of
	  scalar, booleant, chart, integert:
	    case fop of
	      plus:
		gattr.avalue.ival := fattr.avalue.ival + gattr.avalue.ival;
	      minus:
		gattr.avalue.ival := fattr.avalue.ival - gattr.avalue.ival;
	      mul:
		gattr.avalue.ival := fattr.avalue.ival * gattr.avalue.ival;
	      idiv:
		gattr.avalue.ival := fattr.avalue.ival div gattr.avalue.ival;
	      imod:
		gattr.avalue.ival := fattr.avalue.ival mod gattr.avalue.ival;
	      ltop:
		gattr.avalue.ival := ord(fattr.avalue.ival < gattr.avalue.ival);
	      leop:
		gattr.avalue.ival := ord(fattr.avalue.ival <= gattr.avalue.ival);
	      geop:
		gattr.avalue.ival := ord(fattr.avalue.ival >= gattr.avalue.ival);
	      gtop:
		gattr.avalue.ival := ord(fattr.avalue.ival > gattr.avalue.ival);
	      neop:
		gattr.avalue.ival := ord(fattr.avalue.ival <> gattr.avalue.ival);
	      eqop:
		gattr.avalue.ival := ord(fattr.avalue.ival = gattr.avalue.ival);
	      maxop:
		if fattr.avalue.ival > gattr.avalue.ival then
		  gattr.avalue.ival := fattr.avalue.ival;
	      minop:
		if fattr.avalue.ival < gattr.avalue.ival then
		  gattr.avalue.ival := fattr.avalue.ival;
	      ceilop, floorop: error(notyetimpl);
	      impliesop:			{ implication operator }
		if (fattr.avalue.ival <> 0) and (gattr.avalue.ival = 0) then
		  gattr.avalue.ival := 0	{ false }	
		else
		  gattr.avalue.ival := 1;	{ true }
	      andop:
		if (fattr.avalue.ival <> 0) and (gattr.avalue.ival <> 0) then
		  gattr.avalue.ival := 1
		else
		  gattr.avalue.ival := 0;
	      orop:
		if (fattr.avalue.ival <> 0) or (gattr.avalue.ival <> 0) then
		  gattr.avalue.ival := 1
		else
		  gattr.avalue.ival := 0
	      end; {case fop}

	  fixedt:
	    with gattr.avalue do
	     begin
				{ CASE statement changed to IF to remove others }
	      if fop = plus then
		  begin
		    rval := fattr.avalue.rval + rval;
		    prcsn := rmin(fattr.avalue.prcsn, prcsn)
		  end
	      else if fop = minus then
		  begin
		    rval := fattr.avalue.rval - rval;
		    prcsn := rmin(fattr.avalue.prcsn, prcsn)
		  end
	      else if fop = mul then
		  begin
		    prcsn := rmin(  abs(fattr.avalue.prcsn*rval),
				   abs(prcsn*fattr.avalue.rval) );
		    rval := fattr.avalue.rval * rval;
		  end
	      else if fop = sdiv then
		  begin
		    prcsn := abs( fattr.avalue.prcsn/rval );
		    rval := fattr.avalue.rval / rval;
		  end
	      else if fop = idiv then
		  error(134) 		{"illegal type of operands"}
	      else			{ OTHERS case }
		  error(notyetimpl);
	     end;

	  sett:
	   begin
	    if fop <= orop then begin
	      new(s);
	      s^ := gattr.avalue.sval^;
	      gattr.avalue.sval := s
	      end;
	    case fop of
	      plus:
		gattr.avalue.sval^ := fattr.avalue.sval^ + gattr.avalue.sval^;
	      minus:
		gattr.avalue.sval^ := fattr.avalue.sval^ - gattr.avalue.sval^;
	      mul:
		gattr.avalue.sval^ := fattr.avalue.sval^ * gattr.avalue.sval^;
	      inop:
		gattr.avalue.ival :=
		  ord(fattr.avalue.ival in gattr.avalue.sval^);
	      ltop, leop, geop, gtop, neop, eqop:
		error(notyetimpl)
	      end
	   end;

	end {case form}
    end {else opcode <> 0}
  end {not nil}
end {binop};
