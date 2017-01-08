procedure WHATaugment; const WHAT = '@(#)p2augment.i	2.4'; begin SINK := WHAT; end;	{ Version 2.4 of 1/6/83 }
{
	Tree Augmentation

	Tasks:

	1.  	Resolve ambiguous size information.  
		For VARBL, PARAM, and DIVAD operators, the
		size information in the operator may be overridden
		by the operator operating on the VARBL, PARAM, or
		DIVAD.  In such a case, the size in the VARBL, etc.
		is set to 0 to indicate that the address, rather than
		the contents, of the variable are wanted.

	2.	Determine types and names of operands
		For VARBL, PARAM, and DIVAD operators, the operand 
		type and name has already been obtained. 
		Where the size of the variable is known, the field
		of the variable exactly matching the address and size
		is associated with the operator.  Where the size is
		unknown (0), the name of the lowest depth data construct
		starting at the indicated address is associated with the
		operator.  For example, if a record begins at address X,
		an operator with an 0 size is considered to be addressing
		the entire record.

	3.	Fold FIELD operators.
		When a FIELD operator refers directly to a VARBL, PARAM,
		or DIVAD, the node containing the FIELD is replaced by
		a VARBL, PARAM, or DIVAD node constructed to refer to
		the exact field wanted.  Since, in the icode tree, all
		addresses and sizes are in bits, all nodes can represent
		fields and the FIELD operator can be eliminated.
}
{
	fixfield  --  collapse FIELD operator into VARBL, etc. below
}
procedure fixfield(p: ptn);		{ fix FIELD operator }
var q: ptn;				{ argument below }
    more: boolean;			{ continuation flag }
begin
    assert(p <> nil);			{ expression may not be nil}
    more := true;			{ continue until finished }
    while (p^.code in [fieldop, ofsetop]) and more do 
					{ continue until all collapsed }
    with p^ do begin			{ using FIELD node }
	assert(nrarg = 1);		{ one argument }
	q := arg[1];			{ get first argument }
					{ if resizable operator }
	if (q^.code in [varblop, paramop, dvadop, rdataop, fieldop, ofsetop]) 
	   or ((disp = 0) and (size = 0)){ or redundant operator }
	then begin 			{ collapse out field/ofset operator }
					{ VARBL, etc node will be used for scr }
					{ then will be eliminated }
	    if q^.vtype <> nil then begin { if already typed }
	       	q^.vtype := findfield(q^.vtype,disp,0); { compute new type }
		end;
	    q^.size := size;		{ put size into new VARBL node }
	    q^.disp := disp + q^.disp;  { compute total displacement }
	    arg[1] := nil;		{ clear link to VARBL }
	    p^ := q^;			{ replace contents of FIELD node }
	    disposenode(q);		{ get rid of VARBL, etc. node }
	end else begin			{ field/ofset, but not collapsable }
	    more := false;		{ stop collapse }
	    end;
	end;				{ end field operator }
end {fixfield};
{
	WITH statement and reference management

	WITH expressions are literally expanded in-line by removing the
	RTEMP operators and replacing them with pointers to the 
	expression referred to.

	This machinery is invoked during both set/used analysis
	and during tree augmentation.
}
{
	fixdtemp -- record DTEMP (WITH expression) values for later use
}
procedure fixdtemp(p: ptn);
begin
    with p^ do       			{ using given node }
    if (disp < 0) or (disp > rtempmax) then begin { if out of range }
	badnode(p,43);			{ "rtempmax" limit exceeded }
    end else begin			{ in range }
	rtemptab[disp] := arg[1];	{ record WITH arg for references }
	end;
end {fixdtemp};
{
	fixrtemp  --  fix up reference to WITH object
}
procedure fixrtemp(p: ptn);		{ node to be fixed }
var tempnum: bitaddress;		{ actually a number, not an address }
    w: ptn;				{ working icode node }
begin
    assert(p <> nil);			{ must exist }
    with p^ do begin			{ using given node }
	assert(code = rtempop);		{ must be RTEMP }
	tempnum := disp;		{ which temp from WITH }
	if (tempnum < 0) or (tempnum > rtempmax) then begin { if out of range }
	    badnode(p,39);		{ "rtempmax" limit exceeded }
	    tempnum := 0;		{ avoid subscript out of range }
	end;
	w := rtemptab[tempnum];		{ relevant WITH expression }
	if w = nil then begin 		{ if no WITH clause found }
	    verybadnode(p,40);		{ cannot find associated WITH }
	end else begin			{ WITH clause found }
	    {	The following line has major implications.  
		The rtempop node is overwritten with the node
		describing the expression from the WITH clause.
		This turns the tree into a directed graph.
		It has the effect of a macro expansion interpretation
		of a WITH.
	    }
	    p^ := w^;			{ replace entire rtemp node }
	    end;
	end;				{ end WITH }
end {fixrtemp};
{
	clearrtemptab  --  clear WITH memory
}
procedure clearrtemptab;
var i: 1..rtempmax;
begin
    for i := 1 to rtempmax do rtemptab[i] := nil; { forget prev WITH info }
end {clearrtemptab};
{
	fixliter  --  check machine type for literal operands
}
procedure fixliter(p: ptn; need: machinetype);	{ node to be fixed }
begin
    with p^ do begin				{ using given node }
	assert(code = literop);			{ must be literal }
	mtype := need;				{ make literal kind wanted }
	with mttab[mtype] do begin		{ validate literal value }
	    if disp > mtmax then badnode(p,51);	{ literal too big }
	    if disp < mtmin then badnode(p,52);	{ literal too small }
	    if (disp = 0) and (not mtzerook) then { literal 0 when disallowed }
		badnode(p,54);			{ literal 0 when disallowed }
	    end;
	end;
end {fixliter};
{
	fixparam  --  change param operands to a machine type of
		      "pnt" if by-reference argument
}
procedure fixparam(p: ptn);
begin
    with p^ do begin				{ using given node }
	if vtype <> nil then begin		{ if vtype present }
	    if vtype^.vardata.by = byreference then begin { if ref arg }
		mtype := pnt;			{ this node is a pointer }
		size := 0;			{ and its size is 0 }
		end;
	    end;
	end;
end {fixparam};
{
	fixactual  --  fix actual arg to match formal arg in size

	When one passes a value by address, an ambiguity exists as to
	the level of record object referred to which can be resolved
	only when the size of the object becomes available.
	Fixactual imposes the size required by the called procedure.
	The refer case in selectors will fill in the vtype later.
}
procedure fixactual(p: ptn;			{ node to fix }
		    fsize: sizeinbits);		{ size in bits }
begin
    assert(fsize > 0);				{ must have real size }
    with p^ do begin				{ using node to fix }
	if fsize <> size then begin		{ if sizes differ }
						{ must be resizable op }
	    if not (mtype in [data,addr]) then	{ if not data-valued }
		badnode(p, 161);		{ refer size clash }
	    size := fsize;			{ impose new size }
	    end;   
	end;
end {fixactual};
{
	fixcall  --  fix call to function

	Calls to functions are converted from the form

		call	fn
	to
	 	fcall   resulttype
		 call	fn
	by generating a new lower node and modifying the old one.
}
procedure fixcall(p: ptn);			{ given call node }
var fnode: ptn;					{ working node }
begin
    with p^ do begin				{ using given node }
        if (segnr = 0) and			{ avoid doing this twice }
        (isfunction(p^.vtype)) then begin	{ if is a function }
	    newnode(fnode,callop);		{ generate lower node }
	    fnode^ := p^;			{ copy old into lower }
	    fnode^.segnr := 1;			{ avoid fixcall twice }
						{ modify upper node }
	    code := fcallop;			{ op is fcall }
	    nrarg := 1;				{ one arg }
	    arg[1] := fnode;			{ links to call node }
	    disp := 0;				{ disp not meaningful }
	    vtype := fnode^.vtype^.down;	{ vtype is result of fn }
	    mtype := vtype^.varmtype;		{ mtype from vtype }
	    size := vtype^.vardata.size;	{ size from vtype }
	    end;				{ conversion of lower node }
	end;					{ With }
end {fixcall};
{
	fixdef  --  fix defined operators

	Size of defined operator applies to object being referenced,
	not self.  This applies to DEFND and DEFAR operators.  Since
	these operators can apply to large objects, they need a 
	sizing force.
}
procedure fixdef(p: ptn);
begin
    with p^ do begin
	assert((code = defndop) or (code = defarop));
	arg[1]^.size := size;		{ impose size on arg 1 }
	size := 1;			{ our size is 1 bit (boolean) }
	end;
end {fixdef};
{
	selectors  --  handle selector operators

	All operators of class "slcti" are handled by this routine.
}
procedure selectors(p: ptn;		{ node }  
		    need: machinetype);	{ needed type }
var q: varnodep;			{ working variable node }
begin
    with p^ do begin			{ using given node }
    case code of			{ for various selector operators }
    referop: begin			{ turns data into pnt }    
	if need = ind then begin	{ if caller wants dummy indirect }
					{ used for DTEMP and for passing actual}
	    vtype := arg[1]^.vtype;	{ use vtype from below }
	    mtype := ind;		{ and force ind as type here }
	end else begin			{ if explicit pointer expected }
	    badnode(p, 58);		{ must need ind for refer }
	    end;			{ end pointer }
	end;

    ofsetop: begin			{ fixed offset - in record }
	vtype := findfield(arg[1]^.vtype,disp,size); { calc offset item }
	end;

    indirop: begin			{ turns pnt into data }
					{ only applies to pointers }
	if arg[1]^.mtype = ind then begin { if dummy pointer }
					{ appears in WITH expansions }
	    vtype := arg[1]^.vtype;	{ take type from below }
	end else begin			{ if real pointer vtype }
	    if arg[1]^.vtype^.vardata.form <> pointerdata then 
	        badnode(p,48)		{ expected pointerdata }
	    else vtype := arg[1]^.vtype^.down; { get item pointed to }
	    end;			{ end real pointer }
	vtype := findfield(vtype,disp,size); { handle subfield }
	end;

    rtempop: begin			{ reference to data from WITH }
	fixrtemp(p);			{ fix up reference to WITH }
	end;

    indexop: begin			{ array subscript calculate }
					{ must be array data }
	{  
	    The purpose of this calculation is to find the first array
	    starting at the address being indexed.  Consider the possiblity
	    that the first component of a record is an array.  In such
	    a case, this code will descend the structure tree until the
	    array item is found.
	}
	q := arg[1]^.vtype;		{ get type of operand }
	if (q = nil) then verybadnode(p,36);{ do not know type of index object }
	while (q^.vardata.form <> arraydata) do begin
	    q := q^.down;		{ get first child at same addr }
	    if q = nil then verybadnode(p,37); { cannot find relevant array }
					{ must be at same address }
	    if q^.vardata.loc.address <> 0 then 
		badnode(p,34);		{ no relevant array at given address }
	    end;
	assert(q^.vardata.form = arraydata); { must be array now }
	vtype := q^.down;		{ get array element }
	end;

    fieldop: begin			{ field of other than varbl }
	vtype := findfield(arg[1]^.vtype,disp,size); { get type for this op }
	end;

    rdataop,
    dvadop,
    varblop,
    signlop,
    paramop: begin			{ data objects }
	end;				{ no action required }

    oldop: begin				{ <variable>.old }
	vtype := arg[1]^.vtype;		{ take vtype from below }
	end;				{ of old }

	end;				{ of cases }
	if vtype = nil then verybadnode(p,38); { node not properly typed }
	if nrarg > 0 then begin		{ check form of selector expr }
	    if optab[arg[1]^.code].opclass <> slcti then 
		badnode(arg[1], 127);	{ non-selector inside selector expr }
	    end;
	end;				{ with }
end {selectors};
{
	augment1  --  augment the icode tree 
}
procedure augment1(p: ptn;			{ starting node }
		   need: machinetype);		{ type needed by caller }
var i: 1..maxarg;				{ for loop }  
    j: 1..5;					{ position in optab arg list }
{
	augmentcall  --  handle calls to user-defined procedures
}
procedure augmentcall(p: ptn;			{ starting node }
		      need: machinetype);	{ type needed by caller }
var i: 1..maxarg;				{ for loop }
    q: varnodep;				{ argument being considered }
    parg: ptn;					{ pointer to arg being worked }
begin
    with p^ do begin				{ using given node }
	assert(code = callop);			{ must be a call }
	q := firstformal(vtype);		{ get first formal arg }
	for i := 1 to nrarg do begin		{ for all args given }
	    if q = nil then badnode(p,46) 	{ too many args in call }
	    else begin				{ for this arg }
		parg := arg[i];			{ get arg entry }
		case q^.vardata.by of		{ kind of argument }
		byactualvalue: begin		{ by value }
		    if parg^.mtype = data then begin { if data object }
		        fixactual(parg, q^.vardata.size); {use size from formal}
			end;
		    augment1(parg,q^.varmtype); { use given type }
		    end;
		byreference: begin		{ by reference }
		    if parg^.code <> referop then begin 
			badnode(p,53);		{ ref arg not passed by ref }
		    end else begin		{ proper ref arg }
			if q^.vardata.form <> pointerdata then begin
			    badnode(p,50);	{ not pointer at ref arg }
			    end;
			assert(q^.down <> nil);	{ must be at bottom }
			{ use real size from formal arg declaration }
			fixactual(parg^.arg[1],q^.down^.vardata.size);
			augment1(parg, ind);	{ augment refer expression }
						{ check type equivalence }
			end;			{ end proper ref arg }
		    end;			{ end by reference }
		end;				{ end cases }	
		q := q^.right;			{ advance to next arg }
		end;
	    end;				{ end arg loop }
	if q <> nil then badnode(p,47);		{ too few args in call }
	end;					{ of with }
end {augmentcall};
begin {augment1}
    if p = nil then begin			{ if no node }
	assert(need = xxx);			{ only for non-value }
    end else					{ node exists }
    with p^ do begin				{ using given node }
						{ task 1 }   
	if (need in [addr,pnt]) and (mtype = data) then mtype := need;
	if mtype = addr then size := 0;		{ force size to 0 for address }
						{ special case modifications }
	if code = fieldop then fixfield(p); 	{ collapse FIELDs }
	if code = ofsetop then fixfield(p); 	{ collapse OFSETs }
	if code = dtempop then fixdtemp(p);	{ record DTEMPs }
	if code = literop then fixliter(p,need);{ fix types of literals }
	if code = paramop then fixparam(p);	{ check for by-ref param }
	if code = callop  then fixcall(p);	{ fix call node }
	if code = defndop then fixdef(p);	{ fix defnd node }
	if code = defarop then fixdef(p);	{ fix defar node }
						{ repeat task 1 after fixups }
	if (need in [addr,pnt]) and (mtype = data) then mtype := need;
	if mtype = addr then size := 0;		{ force size to 0 for address }
	with optab[code] do begin		{ using relevant table }
	    if opclass = nonoi then verybadnode(p,31); { unimplemented icode op}
	    if code in 				{ special cases } 
		[lockop, callop, stolop, stofop, entryop] then begin
		case code of			{ fan out }
		callop: augmentcall(p,need);	{ call }
		oldop, 				{ old attribute }
		lockop: begin			{ lock }
		    augment1(arg[1],need);	{ mtype is that of caller }
		    mtype := need;		{ take mtype from above }
		    end;
		stofop, stolop: begin		{ general store operator }
		    augment1(arg[1],data);	{ output is always data }
		    assert(arg[1]^.vtype <> nil);{ selector insures this }
		    augment1(arg[2],arg[1]^.vtype^.varmtype); { input per out }
		    end;
		entryop: begin			{ case entry }
		    if nrarg < 1 then verybadnode(p,404); { entry has no args }
		    for i := 1 to nrarg - 1 do 	{ for case constants }
			augment1(arg[i],i16);	{ augment case constant }
		    augment1(arg[nrarg],xxx); 	{ augment statement part }
		    end;
		end;				{ end special cases }
	    end else begin			{ general case }
	        if opcount >= 0 then begin	{ if known operand count }
		    if nrarg <> opcount then 
			verybadnode(p,33); 	{ arg count wrong in tables }   
		    for i := 1 to opcount do begin	{ for all operands }
		        augment1(arg[i],opargs[i]);	{ parse operand }
		        end;
	        end else begin			{ if variable operand count }
		{
			For built-in operators with over five arguments,
			the last two entries in oparg are used over
			and over.  This mechanism applies to
			    loop
			    case
			    seq
		}
		    for i := 1 to nrarg do begin { using arg list }
			if i <= 5 then j := i 	{ if 1..5, i else 4 or 5 }
				  else j := (i mod 2) + 4;
			augment1(arg[i], opargs[j]); { do operand }
			end;
		    end;			{ end variable arg count }
		end;				{ end general case }
	    end;				{ end WITH }
	{
		After-descent processing
	}
	{
		For variable-valued objects, tag the node with the
		appropriate structure information
	}
	if optab[code].opclass = slcti then begin { if selector }
	    selectors(p,need);			{ handle selectors }
	    end;
	if (need = pnt) and (mtype = ind) then	{ if special WITH case }
	    need := ind;			{ accept ind }
	{
		If the node has a size and is variable-valued, descend
		the variable structure to find the correct size item
		beginning at this address.  This resolves ambiguities
		of the form "does address nnn refer to 'a' or 'a.b' or 
		'a.b[0]'".
	}
	if (size <> 0) and (vtype <> nil) then begin
	    if vtype^.vardata.form <> functiondata then begin
	        vtype := findfield(vtype,0,size);	{ bind proper field }
	        if vtype = nil then badnode(p,163);	{ bad subfield extract }
		end;				{ end not function }
	    end;
	if mtype = data then begin		{ if untyped data }
	    if vtype = nil then begin		{ if no variable type }
		badnode(p,55);			{ "data" ambiguous }
	    end else begin			{ vtype available }
	        if mttab[need].mtvalued then begin {if arithmetic value needed }
		    vtype := findsimpletype(vtype); { find underlying } 
		    mtype := vtype^.varmtype 	{ get machine type from var }
		    end;
		end;				{ end vtype available }
	    end;				{ end untyped data }
	{
	    At this point, every node that has and needs a value should
	    have a valid mtype.  If the node is a selector ("data"), it
	    should also have a vtype.
        }
						{ finish by type checking }
	if need <> mtype then begin		{ if possible type clash }
	    if mtype = xxx then badnode(p,49);	{ need val, have stmt }
	    if mtype in [ind,unp,xxx,pnt,data] then begin { if special }
		badnode(p,41) 			{ bad non-value }
	    end else if mttab[mtype].mtkind <> mttab[need].mtkind then begin 
		badnode(p,42);			{ machine type clash }
		end;
	    end;
	end;					{ end WITH }
end {augment1};
{
	augment  --  perform augmentation phase on one section of code.
		     Applied to both routines and stored vdecl sections.
}
procedure augment(vtree: ptn);			{ tree to be augmented }
begin
    clearrtemptab;				{ clear WITH memory }
    augment1(vtree,xxx);			{ begin recursive descent }
end {augment};
