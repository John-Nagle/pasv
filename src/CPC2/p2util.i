procedure WHATutil; const WHAT = '@(#)p2util.i	2.3'; begin SINK := WHAT; end;	{ Version 2.3 of 12/6/85 }
{
	node allocation  -- handles icode node allocation/deallocation
}
{
	newnode  --  allocate new icode node
}
procedure newnode(var p: ptn; ncode: byte);		{ returns pointer }
begin
   new(p);				{ get new node }
   nodesallocated := nodesallocated + 1;{ count nodes allocated }
   with p^ do begin			{ using new node }
 	code := ncode;			{ indicates node type}
 	size := 0;
	linen.linenumber := 0;		{ source line number for debug }
	linen.filenumber := 0;		{ source file number for debug }
 	segnr := 0;			  
        nrarg := 0;			{ number of args in arg }
 	disp := 0;			{ address of var, or subcode } 
	scalefactor := nil;
	vtype := nil;			{ link to relevant variable def }
        mtype := unp;			{ machine type }
	refcount := 0;			{ for garbage collection }
	ndfntemp.ftstamp := 0;		{ no timestamp }
	ndfntemp.fttemp := 0;		{ no temp id }
	end;
end {newnode};
{
	disposenode  --  dispose of old icode node
}
procedure disposenode(var p: ptn);		{ node to get rid of }
begin
    assert(p <> nil);
    assert(nodesallocated > 0);		{ nodes must be allocated }
    if p^.scalefactor <> nil then dispose(p^.scalefactor); { get rid of addn }
    dispose(p);				{ get rid of node }
    nodesallocated := nodesallocated - 1;{ decrement allocation count }
end {disposenode};
{
	clearrefcount  --  clear ref counts in tree nodes

	This will work for an ordered network as well as a tree
}
procedure clearrefcount(p: ptn);
var i: 0..maxarg;
    q: ptn;						{ arg pointer }
begin
    if p <> nil then with p^ do begin			{ using given node }
	refcount := 0;					{ clear ref count }
	for i := 1 to nrarg do begin			{ for all subnodes }
	    q := arg[i];				{ get next node }
	    if q <> nil then clearrefcount(q);		{ mark subtrees }
	    end;
	end;
end {clearrefcount};
{
	calcrefcount  --  compute reference counts           
}
procedure calcrefcount(p: ptn);
var i: 0..maxarg;
    q: ptn;						{ arg pointer }
begin
    if p <> nil then with p^ do begin			{ using given node }
	refcount := refcount + 1;			{ number of references }
	if refcount = 1 then 				{ only for first ref }
	for i := 1 to nrarg do begin			{ for all subnodes }
	    q := arg[i];				{ get next node }
	    if q <> nil then calcrefcount(q);		{ mark subtrees }
	    end;
	end;
end {calcrefcount};
{
	relsubtree  --  release a subtree                

	Calcrefcount must be run first to put ref counts in all nodes
}
procedure relsubtree(p: ptn);
var i: 0..maxarg;
    q: ptn;					{ arg pointer }
begin
    if p <> nil then begin
	with p^ do begin			{ using given node }
	    assert(refcount > 0);		{ loop in graph will trip this }
	    refcount := refcount - 1;		{ decrement ref count }
	    if refcount = 0 then begin		{ if no pointers left to node }
	        for i := 1 to nrarg do begin	{ release all subtrees }
	            q := arg[i];		{ get next node }
	            if q <> nil then relsubtree(q);	{ mark subtrees }
		    end;			{ end for loop }
	        end;				{ end refcount=0 }
	    end;				{ end with }
	if p^.refcount = 0 then disposenode(p);	{ release node outside with }
	end;					{ end non-nil }
end {relsubtree};
{
	disposetree  --  dispose of a code tree
}
procedure disposetree(p: ptn);			{ tree to get rid of }
begin
    clearrefcount(p);				{ clear reference counts }
    calcrefcount(p);				{ calc new ref counts }
    relsubtree(p);				{ get rid of subtree }
end {disposetree};
{
	checknonodes  --  make sure no nodes allocated

	This is a consistency check on pointer manipulation only.
}
procedure checknonodes;
begin
    if not seriouserror then begin		{ if no big errors so far }
	if nodesallocated <> 0 then begin	{ if any nodes allocated }
	    writeln(nodesallocated:1,' icode nodes still allocated');
	    internalerror(10);			{ icode node allocation error }
	    end;
	end;
end {checknonodes};
{
	utilities for different types of nodes
}
{
	newblocknode  --  allocate a new block node
}
procedure newblocknode(var n: blocknodep);	{ returned block node }
begin
    new(n);					{ allocate a block node }
    with n^ do begin				{ clear block node }
	blvarnode := nil;		
	blrefs := nil;		
	blcallers := nil;		
	blassertions := nil;		
	bldepthexpr := nil;
	blouterblock := nil;	
	bldominator := n;			{ initial dominator is self }
	blshared := unknownshared;
	blrecursive := false;		
	blpin := 0;			
	blblockdepth := 0;		
	bldsize := 0;		
	blpsize := 0;		
	bllsize := 0;		
	blrsize := 0;		
	blunittype := 0;		
	bldoeswait := false;
	bldoessend := false;
	bldoesdevio := false;
	blhasbody := false;
	blhasoutputvararg := false;
	blpriority := unknownpriority;
	blfnkind := unknownroutine;	
	blexamined := false;		
	blpassnum := 0;
	blnext := nil;		
	end;
end {newblocknode};
{
	newblock  --  allocate block node and link
}
procedure newblock(v: varnodep);		{ relevant varnode }
var blk: blocknodep;				{ new block node }
begin
    assert(v <> nil);				{ must exist }
    newblocknode(blk);				{ get a blank block node }
    with v^ do begin				{ using varnode }
	assert(blockdata = nil);		{ no block data yet }
	blockdata := blk;			{ link block to var info }
	varblock := blk;			{ owning block is init to self }
	with blk^ do begin			{ using new block node }
	    blvarnode := v;			{ link var info to block }
	    blpin := vardata.loc.blockn;	{ get procedure number of block}
	    if vardata.loc.relocation <> routineaddr then { if not routine }
		badvarnode(v,267);		{ bad block number for block }
	    end;
	if blockhead = nil then begin		{ if first block }
	    blockhead := blk;			{ this is head }
	    blocktail := blk;			{ this is also tail }
	end else begin				{ if not first }
	    assert(blocktail^.blnext = nil);	{ tail must be nil }
	    blocktail^.blnext := blk;		{ chain to end }
	    blocktail := blk;			{ this is new end }
	    end;
	end;
end {newblock};
{
	newvarnode  --  allocate a new varnode

	Initializes all fields to constant values
}
procedure newvarnode(var vp: varnodep);	{ returned node }
begin
    new(vp);				{ acquire a new var item }
    with vp^ do begin    		{ using the new item }
	varmtype := xxx;		{ no machine type yet }
	lesser := nil;			{ clear search pointer }
	greater := nil;			{ clear search pointer }
	up := nil;			{ clear back pointer }
	down := nil;			{ clear pointer to subitems }
	right := nil;			{ clear pointer to next subitem }
	balance := 0;			{ clear balance count }
	freezecount := 0;		{ clear freeze count }
	blockdata := nil;		{ clear block info for blocks }
	varblock := nil;		{ no owning block yet }
	varmaster := nil;		{ no master block yet }
	varset := false;		{ never set }  
	varused := false;		{ never used }
	varshared := unknownshared;	{ sharing info }
	actuallist := nil;		{ no actuals tied to formal }
	examined := false;		{ not examined yet }
	end;				{ of With }
end {newvarnode};
{
	newcallnode  --  allocate and clear new callnode
}
procedure newcallnode(var v: callnodep);	{ returned call node }
begin
    new(v);				{ allocate new call node }
    with v^ do begin			{ using new node }
	clnext := nil;			{ next on chain }
	clblock := nil;			{ calling node }
	end;
end {newcallnode};
{
	newargnode  --  allocate and clear new argnode
}
procedure newargnode(var v: argnodep);	{ returned arg node }
begin
    new(v);				{ allocate new arg node }
    with v^ do begin			{ using new node }
	arnext := nil;			{ next on chain }
	aractual := nil;		{ actual param base variable }
	end;
end {newargnode};
{
	JCODE utility routines
}
{
	nextlabel  --  next available label number
}
function nextlabel: labelid;
begin
   labelserial := labelserial + 1;
   nextlabel := labelserial;			{ return next available }
end {nextlabel};	
{ 
	basevariable  --  return underlying variable of vtype

	The underlying variable is
	1) the variable itself for simple variables
	2) the top level object for structures and arrays
	3) the top level data object for formal VAR arguments
}
function basevariable(v: varnodep): varnodep;
begin
    assert(v <> nil);					{ must exist }
    basevariable := v;					{ assumed result }
    with v^ do begin					{ using given varnode }
	if vardata.by = byreference then begin		{ if pointer to ref arg}
	    assert(vardata.form = pointerdata);		{ will be pointer }
	    basevariable := down;			{ return obj pointed to}
	end else begin					{ if not ref arg pnt }
	    if up <> nil then				{ if anything above }
	        if vardata.loc.relocation <> paramaddr then {and not a param }
		    if up^.vardata.form <> pointerdata then {and not a pointer }
		        basevariable := basevariable(up); { recurse upward }
	    end;					{ end non-ref case }
	end;						{ end WITH }
end {basevariable};
{
	isbasevar  --  true if item is base variable of self
}
function isbasevar(v: varnodep): boolean;
begin
    isbasevar := v = basevariable(v);			{ true if equal }
end {isbasevar};
{
	alwaysdefined  --  true for variables which are
			   inherently defined.  Constants and devices comply,
			   as do non-VAR formal parameters.
			   The returned-value of a function is also always
			   defined.

	NOTE that this depends on the fact that there is no way to make
	a defined variable undefined other than by re-entering its scope
	or using it as a FOR statement variable, and use as a FOR statement
	variable is forbidden for formal args.
}
function alwaysdefined(v: varnodep)		{ variable to test }
			: boolean;		{ true if always defined }
var basev: varnodep;				{ base of object }
begin
    basev := basevariable(v);			{ base of object }
    with basev^ do begin			{ using base }
	alwaysdefined := (vardata.by = byactualvalue) or
		(vardata.loc.relocation in [deviceaddr, valueaddr]);	
						{ these cannot be undefined }
	end;					{ With }
end {alwaysdefined};
{
	ifsubscripted  --  is selector expression subscripted?

	The expression is explored for an index operator.
	This routine depends on the fact that all selector operators
	other than index have only one argument.
}
function ifsubscripted(p: ptn)			{ selector expression }
		: boolean;			{ true if subscripted }
begin
    ifsubscripted := false;			{ assume not subscripted }
    while p <> nil do begin			{ scan left leaves }
        with p^ do begin			{ using given node }
	    assert(optab[code].opclass = slcti);{ must be selector }
	    if code = indexop then begin	{ if index }
		ifsubscripted := true;		{ this is subscripted }
		p := nil;			{ stop search }
	    end else begin			{ if not subscripted }
		if nrarg > 0 then begin		{ if more structure }
		   assert(nrarg < 2);		{ not more than one arg }
		   p := p^.arg[1];		{ descend to 1st arg }
		end else begin			{ if done }
		   p := nil;			{ stop search }
		   end;				{ end no args }
		end;				{ end not index op }
	    end;				{ end With }
	end;					{ end loop }
end {ifsubscripted};
{
	formalarg  --  return formal arg given proc and arg number

	Arg numbers are those used in calls.  The return value
	item is not counted.
	Arguments count from one.
}
function formalarg(proc: varnodep;		{ which procedure }
		   argnum: integer)		{ which arg }
		   : varnodep;			{ returned formal }
var n: 0..maxarg;				{ arg counter }
    q: varnodep;				{ working formal }
begin
    assert(proc^.vardata.form in [proceduredata, functiondata]);
    n := 1;					{ at arg 1 }
    q := proc^.down;				{ first arg }
    if isfunction(proc) then n := 0;		{ if first arg is returned val }
    while n < argnum do begin			{ for all args }
	if q = nil then internalerror(91);	{ actual arg num out of range }
	assert(q <> nil);			{ arg must exist }
	q := q^.right;				{ get next arg }
	n := n + 1;				{ increment arg count }
	end;
    if q = nil then internalerror(91);		{ actual arg num out of range }
    formalarg := basevariable(q);		{ return formal base }
end {formalarg};
{
	firstformal  --  get first formal argument to routine

	If the routine is a function, the first item in the
	formal arg chain describes the returned value.  This
	item is skipped by firstformal.
}
function firstformal(procitem: varnodep)	{ procedure or fn item }
			: varnodep;		{ returned first item }
begin
    with procitem^ do begin			{ using given node }
	assert(vardata.form in [proceduredata, functiondata]);
	if isfunction(procitem) then begin	{ if function }
	    firstformal := down^.right; 	{ get second on chain }
	end else begin				{ if procedure }
	    firstformal := down;		{ get first on chain }
	    end;
	end;
end {firstformal};
{
	isformal  --  is arg formal arg to given routine?

	The returned value is considered a formal argument.
}
function isformal(v: varnodep;			{ variable to test }
		  blk: blocknodep)		{ given routine }
		  : boolean;			{ true if on param list }
var w: varnodep;				{ working varnode }
begin
    assert(isbasevar(v));			{ must look at base only }
    isformal := false;				{ assume no find }
    if blk^.blvarnode^.vardata.form in [proceduredata, functiondata] then begin 
	w := blk^.blvarnode^.down;		{ prepare to search formals }
        while w <> nil do begin			{ for all formals }
	    if basevariable(w) = v then begin	{ if find }
	        isformal := true;		{ this is a formal }
	        w := nil;			{ force escape }
	    end else begin			{ if not found }
	        w := w^.right;			{ on to next one }
	        end;
	    end;				{ While loop }
	end;					{ end is routine }
end {isformal};
{
	recindexsearch  --  record number lookup

	Takes symbol number from varnode and returns record number
	local to current junit.  0 indicates no find.
	This would be an unnecessarily slow process except that the
	number of record structures per junit tends to be rather 
	limited.
}
function recindexsearch(v: varnodep)		{ varnode to try }
		: recindex;			{ local record index }
var i: integer;					{ for search }
begin
    recindexsearch := 0;			{ assume no find }
    with v^ do begin				{ using given value }
	i := 1;					{ start search }
	while i <= rectab.rntop do begin	{ while in table }
	    if rectab.rntab[i] = vardata.recordnum then begin { if find }
		recindexsearch := i;		{ return result }
		i := rectab.rntop + 1;		{ force exit }
	    end else begin			{ if no find }
		i := i + 1;			{ continue search }
		end;				{ end no find }
	    end;				{ end While }
	end;					{ end With }
end {recindexsearch};
{
	addstmt  --  add item to statement chain

	Statement chains are n-ary trees built with "seq" operators
}
procedure addstmt(var chain: ptn; 		{ starting node }
		  p: ptn);			{ new node }
var w: ptn;					{ working new node }
begin
    if chain = nil then begin			{ if this is first one }
	chain := p;				{ this is all there is }
    end else with chain^ do begin		{ using chain node }
	if (code = seqop) and (nrarg < maxarg) then begin
						{ room for one more }
	    nrarg := nrarg + 1;			{ add one more }
	    arg[nrarg] := p;			{ new one in new slot }
	end else begin				{ if no room or not seq group }
	    newnode(w, seqop);			{ generate new sequence op }
	    w^.arg[1] := chain;			{ old chain first }
	    w^.arg[2] := p;	    		{ new item at end }
	    w^.nrarg := 2;			{ 2 items in seq }
	    w^.linen := chain^.linen;		{ line number }
	    w^.mtype := xxx;			{ is statement }
	    chain := w;				{ new seq heads chain }
	    end;
	end;					{ end With }
end {addstmt};
{
	visible  --  is variable visible in given block?
}
{ ***TEMPORARY IMPLEMENTATION BASED ON OUTERMOST REF IN CODE *** }
{ ***MORE INFORMATION FROM PASS I IS NEEDED TO IMPLEMENT THIS PROPERLY *** }
function visible(v: varnodep;			{ variable to test }
		 blk: blocknodep)		{ block to test against }
		 : boolean;			{ true if visible }
var b: blocknodep;				{ working block }
    owner: blocknodep;				{ owner of object }
begin
    assert(isbasevar(v));			{ must be base var }
    owner := v^.varblock;			{ get owning block }
    visible := false;				{ assume not visible }
    b := blk;					{ starting block for scan }
    while b <> nil do begin			{ check for visibility }
	if b = owner then begin			{ if find }
	    visible := true;			{ mark as visible }
	    b := nil;				{ stop search }
	end else b := b^.blouterblock;		{ otherwise continue search }
	end;
end {visible};
{
	ispresent  --  is node present in sequence?

	Used in invariant prophagation to prevent duplication.
}
function ispresent(seq: ptn;			{ sequence to examine }
		   p: ptn)			{ node to look for }
		   : boolean;			{ true if present }
var i: 0..maxarg;				{ for arg loop }
    find: boolean;				{ result }
begin
    find := false;				{ assume no find }
    if seq <> nil then with seq^ do begin	{ using given node }
	for i := 1 to nrarg do begin		{ for all args }
	    if not find then begin		{ if no find yet }
	        if arg[i]^.code = seqop then begin	{ if sequence }
		    find := ispresent(arg[i],p);{ search recursively }
		end else begin			{ if not seqop }
		    find := arg[i] = p;		{ true if find }
		    end;			{ end not seqop }
		end;				{ end no find }
	    end;				{ end for all args }
	end;					{ end nonnull node }
    ispresent := find;				{ return found/not found }
end {ispresent};
{
	initdummies  --  initialize dummy node items
}
procedure initdummies;
begin
						{ dummy type for DEPTH/MEASURE }
    newvarnode(cardinalvarnode);		{ dummy type 0..32767 }
    with cardinalvarnode^ do begin		{ using dummy node }
	with vardata do begin			{ using varfile part }
	    itemdepth := 1;			{ top level item }
	    form := numericdata;		{ is number }
	    by := bynothing;			{ non argument }
	    loc.address := 0;			{ address meaningless }
	    loc.blockn := 0;			{ block number meaningless }
	    loc.relocation := absoluteaddr;	{ relocation meaningless }
	    size := 16;				{ takes 16 bits }
	    itemname := 'DEPTH VALUE';		{ dummy name }
	    recordname := '          ';
	    minvalue := 0;			{ range 0..32767 }
	    maxvalue := targetintegermax;
	    scale := 0;				{ integer }
	    vrsource.linenumber := 0;		{ no line number }
	    vrsource.filenumber := 0;		{ no file number }
	    end;
	varmtype := u15;			{ 0..32767 }
	end;					{ end outer With }
    newvarnode(booleanvarnode);			{ dummy type boolean }
    booleanvarnode^ := cardinalvarnode^;	{ get initial value }
    with booleanvarnode^ do begin		{ using dummy node }
	with vardata do begin			{ using varfile part }
	    form := booleandata;		{ is boolean }
	    size := 1;				{ 1 bit }
	    itemname := 'LOOP FLAG';		{ dummy name }
	    maxvalue := 1;			{ max value }
	    end;
	varmtype := b1;				{ boolean }
	end;					{ end outer With }
{
	Dummy icode expressions  -  used when a generator routine requires
	an icode expression.
}
						{ integer zero }
    newnode(zeroexpr,literop);			{ liter 0 }
    with zeroexpr^ do begin			{ using new node }
	disp := 0;				{ value 0 }
	mtype := u15;				{ type unsigned integer }
	end;					{ with }
						{ boolean true }
    newnode(trueexpr,literop);			{ liter true }
    with trueexpr^ do begin			{ using new node }
	disp := 1;				{ value true }
	mtype := b1;				{ type boolean }
	end;					{ with }
end { initdummies };
{
	tick  --  advance clock used for detecting side effect clashes
}
procedure tick;
begin
    clockserial := clockserial + 1;		{ advance clock }
end {clockserial};
{
	Driving routines  --  apply procedure param to all objects
			      of a given class
}
{
	vardrive  --  apply to every variable
}
procedure vardrive(procedure eachvar(v: varnodep));	{ called for each var }
procedure vardrive1(q: varnodep);			{ node to descend from }
begin
    with q^ do begin					{ using given node }
	if lesser <> nil then vardrive1(lesser);	{ do subtree }
	eachvar(q);					{ process this node }
	if greater <> nil then vardrive1(greater);	{ do subtree }
	end;
end {vardrive1};
begin {vardrive}
    if vartree^.greater <> nil then			{ if tree not nil }
        vardrive1(vartree^.greater);			{ start tree descent }
    { Don't need lesser.  See varfile routines }
end {vardrive};
{
	seqdrive  --  process all statements in "seq" list
}
procedure seqdrive(p: ptn;			{ starting node }
		    procedure doseq(st: ptn));	{ arg to pass }
var i: 1..maxarg;				{ working arg position }
    parg: ptn;					{ working arg }
begin
    if p <> nil then				{ if work to do }
    with p^ do begin				{ appears to work }
	if code = seqop then begin		{ if sequence op }
	    for i := 1 to nrarg do begin	{ for all args }
		parg := arg[i];			{ get this arg }
		if parg <> nil then begin	{ if valid arg }
		    seqdrive(parg, doseq);	{ recurse to handle }
		    end;			{ end non-null arg }
		end;				{ end arg loop }
	end else begin				{ if non sequence op }
	    doseq(p);				{ process this statement }
	    end;
	end;					{ end With }
end {seqdrive};
{
	varinexprdrive  --  find all variables in expression
}
procedure varinexprdrive(expr: ptn;			{ expression }
			 procedure dovar(v: varnodep));	{ call on find }
procedure vdrive(p: ptn);				{ expression to scan }
var i: 1..maxarg;				{ for loop }
begin
    if p <> nil then with p^ do begin		{ using given node }
        if code in [varblop, dvadop, paramop] then begin{ if variable }
	    dovar(basevariable(vtype));		{ do this variable }
        end else      	 			{ if not variable }
	    for i := 1 to nrarg do vdrive(arg[i]); { recurse }
	end;					{ end With }
end {vdrive};
begin {varinexprdrive}
    vdrive(expr);				{ start recursion }
end {varinexprdrive};
{
	innerblkdrive  --  scan modules and monitors for inner 
			   modules.

	This is meaningful only for monitors, modules, and main programs.
}
procedure innerblkdrive(blk: blocknodep;		{ starting block }
		procedure inner(b: blocknodep));	{ use on inners }
var b: blocknodep;					{ working block }
begin
    assert(blk^.blvarnode^.vardata.form in 		{ must be static blk }
	[programdata, moduledata, monitordata]); 
    b := blockhead;					{ chain through blocks }
    while b <> nil do begin				{ chain through blocks }
	with b^ do begin				{ using this ref }
	    if blouterblock = blk then begin		{ if immediately inner }
	        if blvarnode^.vardata.form in 		{ if INIT applicable }
		    [moduledata, monitordata] then begin
		    if blhasbody then begin		{ if has init part }
			inner(b);			{ process inner blk }
		    end else begin			{ if no init part }
			innerblkdrive(b,inner);		{ recurse inward }
			end;				{ end no init part }
		end else begin				{ not module }
		    if not (blvarnode^.vardata.form in 	{ check for routine }
			[proceduredata, functiondata]) then
			if blk^.blvarnode^.vardata.form <> programdata then
			   internalerror(303);{non routine/module inside module}
		    end;				{ end module/monitor }
		end;					{ end immediately inner}
	    end;					{ end With }
	b := b^.blnext;					{ on to next block }
	end;						{ end block scan }
end {innerblkdrive};
{
	blockcrossdrive  --  drive for all blocks crossed from inner to
			     outer.  Outer block must be on the path from
			     the inner block.
			     Used in invariant and call processing.
}
procedure blockcrossdrive(inner, outer: blocknodep;	{ bounds of scan }
			  procedure docross(b: blocknodep)); { crossed blk }
var b: blocknodep;				{ working block }
begin
    assert(inner <> nil);			{ inner must be real }
    assert(outer <> nil);			{ outer must be real }
    if inner <> outer then begin		{ if useful range }
	b := inner^.blouterblock;		{ skip inner block }
	while b <> outer do begin		{ for all intermediate blks }
	    docross(b);				{ do this block }
	    b := b^.blouterblock;		{ go outward one block }
	    assert(b <> nil);			{ did not find outer }
	    end;
	end;					{ not null range }
end {blockcrossdrive};
{
	blockdrive  --  do once for each block
}
procedure blockdrive(procedure d(b: blocknodep));	{ do on each block }
var blk: blocknodep;				{ working block }
begin
    blk := blockhead;				{ get first block }
    while blk <> nil do begin			{ for each block }
	d(blk); 				{ do this block }
	blk := blk^.blnext;			{ get next block }
	end;
end {blockdrive};
{
	functinexprdrive  --  find all functions in expression
}
procedure functinexprdrive(expr: ptn;			{ expression }
			 procedure dovar(v: varnodep));	{ call on find }
procedure fdrive(p: ptn);				{ expression to scan }
var i: 1..maxarg;				{ for loop }
begin
    if p <> nil then with p^ do begin		{ using given node }
	if code = callop then 			{ if call }
	    if isfunction(vtype) then		{ if function }
		dovar(vtype);			{ handle it }
	for i := 1 to nrarg do fdrive(arg[i]); { recurse }
	end;
end {fdrive};
begin {functinexprdrive}
    fdrive(expr);				{ start recursion }
end {functinexprdrive};
