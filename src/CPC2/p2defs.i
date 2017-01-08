  bit = 0..1;
  byte = 0 .. 255;      { change to subrange to reflect reality - ECN}
  word = 0 .. 65535;
  sizeinbits = 0..addressmax;		{ size of target objects in bits }
  targetinteger = targetintegermin..targetintegermax; { target machine int }
  loadtype = 0..loadtypemax;	{ RCODE marker for loadtype }
  unittype = 0..unittypemax;	{ RCODE marker form programunittype }
					{ 0 - main program	}
					{ 1 - procedure/function}
					{ 2 - exported proc/fn	}
					{ 3 - module		}
					{ 4 - monitor		}



			{Fixed point data structures}
			{***************************}
  fixedvalue = packed record
		    scale: byte;
		 mantissa: word
		end;
  scaler = record  low,high,delta: fixedvalue  end;
  scalp = ^scaler;

  srep  = record	{fixed point representation descriptor}
	    point: -31 .. 31;
	    magnitude: integer
	  end;

{
	Generally useful types
}
   cardinal = 0..maxint;			{ nonnegative integer }
   string6 = packed array [1..6] of char;
   string15 = packed array [1..15] of char;
   string60 = packed array [1..60] of char;
{
	types of the i-machine

	Note that more restrictive types must precede less restrictive
	compatible types, for search by getmtype.
}
   machinetype = (xxx,				{ illegal where appears }
		  unp,				{ unimplemented }
		  data,				{ address with possible size }
		  addr,				{ address with unknown size }
		  b1,				{ booleans }
		  u4, u7, u8, u15, u16,		{ unsigned integers }
		  ubig,				{ oversize unsigned integer }
		  i8 ,i16,			{ signed integers }
		  ibig,				{ oversize signed integer }
		  ni7, ni8, ni16,		{ signed integers less 0 }
		  nu7, nu8, nu15, nu16,		{ unsigned integers less 0 }
		  f16,				{ fixed point }
		  sig,				{ signal }
		  ind,				{ indirect reference }
		  pnt,				{ pointer }
		  s16);				{ set }

{
	kinds of object references 
}
varrefkind = (useref,				{ USE }
	      setref,				{ SET }
	      initref, 				{ called by INIT }
	      varref,				{ passed as VAR arg }
	      fcallref,				{ called as function }
	      pcallref);			{ called as procedure }
reflistkind = useref..pcallref;			{ represented in reflist items }
setofreflistkind = set of reflistkind;		{ set of above }

mentionkind = (directmention,			{ ref by this one }
	       transitivemention);		{ ref by transitive closure }
{
	Kind of variable object to be generated
}
gennewmode = (genwithoutnew, genwithnew);	{ is NEW! needed? }
gendefmode = (genwithoutdef, genwithdef);	{ is DEFINED! needed ? }
{
	priority

	Big numbers preempt little numbers.
}
priority = unknownpriority..maxpriority;	{ range of priorities }
{
	Kinds of routines

	A rule function is strictly for intercommunication with the
	rule builder.  Rule functions consist only of a function declaration
	without body or assertions.

	A pure function is a pure function in the mathematical sense.
	Its output depends only on the value of its actual parameters.

	A safe routine has no side effects.  References to global variables
	are allowed.

	Any routine not in the above classes is a general routine.

	Initially, all functions are unknown functions, but during
	transitive closure, all functions are assigned a kind.
}
routinekind = (unknownroutine, generalroutine, 
	saferoutine, purefunction, rulefunction);
{
	pinnum  --  "procedure number" or actually block number from pass 1

	0 is the main program.
}
pinnum = 0..maxpin;
{
	sharedinfo  --  information about sharing between processes
			for variables.
			This information appears in basevars only.

			The value is the procedure number of the
			owning process or one of the constants 
			"unknownshared" or "isshared".
}
sharedinfo = unknownshared..maxpin;		{ sharing info }
{
	Types for unique object generation
}
tempid = 0..maxtempid;				{ uniqueness for TEMPnn temps }
labelid = 0..maxlabelid;			{ uniqueness for Jcode labels }
timestamp = 0..maxtimestamp;			{ uniqueness for time }
{
	Pointers to nodes  -- forward referenced items
}
ptn = ^node;				{ pointer to icode node }
varnodep = ^varnode;			{ pointer to varnode }
blocknodep = ^blocknode;		{ pointer to blocknode }
refnodep = ^refnode;			{ pointer to refnode }
callnodep = ^callnode;			{ pointer to call node }
argnodep = ^argnode;			{ pointer to arg node }
{
	varnode  --  variable tree node
}
varnode = record
	vardata: varitem;		{ data from file }
	varmtype: machinetype;		{ machine representation }  
	lesser: varnodep;		{ lesser in address sense }
	greater: varnodep;		{ to greater in address sense }
	up: varnodep;			{ owner at previous level }
	down: varnodep;			{ first child }
	right: varnodep;		{ next child }
	balance: -1..1;			{ for tree balancer }
	freezecount: cardinal;		{ count of freeze operations }
	idunique: cardinal;		{ discriminator for unique names }
	blockdata: blocknodep;		{ block information if block }
	varblock: blocknodep;		{ owning block of variable }
	varmaster: blocknodep;		{ outermost block that sets variable }
	varset: boolean;		{ assigned a value }
	varused: boolean;		{ referenced }
	varshared: sharedinfo;		{ shared between routines }
	actuallist: argnodep;		{ actuals tied to this formal }
	examined: boolean;		{ examined in VAR arg check }
	end;
{
	blocknode  --  one for each block
}
blocknode = record
	blvarnode: varnodep;		{ back pointer to var file }
	blrefs: refnodep;		{ head of ref chain }
	blcallers: callnodep;		{ those who call this block }
	blassertions: ptn;		{ associated assertions }
	bldepthexpr: ptn;		{ associated DEPTH expression }
	blouterblock: blocknodep;	{ next outer block or nil }
	bldominator: blocknodep;	{ dominator of callers }
	blshared: sharedinfo;		{ sharing info for block }
	blrecursive: boolean;		{ true if recursive }
	blpin: pinnum;			{ procedure number }
	blblockdepth: cardinal;		{ depth into block nesting }
	blscopedepth: cardinal;		{ depth into declaring scopes }
	bldsize: bitaddress;		{ data size (bits) }
	blpsize: bitaddress;		{ param size (bits) }
	bllsize: bitaddress;		{ local size (bits) }
	blrsize: bitaddress;		{ returned value size (bits) }
	blpriority: priority;		{ priority of block }
	blunittype: unittype;		{ proc/fn, monitor, etc. }
	blhasbody: boolean;		{ true if has code in body }
	bldoeswait: boolean;		{ true if does WAIT operation }
	bldoessend: boolean;		{ true if does SEND operation }
	bldoesdevio: boolean;		{ true if does DEVICE I/O }
	blhasoutputvararg: boolean;	{ true if has output VAR arg }
	blfnkind: routinekind;		{ kind of function }
	blexamined: boolean;		{ for descent routines }
	blpassnum: cardinal;		{ true if examined on this pass }
	blnext: blocknodep;		{ next block for chaining }
	end;
{
	callnode  --  one for each call

	Used to construct a backward chain of callers for each routine
}
callnode = record
	clnext: callnodep;		{ next on chain }
	clblock: blocknodep;		{ calling node }
	end;
{
	argnode  --  one for each VAR argument in each call

	Used to link formals back to actuals for the purpose of
	resolving whether a VAR argument is input, output, or
	input-output.
}
argnode = record
	arnext: argnodep;		{ next on chain }
	aractual: varnodep;		{ actual param base variable }
	end;

   iopclass = (nonoi,				{ illegal icode operator }
	      stmti,				{ statement-valued }
	      expri,				{ part of expression }
	      decli,				{ declaration operator }
	      litri,				{ literal constant }
	      slcti);				{ selector (variable-valued) }
{
	fntemp  --  function value temporary result marker
}
fntemp = record				
	ftstamp: timestamp;			{ clock cycle where attached }
	fttemp: tempid;				{ associated temp or none }
	end;
{
	node  --  icode tree node
}
  node = packed record
 	code: byte;			{indicates node type}
 	size: sizeinbits;
	linen: lineinfo;		{ source line number for debug }
 	segnr: byte;			{ misc. info field }
        nrarg: 0..maxarg;		{ number of args in arg }
 	disp: bitaddress;		{ address of var, or subcode } 
	scalefactor: scalp;
	vtype: varnodep;		{ link to relevant variable def }
        mtype: machinetype;		{ machine type }
	refcount: cardinal;		{ for garbage collection }
	ndfntemp: fntemp;		{ function temp value if present }
        arg: array[1..maxarg] of ptn
	end;

			{miscellaneous types}
			{*******************}

   compresult = (lesscomp, greatercomp, overlapcomp); { for comparing fields}

   subcodes = 1..20;				{ subcodes in assertions }
   subcodeset = set of subcodes;		{ set of subcodes }

{
	Precedence levels are used only for printing expressions in messages
	with proper parenthesization.  The order is significant.
}
precedence = (					{ Pascal-F precedence levels }
	 negationoperator,
	 multiplyingoperator,
	 addingoperator,
	 relationaloperator,
	 functional);				{ indicates function }
{
	Items for constant tables
}
mttabitem = record				{ properties of machine types }
	mtkind: datakind;			{ kind of data represented }
	mtmin: targetnumber;			{ minimum value }
	mtmax: targetnumber;			{ maximum value }
	mtzerook: boolean;			{ false requires <>0 check }
	mtvalued: boolean;			{ arithmetic value? }
	mtsimple: boolean;			{ simple type }
	end;

optabitem = record				{ icode operator table }
	opname: string6;			{ printable name }
	opclass: iopclass;			{ class of operator }
	opjcode: string15;			{ jcode representation if any }
	opresult: machinetype;			{ machine type of result }
	opcount: -1..5;				{ arg count; -1 means variable }
	opargs: array [1..5] of machinetype;	{ machine types of args }
	opmcode: string15;			{ message representation }
	opmpred: precedence;			{ message precedence }
	end;
{
	refnode -- used for constructing set/used lists
}
refnode = record			{ for one reference }
	refnext: refnodep;		{ next reference }
	refvar: varnodep;		{ object referenced }
	refkind: reflistkind;           { type of reference }
	refformal: varnodep;		{ for varref, formal referenced }
	refmention: mentionkind;	{ direct or transtiive mention }
	end;
{
	selector stack  --  used for accumulating selector expression
			    information in processable form
}
selkind = (arraysel, recordsel, variablesel);	{ kind of selection operation }
selstack = record
	top: 0..maxselstack;			{ stack depth }
	sellinen: lineinfo;			{ line number for debug }
	sedef: gendefmode;			{ use as "defined" }
	sesubstitute: boolean;			{ do substitutions on stvars }
	tab: array [1..maxselstack] of record
	    stkind: selkind;			{ "[]", ".", or "x" }
	    stvar: varnodep;			{ variable or field }
	    stsub: ptn;				{ subscript expression }
	    stold: boolean;			{ ref as x.old }
	    end;
	end;
{
	substitution stack -- used to indicate formal-actual bindings
			      and new! / defined! tagging
}
substituteposition = 0..maxsubstack;		{ for mark/release type work }

subitem = record				{ subs for one variable }
	sbvarkey: varnodep;			{ search key }
	sboldkey: boolean;			{ more search key }
	sbtempid: tempid;			{ temp id to subst or zero }
	sbnew: gennewmode;			{ mark with new! if true }
	end;
{
	Old args table  --  used to bind old arg dummies used in ".old"
	references in procedure blocks.  See headerpart routine.
}
oldargtab = record
    oldargcount: 0..oldargsmax;			{ position in oldargs }
    oldargs: array [1..oldargsmax] of record
	oavar: varnodep;			{ variable }
	oatnum: tempid;				{ temporary id }
	end;
    end;
{
	TEMPnn variable usage information

	TEMP variables are used in Jcode to save the values of various
	expressions for later use.
}
						{ usages of TEMP vars }
tempkind = (inputformaltemp, inputbaseactualtemp, inputglobaltemp, 
	    outputformaltemp,outputresulttemp);
setoftempkind = set of tempkind;		{ for selection set }

tempitem = record				{ usage of one TEMP var }
    tevarnode: varnodep;			{ relevant varnode }
    tekind: tempkind;				{ usage of temp }
    tenum: tempid;				{ which temp }
    end;

temptab = record				{ usage of all TEMPs of call }
    tttop: 0..temptabmax;			{ last used in temptab }
    tttab: array [1..temptabmax] of tempitem;	{ array of temp usages }
    end;
{
	subconsttab  --  constant subscript table

			 Used for generating subscript expressions referring
			 to a specific object.
}
subconsttab = record
	sutab: array [1..subconstmax] of targetinteger;	{ constant subscript }
	sutop: 0..subconstmax;				{ top of sutab }
	end;
{
	recordnumtab  --  record number table

		Used to convert symbol numbers from pass 1 to
		local (per-Junit) indices used in pass 2.
}
recindex = 0..recindexmax;				{ index to table }
recindextab = record
	rntab: array [1..recindexmax] of symbolnumber; { indexed by p2 nums }
	rntop: recindex;				{ top of rntab }
	end;
{
	file pathname
}
filepath = array [1..maxfilepath] of char;
{
	list item  -  used only for generating conjunctions and disjunctions
	in jcode
}
listitem = record
	lldepth: cardinal;			{ paren depth }
	llop: string15;				{ operator string }
	end;
