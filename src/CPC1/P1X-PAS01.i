type
  lltype = 0..maxlevel;
  addrrange = longint;		{address type}
  fint =file of byte;

				{value information}
				{*****************}
  std =0..15;
  stndset = set of std; { ***TEMP Restriction*** }
  stndsetptr = ^stndset;	{ strong typing requires this }
  cstclass = (lit,data,reel,setc);
  valu = record case kind: cstclass of
    lit: (ival: longint);
    data: (daddr: longint);
    reel: (rval, prcsn: real);
    setc: (sval: stndsetptr)
  end;



 
				{lexical information}
				{*******************}
{---------------------------------------------------------|
| 							  |
|   The symbol table is a forest of binary trees with	  |
|  one tree for every scope. (A scope is defined by 	  |
|  a procedure, function, module, monitor or record.)	  |
|  The array display holds a description of all the	  |
|  trees in the forest, each entry containing a field	  |
|  (FNAME) pointing to the root of the corresponding	  |
|  tree, and a field (OCCUR) denoting the type of scope	  |
|  represented by the tree.				  |
|   Nodes in an identifier tree are described by the type |
|  IDENTS, declared below.  The fields LLINK and	  |
|  RLINK contain pointers to the subtrees containing	  |
|  those identifiers which come before and after, 	  |
|  respectively, the identifier stored in the 		  |
|  current node (in alphabetical order).  The pointer	  |
|  NEXT is used to form lists of identifiers in other	  |
|  than alphabetic order, for instance, to form the 	  |
|  parameter list of a procedure.  			  |
|   A single tree in the identifier forest is searched	  |
|  by the procedure SEARCHLEVEL.  The entire forest	  |
|  is searched by the procedure SEARCHID, which uses	  |
|  SEARCHLEVEL for the individual trees.  SEARCHID takes  |
|  as a parameter a set of identifier classes, allowing	  |
|  it to search for only identifiers of a certain class,  |
|  for example, for only type identifiers.		  |
|   New nodes are added to an identifier tree by the	  |
|  procedure NEWID.  NEWID will only add identifiers to   |
|  the tree representing the current local scope.	  |
|							  |
|---------------------------------------------------------}


				{basic symbols}
				{*************}
  symbol = (ident,intconst,fixconst,stringconst,notsy,mulop,addop,
            relop,lparen,rparen,lbrack,rbrack,comma,semicolon,
            period,colon,becomes,constsy,typesy,varsy,
            programsy,proceduresy,functionsy,setsy,packedsy,arraysy,
            recordsy,devicesy,forwardsy,beginsy,ifsy,casesy,repeatsy,
            whilesy,forsy,withsy,endsy,elsesy,
            untilsy,ofsy,dosy,tosy,downtosy,thensy,whensy,externalsy,
            eofsy,othersy,fixedsy,precisionsy,valuesy,raisesy,
	    exportsy,importsy,initsy,modulesy,monitorsy,prioritysy,
					{ verifier symbols follow }
	    assertsy,depthsy,effectsy,entrysy,exitsy,extrasy,returnsy,
	    invariantsy,measuresy,proofsy,rulesy,statesy,summarysy,
	    oldsy);

  operatorenum = (mul,andop,sdiv,idiv,imod,plus,minus,orop,ltop,leop,
              geop,gtop,neop,eqop,inop,maxop,minop,ceilop,
	      floorop,impliesop,noop);

  symtype = record
              sy: symbol;
              op: operatorenum
            end;
  buffer= packed array[1..alfaleng] of char;
  idtype = record
	     l: 0 .. alfaleng;	{length of identifier}
	     s: buffer 
	   end;
  idp = ^idtype;

  itp = ^idents;		{pointer to identifier information}

				{form of types:}
				{**************}
				   {NOTE: enumerative forms (or those
				          which have enumerative subranges)
					  are declared BEFORE longint,
					  Non-enumerative forms are
					  declared after.		}
  forms = (scalar, booleant, chart, integert, longintt, fixedt, xcptnt,
	   signalt,  pointer, sett, arrayt, recordt, devicet,  tagfield, variant);

				{type information:}
				{*****************}
  stp = ^struct;		{pointer to type information}
  struct = record
    size: longint;
    marked: boolean;	{used by printtables}
    tserial: longint;	{ external type identification}
    typeid: itp;			{ link to id for named types }
    case form: forms of
    scalar, booleant, chart, integert, longintt: (
      maxconst: itp;
      maxvalue: longint;		{ upper bound }
      minvalue: longint);		{ lower bound }
    fixedt: (
      rlow,rhigh,precsn: real);
    signalt: (
      addresspresent: boolean;
      trapvec: addrrange );
    pointer: (
      eltype: stp);
    sett: (
      settyp: stp);
    arrayt: (
      aeltyp,		{ .. element type }
      inxtyp: stp);	{ .. index type }
    recordt: (
      fstfld: itp;		{first ident in fieldlist}
      recvar: stp);		{pointer to tag type if present}
    devicet: (
      fstdfld: itp;		{ptr to first ident in fieldlist}
      devvar: stp;		{pointer to variants or NIL}
      addressed: boolean;	{true if device address specified}
      devaddr: addrrange  );	{specified address}
    tagfield: (
      fstvar: stp;		{pointer to first variant}
      tagfld: itp;
      tagtyp: stp);
    xcptnt: (			{ exception }
				{ currently an empty variant }
	);
    variant: (
      varval: longint;		{tagfield value for this variant}
      firstvfld: itp;		{1st idnt in fieldlst for this varnt}
      nxtvar,			{next variant in record containing this vrnt}
      subvar: stp)		{analog of recvar for the variant fieldlist}
  end;
				{identifier classes:}
				{*******************}
  classes = (types, konst, vars, field, proc, modul, xports);
  setclass = set of classes;

				{kinds of variables:}
				{*******************}
  varkinds = (local, param, formal);
 
				{verifier class of variables:}
  verclass = (executablevar, extravar);
  verclassdoers = executablevar..extravar; { subclass of ones for statements}


				{kinds of procedures:}
				{********************}
  pkinds = (decl, stnd, forw, extn);
  unittype = ( main, proctyp, xproctyp, modtyp, montyp );
  mkinds = modtyp..montyp; { module or monitor }

				{identifier information:}
				{***********************}

  chain = ^link;     		{  import/export list  }
  link =  record
	    this:  itp;		    {points to identifier definition}
	    next: chain
	  end;
		{Note: In the following record the pointers llink and
			rlink are used to alphabetize identifiers for
			easy lookup.  The pointer Next is used to
			build special purpose lists of identifiers,
			for example, the parameter list of a procedure
			or the field list of a record.		}
  idents = record
    iserial: longint;	{serial number - used by PrintIdent}
    name: idp;	{address of identifier string}
    llink, rlink: itp;	{pointers to build binary tree}
    itype: stp;		{pointer to type information}
    next: itp;		{used to build lists of identifiers}
    fileser: longint;		{ file number in which declared (for diags) }
    lineser: longint;		{ line number on which declared (for diags) }
    case klass: classes of
    types: ();
    konst: (kvalue: valu);
    vars:(
      vkind: varkinds;
      vlev: lltype;
      vaddr: addrrange;
      typserial: longint;	{type serial number for this variable}
      vclass: verclass);	{ normal, EXTRA, or FREE }
    field:(
      fvclass: verclassdoers;	{ EXTRA or executable }
      case ispacked: boolean of
      false: (fdisp: addrrange);
      true: (bdisp: longint));
    proc:(
      pvclass: verclassdoers;	{ EXTRA or executable }
      restrictor: itp;	{ if this proc is exported from a monitor, then
			  ^ to the monitor name  else  NIL   }
      case pkind: pkinds of
      decl: (
	plev: lltype;
	paddr: addrrange);
      stnd: (
	psinx: longint);
      extn: (
	pxinx: longint));
    modul:(
      exportlist, importlist: chain;
      exclusion : mkinds;		{module or monitor}
      mprio:  0..maxprio;		{priority level (if monitor)}
      case mkind: pkinds of
	decl: (
	  mlev: lltype;
	  maddr: addrrange);
        extn: (
	  mxinx: longint)  );
    xports: (
      enclosedident: itp;	{^ to ident in scope from which it's exported}
      fromclass: classes)
  end;   {ident}

				{scope control information}
				{*************************}
  disprange = 0..maxdis1;	{display index range}
  scopetype = (blck, modyul, rcrd);
  rctype = (crec, cdev, vrec);


  attributestates = (cst, ref, exp);
  accessmodes = (direct, byvalue, offset, indirect, indexed, absolute,subfield);
			{absolute access is for device types}

			{ attributes of expressions }
			{***************************}
  attr = record		{ attributes of expressions }
    atype: stp;
    case akind: attributestates of
      exp: (
	    );
      cst: (
	avalue: valu;
	cstnamed: boolean);			{ true if named constant }
      ref: (
	access: accessmodes;
	alevel: lltype;
	addr: longint)
    end;

  signalinfo = record
		  varaddr: addrrange;	{address allocated to signal VARIABLE}
		  varlev:  lltype;	{level at which sig is declared}
		  hardwired: boolean;
		  vecaddr: addrrange	{address of interrupt vector (hardware) }
		end;

  chartype = (ctl,oth,dig,xhex,let,quo,db0,db1,db2,db3,eos,s00,s01,s02,s03,
     s04,s05,s06,s07,s08,s09,s11,s12);
  chartabtype = array[char] of chartype;
  chartoktab = array[s00..s12] of symtype;
  keywords = array[1..nrkeywords] of record
    id: record
	  l: 0 .. alfaleng;	{length of identifier}
	  s: packed array [1..kywdlen] of char
	end;
    sym: symtype
  end;
  nametab = array[0..nrbuiltin1] of idtype;
  sizetables = array[forms] of longint;
  binoptab = array[forms,operatorenum] of 0..255;
  doargtab = array[0..nrbuiltin1] of set of (hasarg, getarg);
  litshfttab = array[0..auword] of longint;
  options =array['A'..'Z'] of boolean;

		{type which defines file parameters passed by GET_DIRECTIVES}
		{***********************************************************}

 pack5 = packed array[1..5] of char;
 pack6 = packed array[1..6] of char;
 pack9 = packed array[1..9] of char;
 pack80 = packed array[1..80] of char;

{
	Verifier-language types
}
compoundtally = 0..256;				{ count for SEQ operator }
{
	vmodeswitches  --  switches determining what is allowed in
			  any given verifier statement mode
}
vmodeswitches = record
	generatecode: boolean;			{ generates real code }
	variableref: array [verclass] of boolean; { is ref allowed? }
	variablechg: array [verclass] of boolean; { is chg allowed? }
	paramallowed: boolean;			{ can mention formal param? }
	localallowed: boolean;			{ can mention local var? }
	globalallowed: boolean;			{ can mention global var? }
	bodyallowed: boolean;			{ body, ENTRY, EXIT allowed? }
	end;
{
	vermodes  --  verification statement modes 
}
vermodes = (codemode, proofmode, assertmode, entrymode,
		exitmode, effectmode, invariantmode, rulemode);

refchg = (reference, change, referenceandchange); { read/write modes }


