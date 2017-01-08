procedure WHATbuild; const WHAT = '@(#)p2build.i	2.11'; begin SINK := WHAT; end;	{ Version 2.11 of 4/6/83 }
{
	buildtree  --  build the code tree from the icode file

	Builds the tree for one routine per call.
} 
procedure buildtree;
 
	{special code values: call, varb, and parm all have the form

			+____+____+
			|    |    |
			+____+____+
			   |    |
			   |    Lex level referred to
			  "code"				}


const stacksize=256;
var
  s: 0..stacksize;
  stack: array[1..stacksize] of ptn;
  argn,addrn: integer;  coden: byte;
  naddr: addressitem;	{ working address }
  temp: ptn;
  sizen: sizeinbits;	{ size of operand in bits }
  segn,ch,t: byte;
  psuedo: boolean;	{ do not generate node if set }
  currentfile: integer;	{ current file number for diagnostics }
  currentline: integer;	{ current source line number for diagnostics }
  programunittype: integer; { procedure, monitor, etc. }
  prioritylev: integer;	{ priority of monitor }
  scaleit: scalp;	{temp to point to scaling information}
  i: integer;		{ for loops }
  lastblock: blocknode;	{ block info from end operator }
  blk: varnodep;	{ working entry for block varnode }
  needlookup: boolean;	{ true if node has address needing lookup }
  sink: integer;	{ sink for dumping unwanted getword/getbyte }
{
	getbyte  --  get an 8-bit byte from the icode file
}
function getbyte: byte;
  begin 
    getbyte:=int^; 
    get(int) 
end{getbyte};
{
	getword  --  read a 16-bit word from the intermediate file
}
function getword: integer;
var temp: integer;
begin
 temp:=int^*256; get(int);
 temp:=int^+temp; get(int);
 getword := temp
end {getword};
 
begin {buildtree}
s := 0;					{ stack is clear }
currentfile := 0;			{ at source file zero }
currentline := 0;			{ at source line zero }
firstline := nulllineinfo;		{ no first line info yet }
repeat
  coden := ord(getbyte);    
      segn:=0; addrn:=0; sizen:=0;	{so subtreematch will work}
      argn:=0;  scaleit:=nil;
      psuedo := false;			{ true if psuedo-op }
      needlookup := false;		{ true if has address }
      case coden of
      176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191, {varblop}
      192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207: {paramop}
      begin segn:=(coden mod 16) 	{ segn = lex level};
	segn := depthconv(segn);	{ convert to block serial number }
        sizen:=getbyte*bitsperadrunit;	{ size in bits }
	addrn:=getword;			{ address in bytes }
					{ collapse code value} 
	if coden < paramop then coden:=varblop else coden:=paramop;
					{ find name of variable }
	case coden of			{ kinds of operators }
	varblop: begin			{ variable }
	    if segn = 0 then begin	{ if in outermost block }
		naddr.relocation := absoluteaddr { global }
	    end else begin		{ if in inner block }
		naddr.relocation := stackaddr;   { local }
	        addrn := extractsigned(addrn);   { and signed }
	        end;
	    end;
	paramop: begin			{ if formal parameter }
	    naddr.relocation := paramaddr;	{ parameter }
	    addrn := extractsigned(addrn);      { and signed }
	    end;
	end;				{ end cases }
	addrn := addrn * bitsperadrunit;{ convert address to bits }
	naddr.blockn := segn;		{ block number }
	naddr.address := addrn;		{ address, may be negative number }
	needlookup := true;		{ cause lookup later }
      end;
      dvadop:
	begin
          sizen:=getbyte*bitsperadrunit;	{ size in bits }
	  addrn:=getword*bitsperadrunit;	{ address in bits }
						{ find variable name }
    	  naddr.relocation := deviceaddr;	{ construct address item }
	  naddr.address := addrn;		{ device type at disp } 
	  naddr.blockn := 0;			{ block number irrelevant }
	  needlookup := true;			{ address lookup needed }
	end;
      litscop: begin
          sizen:=getbyte*bitsperadrunit;	{ size in bits }
	  addrn:=getword*bitsperadrunit;	{ address in bits }
	end;
      literop: begin
	  addrn := extractsigned(getword); 	{ take signed value }
	  sizen := 2*bitsperadrunit;		{ size in bits }
	end;
      rdataop: begin				{ from VALUE declaration }
	  sizen := bitsperadrunit*ord(getbyte);	{ get size }
	  addrn := getword*bitsperadrunit; 	{ address in bits }
    	  naddr.relocation := valueaddr;	{ construct address item }
	  naddr.address := addrn;		{ address in ROM }
	  naddr.blockn := 0;			{ block number irrelevant }
	  needlookup := true;			{ address lookup needed }
	end;
      litdop : internalerror(9);		{ floating point deimplemented};
      rtempop,dtempop : begin addrn:=ord(getword);
        if coden=dtempop then argn:=2 end;
      fieldop:					{ bit field extract }
      	begin
	  sizen := getbyte;  addrn := getword;
	  argn := 1
	end;

      ofsetop,indirop: begin			{ addressing operators }
          sizen:=getbyte*bitsperadrunit;	{ size in bits }
	  addrn:=getword*bitsperadrunit;	{ address in bits }
	  argn := 1;				{ single operand }
	  end;

      indexop: begin				{ array indexing }
          sizen:=getbyte*bitsperadrunit;	{ size in bits }
	  addrn:=getword*bitsperadrunit;	{ address in bits }
	  argn := 2;				{ array, index }
	  end;

      movemop: begin				{ move operation }
						{ NOTE reversed operands }
          addrn:=getbyte*bitsperadrunit;	{ element size in bits }
	  sizen:=getword*bitsperadrunit;	{ total size in bits }
	  argn := 2;				{ destination, source }
	  end;

      vceqop,vcneop,vcgtop,vcleop,vcgeop,vcltop: begin { vector operations }
          sizen:=getbyte*bitsperadrunit;	{ size in bits }
	  addrn:=getword*bitsperadrunit;	{ address in bits }
	  argn := 2;				{ operands }
	  end;

      entryop,loopop,seqop {n-ary}: argn:=ord(getbyte) + 1;
      caseop: begin
	  argn := ord(getbyte);			{ arg count }
	  addrn := ord(getword);		{ max case value }
	  end;
      invokop: begin argn:=ord(getbyte); addrn:=ord(getword);
          end;

      208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223: {call}
       begin 
					{ level of call is irrelevant }
	coden:=callop; 			{ collapse code value}
        sizen:=getbyte*bitsperadrunit;	{ size of function result in bits }
        argn:=ord(getbyte);		{ number of arguments }
	addrn := getword;		{ address is procedure number }
        naddr.relocation := routineaddr;{ construct address item }
	naddr.blockn := addrn;		{ routine number as block number } 
	naddr.address := 0;		{ address irrelevant }
	needlookup := true;		{ address lookup needed }
       end;

      224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239: {icall}
      begin  
	addrn := ord(getbyte);
	coden := icallop;
        naddr.relocation := routineaddr;{ construct address item }
	naddr.blockn := addrn;		{ routine number as block number } 
	naddr.address := 0;		{ address irrelevant }
	needlookup := true;		{ address lookup needed }
	end;

      forop:  
	begin argn:=5; end;

      ifop {tertiary}: 
	begin argn:=3; end;

      stolop,stofop,
      uceqop,ucneop,ucgtop,ucleop,ucgeop,ucltop,umaxop,uminop,
      iaddop,isubop,imulop,idivop,imodop,
      ceilop,floorop,
      saddop,ssubop,smulop,sdivop,
      iceqop,icneop,icgtop,icleop,icgeop,icltop,imaxop, iminop,
      faddop,fsubop,fmulop,fdivop,
      fxeqop,fxneop,fxgtop,fxleop,fxgeop,fxltop,fxmaxop,fxminop,
      fceqop,fcneop,fcgtop,fcleop,fcgeop,fcltop,fmaxop,fminop,
      eqvop,xorop,nimpop,rimpop,impop,nrimpop,orop,andop,
      unionop,interop,
      sdiffop,sadelop,sceqop,scneop,scgtop,scleop,scgeop,scltop,
      inop,exitop:
	begin {binary}
	  argn:=2;
	  sizen:=2*bitsperadrunit
	  end;

      resclop:
	begin
	  new(scaleit);
	  with scaleit^ do
	    begin
	      low.scale:=getbyte; low.mantissa:=getword;
	      high.scale:=getbyte; high.mantissa:=getword;
	      delta.scale:=getbyte; delta.mantissa:=getword;
	    end;
	  argn:=1;  sizen:=2*bitsperadrunit
	end;

      succop,predop,inegop,iabsop,ioddop,
      fnegop,fabsop,floatop,truncop,roundop,notop,complop,
      sgensop,sanyop,waitop,sendop,tsigop,raiseop:
	{unary}
        begin argn:=1; sizen:=2*bitsperadrunit end;

      referop: begin			{ unary address take }
	argn := 1;
	end;

      lockop: begin 
	  addrn:=getbyte; 		{priority} 
	  argn := 1;			{ takes call as operator }
          end;

      {
	Initialization operators  --  used only at startup
      }
      signlop: begin			{ device signal location }
	addrn := getword;		{ device address of signal }
	addrn := addrn * bitsperadrunit;{ convert to bits }
	argn := 1;			{ arg is signal variable }
	sizen := 2 * bitsperadrunit;	{ size is 2 bytes }
	naddr.relocation := deviceaddr;	{ address of device }
	naddr.address := addrn;		{ interrupt vector address }
	naddr.blockn := 0;		{ block irrelevant }
	needlookup := true;		{ cause lookup }
	end;

      isgnlop: begin			{ signal clear operator }
	argn := ord(getbyte);		{ N args }
	end;

      blockop: begin			{ block placeholder }
	argn := ord(getbyte);		{ get number of args }
	end;

      xhndlop: begin			{ exception header declaration }
	argn := ord(getbyte);		{ number of args }
	end;

      {
	Verifier operators  --  for verifier use only
      }
      defarop: begin			{ defined(<array> <bound> <bound>) }
	argn := 3;			{ 3 args }
        sizen:=getword*bitsperadrunit;	{ size in bits }
	end;

      vinitop: begin			{ defined(<modulename>) predicate }
	argn := 0; sizen := 0;
	addrn := ord(getbyte);		{ get procedure number }
        naddr.relocation := routineaddr;{ construct address item }
	naddr.blockn := addrn;		{ routine number as block number } 
	naddr.address := 0;		{ address irrelevant }
	needlookup := true;		{ address lookup needed }
	end;

      measop: begin			{ measure statement }
	argn := 1; sizen := 0; end;
	
      depthop: begin			{ depth statement }
	argn := 1; sizen := 0; end;

      defndop: begin			{ "defined" psuedo-function }
        sizen:=getword*bitsperadrunit;	{ size in bits }
	argn := 1;  end;

      oldop: begin			{ "old" attribute }
	argn := 1; sizen := 0; end;

      vheadop:
	begin argn:=2; sizen:=0; end;

      vdeclop:
	begin argn:=1; addrn:=getbyte; {assertion type}; end;

      asertop:
	begin argn := getbyte; addrn := getbyte; {assertion type}; end;

    {
	Psuedo-operators - do not generate tree nodes
    }
    stnumop: begin			{ statement number for diags }
	psuedo := true;			{ no node }
	sink := getbyte;		{ discard 3 bytes }
	sink := getbyte;		{ discard 3 bytes }
	sink := getbyte;		{ discard 3 bytes }
	end;

    linenop: begin			{ source line number for diags }
	psuedo := true;			{ no node }
	currentfile := getbyte;		{ current file number }
	currentline := getword;		{ applies until further linenop }
	if firstline.linenumber = 0 then begin { if no first line yet }
	    firstline.linenumber := currentline; { first line of junit }
	    firstline.filenumber := currentfile; { first file of junit }
	    end;
	end;

    nullop: begin
	psuedo := true;			{ does not generate a node }
	if s>= stacksize-1 then begin 
	    internalerror(2); 		{ arg stack overflow }
	    s := 0; 
	    end; { avoid overflow }
	s := s + 1;			{ push nil on stack }
	stack[s] := nil;		{ this is a nil operand }
	end;

    xchop: begin 
	psuedo := true;
					{swap(stack[s],stack[s-1])}
	temp:=stack[s]; stack[s]:=stack[s-1]; stack[s-1]:=temp; 
	end;

    delop: begin 			{ delete top node on stack }
	psuedo := true;
  	if s>0 then begin 
	    temp := stack[s];		{ pop stack once }
	    s:=s-1;
	    if temp <> nil then disposenode(temp); { dispose of unwanted node }
	end else begin
	    internalerror(1);		{ arg stack underflow }
	    end;
	end;
	
    fixop: begin 
	  psuedo := true;		{ non-node operator }
	  new(scaleit);
	  with scaleit^ do
	    begin
	      low.scale:=getbyte;  low.mantissa:=getword;
	      high.scale:=getbyte;  high.mantissa:=getword;
	      delta.scale:=getbyte; delta.mantissa:=getword;
	      stack[s]^.scalefactor:=scaleit
	    end
	  end;

    monitop: begin  
	  psuedo := true;		{ non-node operator }
	  programloadtype := ord(getbyte);
	  t := ord(getbyte);		{ unittype + 16*priority }
	  programunittype := t mod 16;	{ monitorunit or moduleunit }
	  prioritylev := t div 16;	{ priority of monitor }
					{ enter new block }
	  pushblock(blocksequence, programunittype, prioritylev, false); 
	  blocksequence := blocksequence + 1;
	  end;

    identop: begin 			{ identifier in icode }
	psuedo := true;			{ non-node operator }
	argn:=ord(getbyte); namesize:=0;
	for i := 1 to 15 do name[i] := ' '; { clear name space }
        while argn>0 do
	  begin
            if namesize<15 then namesize:=succ(namesize);
            ch:=getbyte; name[namesize]:=chr(ch);
            argn:=pred(argn)
          end
       end;

    procop: begin 			{ proc, fn, or main program }
	psuedo := true;			{ non-node operator }
	programloadtype := ord(getbyte);
	programunittype := getbyte;	{ type of procedure }
					{ get priority from next outer blk }
					{ adjust static nesting }  
	pushblock(blocksequence, programunittype, unknownpriority, true);
	blocksequence := blocksequence + 1;
	end;

    endop: begin 
	psuedo := true;				{ non-node operator }
	lastblock.blpin := ord(getbyte);	{ procedure number }
	lastblock.blrsize := ord(getbyte)*8; 	{ size of function result }
	lastblock.bllsize := getword*8;		{ local var size }
	lastblock.blpsize := getword*8;		{ local param size }
	lastblock.bldsize := getword*8;		{ data size as of this proc }
	lastblock.blscopedepth := scopedepth;	{ scope depth of block }
						{ unit type of block }
    						{ look up routine entry }
	blockend(lastblock);		{ file information about block }
	if scopestack[scopedepth].nonscopes = 0 then
 	    if lastblock.blpin <> scopestack[scopedepth].scopepin  then begin
	    internalerror(6);		{ scope number tally is off }
	    end;
	if s < 1 then internalerror(1);	{ too few args for end }
	s := s - 1;			{ pop stack once }
	popblock;			{ exit a scope }
        end;
    end; {pseudo ops}
    if not psuedo then begin			{ if node required }
      newnode(tree,coden);			{ allocate new node }
      with tree^ do begin			{ build the node }
	size := sizen;
 	disp:=addrn; 
        segnr:=segn; 
	nrarg:=argn;
	scalefactor:=scaleit;
	linen.filenumber := currentfile;	{ source file number for diags }
	linen.linenumber := currentline;	{ source line number for diags }
	mtype := optab[code].opresult;		{ set assumed result type }
	if needlookup then begin		{ if item has address }
	    vtype := findvar(tree, naddr);	{ look up variable node }
	    end;
	if optab[code].opcount >= 0 then 	{ if exact arg count }
	    if argn <> optab[code].opcount then 
		badnode(tree,15); 		{ arg count table bad }
        if s<argn then begin 
	    verybadnode(tree,1);		{ arg stack underflow }
	    end;
        while argn>0 do begin			{ build N-ary tree }
          arg[argn] := stack[s]; argn := argn-1; s := s-1
          end;
        end;
    if s>=stacksize then begin 
	verybadnode(tree,2);			{ arg stack overflow }
	end;
    s := s+1;
    stack[s] := tree
    end {not a pseudo op}
until coden = endop;
{
	routine has been read - perform post-processing
}
    if s <> 0 then begin		{ stack not entirely popped }
        if debugg then begin		{ if debugging }
            writeln(dbg,'*** ICODE STACK NOT EMPTY ***'); 
            writeln(dbg,'*** ',s:1,' items remain on icode stack. ***');
            while s > 0 do begin	{ print all the extra stuff }
	        writeln(dbg,'s[',s:1,']:');
	        treeprint(stack[s],0);	{ print extras, indented slightly }
	        s := s - 1;		{ pop }
	        end;
	    end;			{ end debug output on }
       verybadnode(tree,4);		{ icode stack depth error }
       end;
end {buildtree};
