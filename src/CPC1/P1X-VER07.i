{
	VER06 -- verifier routines dealing with variables

	The routines here create the variables
	file used by pass 2 of the verifier.
}
{
	movename  --  move name, filling with spaces
}
procedure movename(id: idp;			{ id given }
		   var name: identifier);	{ filled name returned }
const blank = '               ';		{ 15-char spaces }
      anonname = 'UNNAMED_ITEM   ';		{ dummy identifier for field }
var i: 0..alfaleng;				{ for loop }
begin
    if id = nil then 				{ special case }
	name := anonname			{ use array element dummy }
    else
    with id^ do begin				{ using given name }
        name := blank;				{ space fill }
	for i := 1 to l do name[i] := s[i];	{ copy name part in use }
	end;
end {movename};
{
	putvitem  --  put out variable item
}
procedure putvitem(vitem: varitem);		{ item to put }
begin
    write(varfile,vitem);			{ write to var file }
end {putvitem};  
{
	vitembuild - build variable/type items for verifier
}
procedure vitembuild(vdepth: vitemdepth;	{ depth into type def }
		     vid: idp;			{ name being defined }
		     vloc: addressitem;		{ location in target mem }  
		     vtype: stp;		{ type of var or field }
		     vpack: boolean;		{ true if packing }
		     vby: paramkind;		{ kind of param }
		     vfile: integer;		{ file number }
		     vline: integer);		{ source line number }
const
    setelement = 'SET_ELEMENT    ';		{ dummy identifier }  
    anontype =   'UNNAMED_TYPE   ';		{ dummy record type }
    notypename = '               ';		{ no record type }
var                                                     
    vi: varitem;				{ working variable item }  
    fieldaddr: addressitem;			{ working address of field }
    wfield: itp;				{ for chaining  }
begin
    vi.itemdepth := vdepth;			{ set depth into type defn }
    movename(vid,vi.itemname);			{ set name of var }
    vi.recordname := notypename;		{ no type name }
    vi.recordnum := 0;				{ no type number yet }
    vi.loc := vloc;				{ set location of var }
    vi.by := vby;				{ param class if any }
    vi.vrsource.filenumber := vfile;		{ file number }
    vi.vrsource.linenumber := vline;		{ line number }
						{ handle type information }
    assert(vtype <> nil);			{ type must exist }
    with vtype^ do begin			{ using type of var }
	    if vpack then begin			{ if inside packed field }     
		vi.size := typwidth(vtype);	{ compute size of element }
	    end else begin			{ if not in packed field }
		vi.size := size*8;		{ size is in bytes }
	    end;
	    case form of			{ handle different forms }
	    scalar, chart, integert, longintt: begin { bounded }
		vi.form := numericdata;		{ record as numeric }
		vi.minvalue := minvalue;	{ move bounds }
		vi.maxvalue := maxvalue;	{ move bounds }
		vi.scale := 0;			{ unit of measure = 1 }
		putvitem(vi);			{ generate var item }
		end;
	    booleant: begin			{ Boolean }
	        vi.form := booleandata;		{ type is boolean }
		vi.minvalue := 0;		{ minimum is 0 }
		vi.maxvalue := 1;		{ maximum is 1 }
		vi.scale := 0;			{ unit of measure = 1 = 2**0 }
		putvitem(vi);			{ generate var item }
		end;
	    fixedt: begin			{ fixed point }
		vi.form := numericdata;		{ again, numeric }
		error(1050 {Fixed point UNIMPLEMENTED in verifier});
		vi.minvalue := 0; vi.maxvalue := 0; { ***TEMP*** }
		vi.scale := 0;			{ ***TEMP*** }
		{ ***NEED MORE INFO ABOUT FIXED POINT REPRESENTATION *** }
		putvitem(vi);			{ generate var item }
		end;
	    sett: begin				{ set type }
		vi.form := setdata;		{ indicate set }
		assert(settyp^.form in [scalar, booleant, chart, integert,
					longintt]); { legal set forms }
		vi.minvalue := settyp^.minvalue;{ low bound of set type }
		vi.maxvalue := settyp^.maxvalue;{ high bound of set type }

		putvitem(vi);			{ put item out }
						{ put out type of set elt }
		vi.form := numericdata;		{ form of elt is numeric }
		vi.minvalue := 0;		{ and is always 0 to 1 }
		vi.maxvalue := 1;		
		vi.loc.address := 0;		{ starts at 0 }
		vi.loc.relocation := offsetaddr;{ class is offset }
		vi.loc.blockn := 0;		{ blockn is irrelevant }
		vi.itemname := setelement;	{ insert dummy name }
		vi.scale := 0;			{ scale is zero, of course }
		vi.size := 1;			{ size is 1 bit}
		vi.itemdepth := vdepth+1;	{ depth is below set main } 
		putvitem(vi);			{ put out set element item }
		end;
	    arrayt: begin			{ array type }
		vi.form := arraydata;		{ array data }
		assert(inxtyp <> nil);		{ index type must exist }
		assert(inxtyp^.form in          { must be bounded }
			[scalar,booleant,chart,integert,longintt]);
		assert(aeltyp <> nil);		{ element type must exist }  
		vi.minvalue := inxtyp^.minvalue; { min of index }
		vi.maxvalue := inxtyp^.maxvalue; { max of index }
		vi.scale := 0;			{ indices are integers }
		putvitem(vi);			{ put out array top level }
						{ put out array subitem }
		fieldaddr.relocation := offsetaddr; { class is offset }
		fieldaddr.address := 0;		{ offset is zero }
		fieldaddr.blockn := 0;		{ blockn is irrelevant }
		vitembuild(vdepth+1,nil,fieldaddr,aeltyp,vpack,bynothing,
			vfile,vline);
		end;
	    recordt: begin			{ record type }
		vi.recordnum := tserial;		{ serial number of type }
		if typeid <> nil then begin	{ if type is named }
		    movename(typeid^.name,vi.recordname); { record name of type}
		end else begin			{ if anonomous type }
		    vi.recordname := anontype;	{ use anonomous name }
		    end;
		vi.form := recorddata;		{ record data }
		putvitem(vi);			{ put item out }
		wfield := fstfld;		{ get first field of rec }
		while wfield <> nil do begin	{ for all fields }
		    with wfield^ do begin       { using field item }
		        assert(class = field);	{ must be a field }
		        case ispacked of	{ packed or unpacked? }
			true: fieldaddr.address := bdisp; { packed is in bits}
   			false: fieldaddr.address := 8*fdisp; {unpacked in bytes}
			end;
			fieldaddr.blockn := 0;	{ blockn is irrelevant }
			fieldaddr.relocation := offsetaddr; { class is offset }
		        vitembuild(vdepth+1,name,fieldaddr,itype,
				ispacked,bynothing,vfile,vline);
		        end;			{ end of this field }
		    wfield := wfield^.next;	{ link to next field }
	    	    end;			{ loop for all fields }
		end;
	    devicet: begin			{ device type }
		vi.recordnum := tserial;		{ serial number of type }
		if typeid <> nil then begin	{ if type is named }
		    movename(typeid^.name,vi.recordname); { record name of type}
		    end;
		vi.form := recorddata;		{ treated as record }
		vi.loc.relocation := deviceaddr;{ but part of device space }
		{ ***FIND OUT WHY DUPLICATE ADDRESS FIELD EXISTS IN STRUCT *** }
		putvitem(vi);			{ put item out }
		wfield := fstfld;		{ get first field of rec }
		while wfield <> nil do begin	{ for all fields }
		    with wfield^ do begin       { using field item }
		        assert(class = field);	{ must be a field }
		        case ispacked of	{ packed or unpacked? }
			true: fieldaddr.address := bdisp; { packed is in bits}
   			false: fieldaddr.address := 8*fdisp; {unpacked in bytes}
			end;
			fieldaddr.blockn := 0;	{ blockn is irrelevant }
			fieldaddr.relocation := offsetaddr; { class is offset }
		        vitembuild(vdepth+1,name,fieldaddr,itype,
				ispacked,bynothing,vfile,vline);
		        end;			{ end of this field }
		    wfield := wfield^.next;	{ link to next field }
	    	    end;			{ loop for all fields }

	        end;
	    signalt: begin			{ signal variable }
		vi.form := signaldata;		{ treated as signal }
		vi.minvalue := 0;		{ not applicable }
		vi.maxvalue := 0;		{ not applicable }
		vi.scale := 0;			{ not applicable }
		putvitem(vi);			{ output signal item }
		end;
	    tagfield: begin 			{ tag field }
		error(1051 {Variant records unimplemented in verifier});
		end;
	    variant: begin			{ variant record }
		error(1051 {Variant records unimplemented in verifier});
		end;
	    xcptnt: begin			{ exception variable }
		error(1052 {Exceptions unimplemented in verifier});
		end;
	    end;				{ end cases }
        end;					{ end WITH }
end { vitembuild }; 
{
	varfilegen  --  generate variables file for verifier

	This is called at the end of every block.    
}
procedure varfilegen(dlev: disprange);		{ display level to scan }
{
	idchain -- chain down identifier tree
}
procedure idchain(
		      varid: itp);		{ position on chain }
var varlocation: addressitem;			{ working address item}
begin
    if varid <> nil then begin			{ if something to do }
	with varid^ do begin			{ using variable item }
 	    idchain(llink);			{ left chain recursive }
	    if class = vars then begin		{ if this is a variable }
		if vkind = local then begin	{ and not a parameter }
	            assert(name <> nil);	{ name must exist }
                    assert(itype <> nil);	{ type must exist }
						{ build address item }
	            varlocation.address := vaddr*8;	{ address of var }
		    assert(vlev <= top);	{ in current display }
		     				{ get owning block }
		    varlocation.blockn := blockstack[vlev].blockpin;
		    if varlocation.blockn = 0 then begin { if global variable }
		        varlocation.relocation := absoluteaddr; {address is abs}
		    end else begin		{ if local variable }
		        varlocation.relocation := stackaddr;{ address on stack }
		        end;
						{ write variable records }
	            vitembuild(1,name,varlocation,itype,
			false,bynothing,fileser, lineser);
						{ handle interrupt locations }
		    if itype^.form = signalt then 	{ if signal }
		      if itype^.addresspresent then begin { if interrupt loc }
			varlocation.blockn := 0;{ interrupt loc block irrelev }
			varlocation.relocation := deviceaddr; { in dev space }
			varlocation.address := itype^.trapvec*8; { interrupt }
			vitembuild(1,name,varlocation,itype,
			    false,bynothing,fileser, lineser);
			end;			{ end interrupt loc }
		    end;			{ end vkind=local }
		end;				{ end class=vars }
	    if class = konst then		{ if constant }
		if kvalue.kind = data then begin	{ if VALUE clause constant }
		    varlocation.blockn := 0;	{ block is irrelevant }
		    varlocation.address := 8*kvalue.daddr; { address }
		    varlocation.relocation := valueaddr; { in value space }
		    vitembuild(1,name,varlocation,itype, 
			false, bynothing, fileser, lineser);
		    end;			{ end VALUE constant }
	    idchain(rlink);			{ right chain recursive }
	    end;				{ end with }     
	end;					{ end non-nil }
end {idchain};
begin { varfilegen} 
    if display[dlev].fname <> nil then begin	{ if any to display }
	idchain(display[dlev].fname);	{ display variables at level }
	end;
end {varfilegen};  
{
	paramfilegen --  block info generator                              
}
procedure paramfilegen(routine: itp;	{ routine to generate arg list for }
		       blkkind: unittype); { proc, fn, etc. }
var
    wparam: itp;			{ working pointer in arg list }
    paramlocation: addressitem;		{ address of param }
    rwork: varitem;			{ for routine definition }
begin
    with routine^ do begin		{ using procedure ident }
					{ common items for monitor/proc/fn }
	wparam := nil;			{ assume no arg list }
	rwork.itemdepth := 1;		{ always at outermost level }
	rwork.loc.address := 0;    	{ not meaningful for routines }
	rwork.size := 1;		{ somewhat arbitrary }
	rwork.loc.relocation := routineaddr; { address in routine # space }
	movename(name, rwork.itemname);	{ name of routine }
	rwork.minvalue := 0;		{ not applicable }
	rwork.maxvalue := 0;		{ not applicable }
	rwork.scale := 0;		{ not applicable }
	rwork.by := bynothing;		{ not applicable }
	rwork.vrsource.filenumber := fileser; { file number of proc decl }
	rwork.vrsource.linenumber := lineser; { line number of proc decl }
	case blkkind of			{ different kinds of block }   
	proctyp: begin			{ procedure or function }
	    assert(class = proc);
	    assert(pkind = decl);	{ check variant }
	    rwork.loc.blockn := paddr;	{ routine serial number }
	    rwork.form := proceduredata;{ indicate procedure or function }
	    if itype <> nil then	{ if function }
		rwork.form := functiondata; { note as function }
	    wparam := next;		{ head of arg chain }	
	    end;
	montyp: begin		{ monitor }
	    assert(class = modul);	{ monitor or module variant }
	    assert(mkind = decl);	{ check variant }
	    rwork.loc.blockn := maddr;	{ routine serial number }
	    rwork.form := monitordata;	{ must be a monitor }
	    end;
	modtyp: begin		{ module }  
	    assert(class = modul);	{ monitor or module variant }
	    assert(mkind = decl);	{ check variant }
	    rwork.loc.blockn := maddr;	{ routine serial number }
	    rwork.form := moduledata;	{ is a module }
	    end;	
	main: begin		{ main program }
	    rwork.loc.blockn := 0;	{ always block zero }
	    rwork.form := programdata;	{ is a program }
	    end;			{ nothing is generated }
	  end;				{ end cases }
	end;				{ of with }
    putvitem(rwork);			{ generate item }
    while wparam <> nil do begin	{ for all parameters }
	with wparam^ do begin		{ using this param }
	    assert(class = vars);	{ must be variable }
	    assert((vkind = param) or (vkind = formal)); { must be param }
					{ generate argument entry }
	    paramlocation.address := vaddr*8; { get address of param }
	    paramlocation.relocation := paramaddr; { this is a param }
					{ get owning procedure number }
	    paramlocation.blockn := blockstack[vlev].blockpin;
	    case vkind of		{ handle different kinds of args }
	    param: begin		{ value argument }
	                                { build item }
	        vitembuild(2,name,paramlocation,itype,
		    false,byactualvalue,fileser, lineser); 
		end;
	    formal: begin		{ VAR argument }
					{ pointer entry }
		rwork.itemdepth := 2;	{ depth 2 }
		rwork.loc := paramlocation; { location of pointer }
		rwork.size := 16;	{ size of pointer }
		rwork.form := pointerdata; { pointer type }
		rwork.by := byreference;{ by reference }
		rwork.vrsource.filenumber := fileser; { file number }
		rwork.vrsource.linenumber := lineser; { source line }
		movename(name,rwork.itemname); { name of arg }
		putvitem(rwork);	{ generate pointer item }
					{ generate actual ref arg }
		paramlocation.address := 0;	{ at zero offset }
		paramlocation.relocation := pointeraddr; { pointer ref }
		paramlocation.blockn := 0;	{ block number irrelevant }
	        vitembuild(3,name,paramlocation,itype,
			false,bynothing,fileser, lineser); 
		end;
	    end;
	    end;
	wparam := wparam^.next;		{ advance to next arg }
	end;				{ end param loop }
    end { paramfilegen };
{
	vdataconst  --  put data constant in varfile

	Data constants are unnamed data objects mentioned in valueexpressions.
	String constants and structured data constants are data constants unless
	used in VALUE or CONST declarations.
}
procedure vdataconst(vaddr: integer;		{ address in data space }
		       vtype: stp;		{ type of subscript }
		       vfile: integer;		{ which source file }
		       vline: integer);		{ source line number }
const
    bitsperchar = 8;				{ bits per character }
var                                                     
    waddr: addressitem;				{ working address of field }
begin
						{ build array element item }
    waddr.address := vaddr * bitsperchar;	{ address in value space }
    waddr.relocation := valueaddr;		{ is in value space }
    waddr.blockn := 0;				{ block number is 0 }
    vitembuild(1,nil,waddr,vtype,false,bynothing,vfile, vline); { build item }
end {vdataconst};
{
	genlineid -- generate line id for statement (not declaration)

	The line number is an index into the source line file, and
	is used to allow proper error message generation in later
	phases of processing.
}
procedure genlineid;
begin
    genbyte(255 {LINENR});		{ 'linenr <lineid>' }
    genbyte(lastfilenumber);		{ file number of file }
    genword(lastlinenumber);		{ line number of file }
end {genlineid};
