procedure WHATspec; const WHAT = '@(#)p2spec.i    2.2'; begin SINK := WHAT; end;    { Version 2.2 of 12/19/82 }
{
    Specification processing

    This phase of processing occurs after transitive closure and before
    Jcode generation.  By this time, all code has been examined and
    all declaration assertions extracted and associated with the
    relevant block.  Further, all set/used list processing is complete.

    The objects of this phase are to

    1.  Detect the use of incorrect variables in declaration assertions,
        and

    2.  generate "defined" entry and exit conditions where the user
        has left such conditions unspecified.
}
{
    blockspec  --  process specifications for one block
}
procedure blockspec(b: blocknodep);        { block to handle }
const iolistmax = maxsubstack;            { max args inc globals }
type iolistindex = 0..iolistmax;        { index to iolist }
type iolist = record                { i/o list for checks }
    iotop: 0..iolistmax;            { top of list }
    iotab: array [1..iolistmax] of record    { table of vars }
        iov: varnodep;            { base variable }
        iodefn: boolean;            { seen in defined }
        end;
    end;
var inlist, outlist: iolist;            { in/out lists }
    blockline: lineinfo;            { block defn line number }
{
    addio  --  add variable to io list
}
procedure addio(var list: iolist;        { iolist to add to }
        v: varnodep);            { variable to be added }
begin
    with list do begin                { using specified list }
    if iotop >= iolistmax then begin    { if overflow }
        internalerror(177);            { block i/o list overflow }
        end;
    iotop := iotop + 1;            { increment entry position }
    with iotab[iotop] do begin        { using new entry }
        iov := v;                { put variable in table }
        iodefn := false;            { no defined() yet }
        end;
    end;
end {addio};
{
    searchio  --  search io list
}
function searchio(var list: iolist;        { list to search }
          key: varnodep)        { search target }
          : iolistindex;        { find position }
var i: longint;                    { for search }
begin
    searchio := 0;                { default return - no find }
    i := 1;                    { start search }
    while i <= list.iotop do begin        { for all on list }
    if list.iotab[i].iov = key then begin    { if find }
        searchio := i;            { return find position }
        i := iolistmax + 1;            { force exit }
    end else begin                { if no find }
        i := i + 1;                { continue search }
        end;
    end;                    { end While }
end {searchio};
{
    buildiolist  --  build input/output lists

    The result is the union of formal and global lists, which
    are disjoint.  Thus, no duplicate checking is required.
}
procedure buildiolist;
var q: varnodep;                { for formal chaining }
    r: refnodep;                { for ref chaining }
begin
    inlist.iotop := 0;                { clear lists }
    outlist.iotop := 0;                { clear lists }
    q := nil;                    { only routines have formals }
    if b^.blvarnode^.vardata.form in [proceduredata, functiondata] then
        q := firstformal(b^.blvarnode);        { get first formal arg }
    while q <> nil do begin            { for all formals }
    with q^ do begin            { using this formal }
        if vardata.by = byreference then begin { if VAR arg }
                        { if used, add to in list }
            if down^.varused then addio(inlist, down);
                        { if set, add to out list }
            if down^.varset then addio(outlist, down);
        end else begin            { if value arg }
        assert(vardata.by = byactualvalue); { must be by value }
        addio(inlist,q);        { add to input list }
        end;                { end value arg }
        end;                { end With }
    q := q^.right;                { on to next formal }
    end;                    { end While }
    if b^.blvarnode^.vardata.form = functiondata then begin { if function }
    addio(outlist,b^.blvarnode^.down);    { add returned value to outs }
    end;                    { end is function }
    r := b^.blrefs;                { get references list }
    while r <> nil do begin            { for all references }
    with r^ do begin            { using this reference }
                        { if used, add to in }
        if refkind = useref then addio(inlist, refvar); 
                        { if set, add to out }
        if refkind = setref then addio(outlist, refvar); 
        end;
    r := r^.refnext;            { on to next ref }
    end;
end {buildiolist};
{
    checkvaruse --  check usage of variable in assertion 

    Allowed cases for procedures and functions are

    Normal (non .old)
        ENTRY        must be in input list
        EXIT        must be in either list
        INVARIANT    must be in both lists
        EFFECT        not implemented

    With .old        
        EXIT        must be in input list

    Allowed cases for monitors and modules are

        INVARIANT    must be of current or inner block
        ENTRY        must be visible to all callers as global
        EXIT        must be visible to all callers as global
}
procedure checkvaruse(p: ptn;            { location for message }
              v: varnodep;        { base variable }
              subcode: longint;        { kind of assertion }
              isold: boolean);        { true if .old }
procedure checkprocvaruse;            { case for procedure/fn }
    var needin, needout: boolean;        { if in or out var }
    badold: boolean;                { true if .old misused }
begin
    needin := searchio(inlist,v) = 0;        { if input var }
    needout := searchio(outlist,v) = 0;        { if output var }
    badold := isold;                { .old bad if present }
    case subcode of                { fan out on subcode }
    entrysubcode: begin                { ENTRY }
    needout := false;            { don't need output }
    end;
    exitsubcode: begin                { EXIT }
    if isold then begin            { if .old }
        needout := false;            { do not need output }
        if not needin then badold := false;    { legal if in input list }
    end else begin                { if not .old }
        if needout and (not needin)    then begin { if input-only variable }
        usererrorstart(p^.linen);    { begin message }
        write(output,'Input variable "');
        writestring15(output,v^.vardata.itemname); { variable name }
        write(output,'" needs a ".old" suffix');
        usererrorend;            { finish message }
        needout := false;        { suppress other msgs }
        end;
        if (not needin) or (not needout) then begin { if either in or out }
        needin := false;        { allow }
        needout := false;        { allow }
        end;
        end;                { end not .old }
    end;
    effectsubcode: begin            { EFFECT }
    badold := false;            { .old is OK }
    needin := false;            { don't need input }
    unimplemented(p^.linen);        { ***UNIMPLEMENTED*** }
    end;
    entryexitsubcode: begin            { routine INVARIANT }
                        { need in and out }
    end;
    invariantsubcode: begin            { module/monitor INVARIANT }
                        { need in and out }
    end;
    end;                    { end cases }
    if badold then begin            { if bad .old }
    usererrorstart(p^.linen);        { begin message }
    write(output,'Variable "');
    writestring15(output,v^.vardata.itemname); { variable name }
    write(output,'" should not appear with .old here');
    usererrorend;                { finish message }
    end;
    if needin or needout then begin         { if trouble }
    usererrorstart(p^.linen);        { error at assertion }
    write(output,'Variable "');
    writestring15(output,v^.vardata.itemname); { variable name }
    write(output,'" is not an ');
    if needin or needout then begin        { if need specific in/out }
        if needin then write(output,'input');    { "input and output" }
        if needin and needout then write(output,' and ');
        if needout then write(output,'output');
    end else begin                { must be needeither }
        write(output,'input or output');
        end;
    write(output,' variable to "');
    writestring15(output,b^.blvarnode^.vardata.itemname); { routine name }
    write(output,'"');
    usererrorend;            { finish message }
    end;
end {checkprocvaruse};
{
    checkvarvisiblecaller  --  check variable visible to all callers of proc
}
procedure checkvarvisiblecaller;
begin
    if not isformal(v,b) then            { if not formal arg }
    if not visible(v,b^.bldominator) then begin { if variable is not visible }
    usererrorstart(p^.linen);        { begin diagnostic }
    write(output,'Variable "');
    writestring15(output,v^.vardata.itemname);    { name of variable }
    write(output,'" is not visible to all callers of "');
    writestring15(output,b^.blvarnode^.vardata.itemname); { name of block }
    write(output,'"');
    usererrorend;
    end;                    { end not visible }
end {checkvarvisiblecaller};
{
    checkmodulevaruse  --  check variable use for assertions of
                   monitors and modules
}
procedure checkmodulevaruse;
var vardepth: longint;                { nesting depth of var use }
begin
    if not (subcode in                 { limit valid types }
    [invariantsubcode, initentrysubcode, initexitsubcode]) then
    badvarnode(v,269);            { non invariant for module }
    with v^ do begin                { using relevant varnode }
    case subcode of                { fan out on subcode }
    invariantsubcode: begin            { INVARIANT }
        if varmaster <> b then begin        { if error possible }
            if varmaster = nil then begin    { if unassigned }
            usererrorstart(p^.linen);    { begin diagnosis }
            write(output,'Variable ');    
            diagvarname(v);        { variable name in quotes }
            write(output,' is never assigned a value.');
            usererrorend;        { finish message }
            end else begin            { if var is set somewhere }
                        { if set outside this block }
            if b <> dominator(varmaster, b) then begin
                usererrorstart(p^.linen);    { begin diagnosis }
                write(output,'Variable ');    
                diagvarname(v);        { variable name in quotes }
                write(output,
            ' cannot be used in this INVARIANT because there');
                usererrorend;        { end first line of message }
                write(output,'*** is an assignment to ');
                diagvarname(v);        { variable name in quotes }
                write(output,' in ');
                diagblockname(varmaster);    { block name in quotes }
                writeln(output,'. ***');
                end;            { end set elsewhere }
            end;                { end set somewhere }
            end;                { end error possible }
        end;                { end INVARIANT case }
    initexitsubcode,            { EXIT for INIT part }
    initentrysubcode: begin            { ENTRY for INIT part }
        checkvarvisiblecaller;        { callers must be able to ref }
        end;
    end;                    { end cases }
    end;                    { end With }
end {checkmodulevaruse};
begin {checkvaruse}
    if b^.blvarnode^.vardata.form in [proceduredata, functiondata] then begin
    checkvarvisiblecaller;            { check visibility }
    checkprocvaruse;            { use procedure check }
    end else begin                { if static block }
    checkmodulevaruse;
        end;
end {checkvaruse};
{
    notedefined  --  note use of defined psuedo-function

    When the user doesn't use "defined", we will later generate one.
    Here we note when the user did use "defined".
}
procedure notedefined(v: varnodep);        { base variable }
var key: iolistindex;                { search key }
begin
    key := searchio(inlist,v);        { look in inlist }
    if key > 0 then inlist.iotab[key].iodefn := true;
    key := searchio(outlist,v);        { look in outlist }
    if key > 0 then outlist.iotab[key].iodefn := true;
    if debugg then begin            { debugging output }
    writeln(dbg,'    Found within "defined": ',v^.vardata.itemname);
    end;
end {notedefined};
{
    adddefined  --  add single "defined" assertion to assertions

    Creates icode of form

        seq
          <other assertions>
          vdecl
            dfnd
              varbl
}
procedure adddefined(v: varnodep;        { variable to add }
             subcode: longint);        { subcode to use }
var p1, p2, p3: ptn;                { working varnodes }
begin
     if debugg then begin            { if debugging }
    writeln(dbg,'    Generating "defined" for ',v^.vardata.itemname,
        '   (subcode ',subcode:1,')');
    end;
     assert(not alwaysdefined(v));        { not on good guys }
     newnode(p3, varblop);            { generate varbl node }
     with v^ do begin                { using varnode }
    with p3^ do begin            { using "varbl" }
        mtype := data;            { arbitrary data }
        size := vardata.size;        { size from variable }
        disp := vardata.loc.address;    { address from variable }
        segnr := vardata.loc.blockn;    { absolute address }
        vtype := v;                { variable }
        linen := blockline;            { line number }
        end;                { end inner With }
    end;                    { end outer With }
    newnode(p2, defndop);            { create "defined" operator }
    with p2^ do begin                { using defined operator }
    mtype := b1;                { returns boolean }
    size := 1;                { size is 1 bit }
    nrarg := 1;                { one arg }
    arg[1] := p3;                { links to previous }
    linen := blockline;            { line number }
    end;
    newnode(p1, vdeclop);            { create "vdecl" operator }
    with p1^ do begin                { using "vdecl" }
    mtype := xxx;                { statement type }
    nrarg := 1;                { one arg }
    disp := subcode;            { subcode is given }
    arg[1] := p2;                { links to previous }
    linen := blockline;            { line number }
    end;
    addstmt(b^.blassertions,p1);        { add to "seq" type list }
end {adddefined};
{
    adddefineds  --  add "defined" calls to icode assertions
             when a variable is not mentioned by the
             user in a "defined" but one is needed.
}
procedure adddefineds;
var i: iolistindex;
begin
    for i := 1 to inlist.iotop do             { for in list }
    with inlist.iotab[i] do                { using this ref }
        if not iodefn then                { if no user defined }
        if not alwaysdefined(iov) then        { if not safe kind }
            if visible(iov,b^.bldominator) or    { if visible to callers}
               isformal(iov,b) then         { or formal arg }
                adddefined(iov,entrysubcode);    { add to entry list }
    for i := 1 to outlist.iotop do            { for out list }
    with outlist.iotab[i] do            { using this ref }
        if not iodefn then                { if no user defined }
        if not alwaysdefined(iov) then        { if not safe kind }
            if visible(iov,b^.bldominator) or    { if visible to callers}
               isformal(iov,b) then         { or formal arg }
                adddefined(iov,exitsubcode);    { add to exit list }
end {adddefineds};
{
    checkassertion  --  check single assertion against i/o lists
}
procedure checkassertion(p: ptn;        { single assertion }
             subcode: longint);    { kind of vardecl }
var i: 1..maxarg;                { for arg loop }
    base: varnodep;                { base variable of item }
begin
    with p^ do begin                { using given node }
    if code in [defarop, defndop] then begin{ if defined operator }
        base := basevariable(arg[1]^.vtype);{ get thing defined }
        notedefined(base);            { note use of defined }
        end;
                        { now check use of variable }
    if code in [varblop, paramop] then begin{ if variable }
        checkvaruse(p,basevariable(vtype),subcode,false); { check use }
    end else if code = oldop then begin    { if .old }
        checkvaruse(p,basevariable(vtype),subcode,true); { check .old use }
    end else                { default case }
    for i := 1 to nrarg do begin        { examine subargs }
        checkassertion(arg[i],subcode);    { recurse }
        end;
    end;                    { end With }
end {checkassertion};
{
    checkdepth  --  check DEPTH statement value
}
procedure checkdepth(p: ptn);            { depth expression }
begin
    with p^ do begin                { using given node }
    if mttab[mtype].mtkind <> numericdata then { if not numeric }
        badnode(p,251);            { DEPTH arg not number }
    checkassertion(p,entrysubcode);        { check var use as if ENTRY }
    end;
end {checkdepth};
{
    checkassertions  --  check assertions against in/out lists
}
procedure checkassertions(var p: ptn);        { starting node }
var i: 1..maxarg;                { for arg loop }
begin
    if p <> nil then with p^ do begin        { using given node }
    if code = seqop then begin        { if sequence }
        for i := 1 to nrarg do begin     { for all assertions in seq }
        checkassertions(arg[i]);    { check all components }
        end;
    end else if code = vdeclop then begin    { if verification decl }
        checkassertion(arg[1],disp);
    end else if code = depthop then begin    { if DEPTH declaration }
        with b^ do begin            { using block node }
        if bldepthexpr <> nil then     { if DEPTH expr already present}
            badnode(p,252);        { duplicate DEPTH in block }
        bldepthexpr := p;        { save DEPTH expr }
        p := nil;            { remove DEPTH expr from decls }
        checkdepth(bldepthexpr^.arg[1]);{ check DEPTH expr }
        end;                { of With }
    end else                 { if neither }
        badnode(p, 176);            { non var decl in assertions }
    end;                    { end With }
end {checkassertions};
begin { blockspec }
    with b^ do begin                { using block node }
        if debugg then begin            { debug printout }
        writeln(dbg,'Specification processing for ',
        blvarnode^.vardata.itemname);
        end;
        blockline := blvarnode^.vardata.vrsource; { line number for diags }
        if (blouterblock=nil) <> (blvarnode^.vardata.form=programdata) then
        internalerror(301);            { block type nest error }
                        { static checks }
        case blvarnode^.vardata.form of        { fan out on block type }
        moduledata: begin            { module }
        if blouterblock^.blvarnode^.vardata.form in 
        [proceduredata, functiondata] then
            diag(blockline,'This module is inside a procedure or function');
        if (blassertions <> nil) and (not blhasbody) then
        diag(blockline,'This module has assertions but no body');
        end;
        monitordata: begin            { monitor }
        if blouterblock^.blvarnode^.vardata.form <> programdata then
            diag(blockline,'This monitor is inside another block');
        if (blassertions <> nil) and (not blhasbody) then
        diag(blockline,'This monitor has assertions but no body');
        end;
        programdata: begin            { main program }
        if blassertions <> nil then     { assertions not allowed }
        internalerror(301);         { assertions in main }
        end;
        proceduredata, functiondata: begin    { procedure/function }
        end;
        end;                { of cases }
                        { real spec processing }
    if blvarnode^.vardata.form <> programdata then begin { if not main }
            buildiolist;            { build in/out vars list }
            checkassertions(blassertions);    { check for invalid uses }
            adddefineds;            { add defined list }
        end;                { end not main program }
        if debugg then begin            { debug printing }
        writeln(dbg);            { blank line }
        writeln(dbg, '    Augmented specification code:');
        treeprint(blassertions,0);        { dump assertions }
        writeln(dbg);            { blank line }
        writeln(dbg, '    Depth expression');
        if bldepthexpr <> nil then 
            treeprint(bldepthexpr,0)    { depth expression if any }
        else writeln(dbg,'    [NONE]');
        writeln(dbg);
        end;                { debugging }
    end;                        { With }
end {blockspec};
{
    specifications  --  perform specification processing on all blocks
}
procedure specifications;
var b: blocknodep;                { for chaining }
begin
    b := blockhead;                { get first block }
    while b <> nil do begin            { for all blocks }
    blockspec(b);                { process block }
    b := b^.blnext;                { on to next block }
    end;
end {specifications};
