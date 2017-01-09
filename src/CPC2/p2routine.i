procedure WHATroutine; const WHAT = '@(#)p2routine.i    2.5'; begin SINK := WHAT; end;    { Version 2.5 of 11/15/82 }
{
    Routine (procedure/function) handling

    Code here is used both when processing calls to routines and
    when handing the beginnings and ends of the routines themselves.
{
    statement  --  Jcode generation for statements
}
procedure statement(p: ptn); forward;
{
    genconjunction  --  generate conjunction of assertions
}
procedure genconjunction(p: ptn;        { node to start at }
             accept: subcodeset);    { subcodes for assertions }
var terms: longint;                { count of terms }
{
    genconjunct1    --  generate one term of conjunction
}
procedure genconjunct1(p:ptn);
var i: 0..maxarg;                { for arg loop }
begin
    with p^ do begin                { using given node }
    if code = vdeclop then begin        { if verification decl  }
        if disp in accept then begin    { if assertion of wanted type }
        genstring15('(and!');        { begin conjunct part }
        genspace;
        genjexpr(arg[1]);        { generate assertion }
        genspace;            { space before next part }
        terms := terms + 1;        { count terms generated }
        end;                { end wanted assertion }
    end else begin                { invalid node }
        badnode(p,106);            { not SEQ or VDECL }
        end;                { end assertion }
    end;                    { end non-null }
end {genconjunct1};
begin {genconjunction}
    terms := 0;                    { clear count of terms }
    seqdrive(p,genconjunct1);            { generate for each elt }
    genstring15('(true!)');            { last conjunct }
    while terms > 0 do begin            { balance parantheses }
    genchar(')');
    terms := terms - 1;            { decrement parens needed }
    end;
end {genconjunction};
{
    gentempasg  --  generate assignment to temporary

    Form generated is roughly

    ASSIGN (TEMP$nn: <type of formal>) (TEMPnn)
        (defined! <actual>) (<actual>)

    If isdef is true, the entire new temp is defined; otherwise,
    the defined status is taken from the actual.

    If actual is nil, then the old value is taken as that of the formal.
    This is used at procedure entry to save the original values of the
    arguments for use in .old references.
}
procedure gentempasg(tnum: tempid;            { which temporary }
             formal: varnodep;            { selector }
             actual: ptn;            { expression }
             isdef: boolean);            { if temp is defined }
begin
    assert(tnum <> 0);                { temp must be real }
    assert(formal = basevariable(formal));        { must be base }
    genstring15('ASSIGN');                { begin ASSIGN }
    genspace;
    genchar('(');                    { open ASSIGN list }
    gentempid(tnum);                { TEMP$nn }
    genchar(':');                    { begin declaration }
    genspace;
    genjtype(formal);                { type of dummy }
    genchar(')');                    { close ASSIGN list }
    genspace;
    genchar('(');                    { begin lhs part }
    gentempid(tnum);                { always entire TEMP }
    genchar(')');                    { end lhs part }
    genspace;                    { begin defined part }
    if isdef then begin                { if fully defined }
        gentrueobject(formal);            { form of formal }
    end else begin                    { if partial defn }
        if actual <> nil then begin            { if real actual }
                            { formal => defined }
            assert(actual^.code = referop);        { must be VAR param }
        gendefselector(actual);            { gen as defined }
        end else begin                { if saving formal }
                            { (defined! formal) }
        gendataid(formal,nulltid,genwithoutnew,genwithdef);
        end;
        end;                    { end defined part }
    genspace;                    { begin value part }
    if actual <> nil then begin            { if real actual }
        genjexpr(actual);                { actual value }
    end else begin                    { if saving formal }
                            { (formal) }
        gendataid(formal, nulltid, genwithoutnew, genwithoutdef);
        end;
    if comments then begin                { if comments wanted }
        gencomment(nulllineinfo);
        genstring15('TEMP');
        geninteger(tnum);
        genchar(' ');
        genstring15(':=');
        genchar(' ');
        if actual <> nil then begin            { if real actual }
        genmexpr(actual);            { generate actual }
        end else begin                { if saving non-expr }
            genstring15(formal^.vardata.itemname);    { name of formal }
        end;
        end;
    genline;                    { finish line }
end {gentempasg};
{
    searcholdtab  --  search oldarg table for variable
}
function searcholdtab(var tab: oldargtab;    { relevant table }
              arg: varnodep)        { search key }
              : longint;        { find index or 0 if no find }
var i: longint;                    { for search }
begin
    i := 1;
    searcholdtab := 0;                { assume fail }
    while i <= tab.oldargcount do begin        { for all in table }
    with tab.oldargs[i] do begin        { using this item }
        if oavar = arg then begin        { if find }
        searcholdtab := i;        { return find index }
        i := tab.oldargcount + 1;    { force loop exit }
        end else begin            { if no find }
        i := i + 1;            { continue search }
        end;                { end no find }
        end;                { end With }
    end;                    { end loop }
end {searcholdtab};
{
    saveoldarg  --  save old value of input variable for use as ".old"
              at exit.
}
procedure saveoldarg(var tab: oldargtab;    { relevant table }
              arg: varnodep;        { arg to save and bind }
              by: paramkind;        { how passed }
              actual: ptn;        { new value or nil }
              var tnum: tempid);    { returned new temp id }
var basearg: varnodep;                { base var of arg }
    i: longint;                    { for search }
begin
                        { generate assign of temp dummy}
    basearg := basevariable(arg);        { get base of arg }
    i := searcholdtab(tab,arg);            { check if already saved }
    if i > 0 then begin                { if find }
    tnum := tab.oldargs[i].oatnum;        { return temp number }
    end else begin                { no find, must save here }
        tnum := nexttemp;            { assign temp number }
        if tab.oldargcount >= oldargsmax then      { if old arg overflow }
            internalerror(130);        { too many set/used args }
        gentempasg(tnum, basearg, actual, by = byactualvalue);
                        { save binding for exit time }
        tab.oldargcount := tab.oldargcount + 1;    { bump count of old args }
        tab.oldargs[tab.oldargcount].oatnum := tnum; { save temp id }
        tab.oldargs[tab.oldargcount].oavar := basearg;    { save arg }
    end;
end {saveoldarg};
{
    saveglobals  --  save values of global variables for .old use
}
procedure saveglobals(var tab: oldargtab;    { table to save in }
            blk: blocknodep);    { block for save }
var wref: refnodep;                { working ref pointer }
    tnum: tempid;                { sink }
begin
    wref := blk^.blrefs;            { get ref chain }
    while wref <> nil do begin            { for all refs }
    with wref^ do begin            { using ref item }
        if refkind in [setref,useref] then begin { if referenced }
        if visible(refvar,blk) then begin { and visible here }    
                        { save the arg for later }
            saveoldarg(tab, refvar, bynothing, nil, tnum);
            end;
        end;
        end;                { end with }
    wref := wref^.refnext;            { on to next ref }
    end;
end {saveglobals};
{
    bindoldargs  --  bind args for use as ".old" in exit condition
}
procedure bindoldargs(var tab: oldargtab);    { using given tab }
var i: 1..oldargsmax;                { old arg counter }
begin
    with tab do                 { use given tab }
    for i := 1 to oldargcount do begin        { for all old args }
        with oldargs[i] do begin        { using old arg table }
                        { bind to temp id }
        setsubstitute(oavar, true, oatnum, genwithoutnew);  
        end;
    end;
end {bindoldargs};
{
    gensubcode  --  generate assertion subcode for message
}
procedure gensubcode(n: longint);        { subcode }
begin
    case n of                    { fan out on subcode }
    entrysubcode:     genstring15('entry assertion');
    exitsubcode:      genstring15('exit assertion');
    invariantsubcode:     genstring15('invariant');
    effectsubcode: begin genstring15('effect'); genstring15(' assertion'); end;
    statesubcode:     genstring15('state assertion');
    summarysubcode: begin genstring15('summary'); genstring15(' assertion');end;
    assertsubcode: begin genstring15('assert'); genstring15(' assertion'); end;
    entryexitsubcode:begin genstring15('routine');genstring15(' invariant');end;
    initentrysubcode: begin genstring15('INIT part'); genchar(' '); 
                genstring15('entry assertion'); end;
    initexitsubcode: begin genstring15('INIT part'); 
               genstring15(' exit assertion'); end;
    end;
end {gensubcode};
{
    genspecrequires  --  generate REQUIREs from specification declarations
}
procedure genspecrequires(diagloc: ptn;        { relevant node for diag }
              specs: ptn;        { assertion list }
              allow: subcodeset;    { kinds of assertions wanted }
                        { explaination generator to use}
              procedure explaingenerator(w: ptn));
var callee: blocknodep;                { relevant block }
{
    genspecrequire  --  generate one spec condition
}
procedure genspecrequire(p: ptn);        { assertion }
var i: 1..4;                    { for spacing }
begin
    with p^ do begin                { using assert node }
    assert(code = vdeclop);            { must be vdecl }
    genstring15('REQUIRE');            { REQUIRE <spec cond> }
    genspace;
    genjexpr(arg[1]);             { generate condition }
    genmsgstart(diagloc^.linen);        { call line number }
    genmexpr(arg[1]);            { entry condition }
    for i := 1 to 4 do genchar(' ');    { space before explaination }
    genchar('(');                { explaination in parens }
    explaingenerator(p);            { use provided explainer }
    genchar(')');                { close explaination }
    genmsgend;
    genline;
    end;                    { With }
end {genspecrequire};
{
    specscan  --  traverse assertion list looking for  
                 assertions of requested subcodes
}
procedure specscan(p: ptn);            { node in assertions }
var i: 0..maxarg;                { for arg loop }
begin
    if p <> nil then begin            { if any expressions }
    with p^ do begin            { using given node }
        if code = seqop then begin        { if SEQ }
        for i := 1 to nrarg do         { for each arg }
            specscan(arg[i]);         { recurse }
        end else begin            { if not SEQ }
        if code <> vdeclop then verybadnode(p,160); { non decl in decls}
        if disp in allow then begin    { if of wanted type }
            genspecrequire(p);        { gen the assertion }
            end;            { end right kind of assert }
        end;                { end VDECL }
        end;                { end With }
    end;                    { non-nil }
end {specscan};
begin {genspecrequires}
    specscan(specs);                { process assertions }
end {genspecrequires};
{
    requireblkinit  --  require that specific block be initialized
                Only monitors and modules are eligible.
}
procedure requireblkinit(callloc: lineinfo;    { for diag loc only }
             callee: blocknodep;    { called block }
             wantdef: boolean);    { want def, or undef }
begin
    with callee^ do begin                { using given block }
    genstring15('REQUIRE');            { REQUIRE (block defined) }
    genspace;
    if not wantdef then begin        { if negation wanted }
        genstring15('(not!');        { want not defined(blk) }
        genspace;
        end;
                        { (defined! blockname) }
    gendataid(blvarnode,nulltid,genwithoutnew,genwithdef);
    if not wantdef then genchar(')');    { balance parens }
    genmsgstart(callloc);            { require message }
    assert(blvarnode^.vardata.form in [moduledata, monitordata]); 
    case blvarnode^.vardata.form of        { types of blocks }
        moduledata: genstring15('module "');
        monitordata: genstring15('monitor "');
        end;
    genstring15(blvarnode^.vardata.itemname); { name of block }
    genstring15('" has');
    if not wantdef then genstring15(' not'); { negation }
    genstring15(' been');
    genstring15(' initialized');
    genmsgend;
    genline;
    end;                    { With }
end {requireblkinit};
{
    requireinit  --  require that modules being entered be defined

    For each block crossed inward from the dominator of the caller
    and callee to the callee, we must show that the module is
    defined.

    We are entitled to assume that any block we are already inside
    is already defined, or we wouldn't be there.

    We are entitled to assume that if a block is defined, all blocks
    within it are defined.  We therefore need check only for the
    outermost block toward the dominator.
}
procedure requireinit(callnode: ptn;        { call operation }
              caller: blocknodep);    { caller }
var callee: blocknodep;                { block being called }
    b: blocknodep;                { working block node }
    dom: blocknodep;                { dominator }
    outblk: blocknodep;                { outermost relevant }
begin
    outblk := nil;                { assume no relevant blk }
    with callnode^ do begin            { using given node }
    assert(code in [callop, icallop]);    { must be call or init }
    callee := vtype^.blockdata;        { block of callee }
    end;
    dom := dominator(caller, callee);        { get dominator block }
    b := callee;                { prepare for search }
    while b <> dom do begin            { for all blocks crossed }
    assert(b <> nil);            { never reach nil }
    b := b^.blouterblock;            { get next block outward }
    assert(b <> nil);            { never reach nil }
    if b <> dom then begin            { do not consider dominator }
        if b^.blvarnode^.vardata.form in 
        [monitordata, moduledata] then begin { if module/mon }
            if b^.blhasbody then begin    { with INIT part }
             outblk := b;        { outermost so far }
            end;            { end has init part }
        end;                { end module or monitor }
        end;                { end not dominator }
    end;                    { block cross loop }
    if outblk <> nil then with outblk^ do begin    { check for key block }
    if blvarnode^.idunique = 0 then begin { if not declared }
        usererrorstart(callnode^.linen);    { user error }
        write(output,'Need to show that "');
        writestring15(output,blvarnode^.vardata.itemname);
        write(output,'" has been initialized    (call into ');
        case blvarnode^.vardata.form of
        moduledata: write(output,'module');
        monitordata: write(output,'monitor');
        end;
        write(output,')');
        usererrorend;
    end else begin            { declared, need REQUIRE }
                    { this block must be inited }
            requireblkinit(callnode^.linen,outblk,true);    
        end;
    end;
end {requireinit};
{
    blkdefined  --  REQUIRE immediately inner block defined

    This is meaningful only for initialization blocks of modules.
    If an inner block is a module but the inner block does not
    contain an INIT part, then the test is repeated for the
    blocks of the next level inward.  See innerblkdrive.
}
procedure blkdefined(b: blocknodep);            { block to check }
begin
    with b^ do begin                    { using this ref }
    if blvarnode^.idunique <> 0 then begin{ if ref this junit }
        requireblkinit(blvarnode^.vardata.vrsource,b,true);
    end else begin                    { if not ref }
        assert(usererrors > 0);            { already caught }
        end;
    end;                        { end With }
end {blkdefined};
