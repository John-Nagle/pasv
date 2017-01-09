procedure WHATjunit; const WHAT = '@(#)p2junit.i    2.4'; begin SINK := WHAT; end;    { Version 2.4 of 11/15/82 }
{
    valdefined  --  turn out (defined! new! x) conjunction
            for all value args to a block, and for all
            inner modules if this is an initialization block.
}
procedure valdefined(blk: blocknodep);        { routine entry }
var rparens: longint;                { number of args }
{
    blkundefined  --  assume immediately inner blocks undefined

    This is meaningful only for initialization blocks of modules.
}
procedure blkundefined;
{
    blkundef1  --  handle inner block not defined test
}
procedure blkundef1(b: blocknodep);            { inner block }
begin
    with b^ do begin                { using this ref }
    if blvarnode^.idunique = 0 then begin    { if no ref this junit }
        usererrorstart(blvarnode^.vardata.vrsource);{ diagnose }
        write(output,'No INIT of "');
        writestring15(output,blvarnode^.vardata.itemname);
        write(output,'" in "');
        writestring15(output,blk^.blvarnode^.vardata.itemname);
        usererrorend;
    end else begin                { if ref }
        genstring15('(and!');        { new clause }
        genspace;
        genstring15('(not!');        { (not! (defined! }
        genspace;
                        { (defined! new! }
        gendataid(blvarnode,nulltid, genwithnew, genwithdef);    
        genchar(')');            { finish not }
        rparens := rparens + 1;        { finish and }
        end;
    end;                    { end With }
end {blkundef1};
begin {blkundefined}
    innerblkdrive(blk, blkundef1);            { scan inners }
end {blkundefined};
{
    paramdefined  --  assume value parameters are defined when callee
              is entered.  We require that these be defined
              at call time.
}
procedure paramdefined;
var warg: varnodep;                { for tracing chain }
begin
    warg := firstformal(blk^.blvarnode);    { get first arg if any }
    while warg <> nil do begin            { for all args }
    if warg^.vardata.by = byactualvalue then begin { if by value }
        genspace;
        genstring15('(and!');        { new conjunct term }
        rparens := rparens + 1;        { note ) needed later }
        genspace;
        genalldefv(basevariable(warg), genwithnew);{ (defined! new! <var>) }
        end;
    warg := warg^.right;            { on to next arg }
    end;                    { end arg loop }
end {paramdefined};
begin {valdefined}
    rparens := 0;                { number of ) needed at end }
    case blk^.blvarnode^.vardata.form of    { fan out on kind of block }
    programdata, moduledata: begin        { program and module }
    blkundefined;                { gen inner blocks undefed }
    end;
    proceduredata, functiondata: begin        { procedure and function }
        paramdefined;                { gen value params defined }
    end;
    monitordata: begin                { monitor }
    end;                    { no defined stuff }
    end;                    { of cases }
    genspace;
    genstring15('(true!)');            { final conjunct }
    while rparens > 0 do begin            { balance parentheses }
    genchar(')');                { right }
    rparens := rparens - 1;            { count down to 0 }
    end;
end {valdefined};
{
    junitbegin  --  generate name part of junit header
}
procedure junitbegin;
begin
    genstring15('BEGIN');
    genspace;
    genblockid(lastblockp);        { name of junit }
    genline;                { end of header line }
end {junitbegin};
{
    junitend  --  generate final END of junit     
}
procedure junitend;
begin
    genline;                { flush line if required }
    genstring15('HANG');        { HANG statement }
    genline;
    genstring15('END');            { end of junit }
    gencomment(nulllineinfo);        { begin comment }
    genstring15(name);            { name of routine }
    genline;                { end of last line }
end {junitend};
{
    headerpart  --  generate ENTRY and INVARIANT proclaims at start
}
procedure headerpart(var tab: oldargtab;    { old args bindings }
             p: ptn);            { assertions }
                    { assumable assertion types }
var warg: varnodep;                { for threading arg list }
    basewarg: varnodep;                { base variable of warg }
    nonewyet: boolean;                { if NEW needed }
    tnum: tempid;                { sink for temp num }
{
    listvar  --  construct list of variables for initial NEW
}
procedure listvar(v: varnodep);            { node being tested }
begin
    v := basevariable(v);            { get base form }
    with v^ do begin                { using given node }
    if idunique <> 0 then begin        { if declared in junit }
        gennewstart(nonewyet);        { start NEW }
        genname(v);                { generate var name }
        setsubstitute(v, false, 0, genwithnew);     { force new! }
        end;
    end;                    { end With }
end {listvar};
begin {headerpart}
                        { initial BREAK }
    genstring15('BREAK');            { start with BREAK }
    genspace;
    genmsgstart(firstline);
    genstring15('Start'); genchar(' ');
    genstring15('of'); genchar(' ');
    genchar('"'); genstring15(name); genchar('"'); { name of routine }
    genmsgend;                    { finish message }
    genline;                    { end of j-statement }
    {
    Generate NEW for all variables in junit.  Assertion of NEW
    will be conjunction of various conditions, if any. 

    For routines, the entry conditions, routine invariants, and
    relevant module invariants may be assumed.
    For monitors, modules, and the main program, only the entry
    conditions of the block may be assumed.  Remember that when
    we initialize a monitor or module we know nothing about its
    internal variables.
    }
    assert(lastblockp^.blvarnode <> nil);    { must have routine entry }
    warg := lastblockp^.blvarnode^.down;    { link to first arg }
    nonewyet := true;                { no new yet }
    vardrive(listvar);                { get all vars in junit }
    if not nonewyet then begin            { if any new variables }
        genchar(')');                { end arg list }
        genspace;                { before assertions }
    genstring15('(and!');            { begin defined part }
    genspace;
    valdefined(lastblockp);            { (defined! arg) ... }
    genspace;            
    if lastblockp^.blvarnode^.vardata.form     { if is routine }
        in [proceduredata, functiondata] then
        genconjunction(p, 
        [entrysubcode, entryexitsubcode, invariantsubcode])    { for routine }
    else                    { if not routine }
        genconjunction(p,[initentrysubcode]);{ for monitor/module/main }
    genchar(')');                { close and }
        if comments then begin            { if notations }
            gencomment(nulllineinfo);        { no line number }
        genstring15('Input args');        { identify type of NEW }
        end;
        genline;                { end of NEW }
        clearallsubstitutes;            { clear NEW! settings }
    end else begin                { if no args }
        if comments then begin            { notation }
            gencomment(nulllineinfo);
            genstring15('No inputs');        { note in jcode }
        genline;
        end;                { end commentary }
        end;                    { end no args }
    {
    Save input values of formal args if required
    }
    warg := lastblockp^.blvarnode^.down;    { first arg }
    while warg <> nil do begin            { for all args }
    basewarg := basevariable(warg);        { get base of arg }
    with basewarg^ do begin            { using base }
        if varused then             { if arg is an input arg }
        if visible(basewarg,lastblockp) then begin { and visible here }
                        { gen temp dummy and bind }
            saveoldarg(tab, warg, warg^.vardata.by, nil, tnum);
            end;            { end temp dummy required }
        end;                { end with }
    warg := warg^.right;            { on to next arg }
    end;
    saveglobals(tab, lastblockp);        { save input values of globals }
    assignvalues(lastblockp);            { set up VALUE constants }
end {headerpart};
{
    trailerpart  --  generate EXIT and INVARIANT requires at end
}
procedure trailerpart(blk: blocknodep);        { relevant block }
{
    trailrequire  --  check if assertion relevant and gen if so

    An assertion is relevant if any variable in it is changed by
    the block.
}
procedure trailrequire(p: ptn);            { relevant node }
{
    explaintrail  --  explain EXIT and INVARIANT requires at end
}
procedure explaintrail;
begin
    with p^ do begin                { using given node }
                        { only valid types }
    assert(disp in 
    [exitsubcode, entryexitsubcode, initexitsubcode, invariantsubcode]); 
    if disp = invariantsubcode then begin    { special case }
        genstring15('invariant');
        genstring15(', at end of "');
        genstring15(blk^.blvarnode^.vardata.itemname); { name of block }
        genchar('"');
    end else begin                { general case }
        gensubcode(disp);            { kind of assertion }
        end;
    end;                    { end WITH }
end {explaintrail};
begin
    assert(p <> nil);                { must exist }
    with p^ do begin                { using given node }
    assert(code = vdeclop);            { must be vdecl }
                        { if EXIT assertion }
    if disp in [entryexitsubcode, exitsubcode,initexitsubcode] then begin    
                        { generate EXIT assertion }
        genrequire(p^.linen, p^.arg[1] , explaintrail);    
    end else if disp = invariantsubcode then begin
        if relevancetest(p^.arg[1],blk,[setref]) then begin { if changes }
                        { generate assertion }
            genrequire(p^.linen, p^.arg[1], explaintrail);    
        end else begin            { if not relevant }
        if comments then begin        { commentary }
            gencomment(p^.linen);    { begin comment }
            genstring15('Assuming');
            genchar(' ');
            genmexpr(p^.arg[1]);    { expression }
            genline;
            end;            { end commentary }
        end;                { end not relevant }
        end;                { end invariant or exit }
    end;                    { With }
end {trailrequire};
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
{
    genreturndefined  --  generate DEFINED require for function return
}
procedure genreturndefined(b: blocknodep);    { function }
begin
    with b^.blvarnode^ do begin            { using varnode }
        assert(vardata.form = functiondata);     { functions only }
        genstring15('REQUIRE');            { REQUIRE (defined! <expr>) }
        genspace;
                        { (defined! <var>) }
    genalldefv(down,genwithoutnew);        { require all defined }
        genspace;
        genmsgstart(vardata.vrsource);        { begin message }
        genchar('"');                { enclose in quotes }
    genstring15(down^.vardata.itemname);    { name of variable }
        genchar('"');                { enclose in quotes }
        genchar(' ');
        genstring15('is defined');        { message }
        genmsgend;                { end message }
        genline;                { end REQUIRE }
    end;
end {genreturndefined};
begin {trailerpart}
    if blk^.blvarnode^.vardata.form = functiondata then begin { if fn }
    genreturndefined(blk);            { REQUIRE return defined }
    end;
    seqdrive(blk^.blassertions,trailrequire);    { process specifications }
    if blk^.blvarnode^.vardata.form in [programdata, moduledata] then 
    if blk^.blhasbody then            { if module/prg has body }
        innerblkdrive(blk,blkdefined);    { require inners def }
end {trailerpart};
{
    junit -- generate jcode for one routine
}
procedure junit(blk: blocknodep;         { block being generated }
        body: ptn);            { icode for body }
var
    unitolds: oldargtab;            { old argument table }
begin
    labelserial := 0;                { reset label generator }
    tempserial := 0;                { reset TEMP$ generator }
    unitolds.oldargcount := 0;            { reset old arg table }
    blockdepthtid := 0;                { reset TEMP$ number for DEPTH }
    sideeffectinthisstmt := true;        { cleared at 1st stmt }
    clearallsubstitutes;            { clear substitutions }
    junitbegin;                    { header of junit }
    genjvars(blk);                { variable declarations }
    headerpart(unitolds, blk^.blassertions);    { entry, invariant, etc. }
    sideeffectinthisstmt := false;        { clear for entry depth check }
    entrydepthcheck(blk);            { check recursion depth }
    statement(body);                { generate body }
    bindoldargs(unitolds);            { set up .old vars for exits }
    trailerpart(blk);                { exit, invariant, etc. }
    junitend;                    { end of junit }
end {junit};
