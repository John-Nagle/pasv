procedure WHATwait; const WHAT = '@(#)p2wait.i    2.3'; begin SINK := WHAT; end;    { Version 2.3 of 6/9/83 }
{
    Multiprogramming  --  wait statement processing
}
{
    issharedvar  --  is variable shared between processes?

    On-stack, value, and parameter variables cannot be shared.
    Other variables are considered shared if their owning
    process is not the current one, or if the variable is
    marked as shared, or the current block is marked as shared.
}
function issharedvar(v: varnodep): boolean;
begin
    assert(isbasevar(v));            { must be base var }
    with v^ do begin                { using given variable }
                        { if not shareable type }
    if vardata.loc.relocation in [stackaddr, valueaddr, paramaddr]
        then begin                 { then not shared }
        issharedvar := false;        { cannot be shared }
    end else begin                { if sharable type }
        issharedvar := (varshared <> lastblockp^.blshared) or
               (varshared = isshared); { chk owning proc }
        end;                { end sharable type }
    end;                    { With }
end {issharedvar};
{
    waitblock  --  generate INVARIANT requires at WAIT
}
procedure waitblock(wloc: lineinfo;        { WAIT location }
             blk: blocknodep);        { block being left }
{
    waitrequire  --  check if assertion relevant and gen if so

    An assertion is relevant if any variable in it is changed by
    the block.
}
procedure waitrequire(p: ptn);            { relevant node }
{
    explainwait  --  explain INVARIANT requires
}
procedure explainwait;
begin
    with p^ do begin                { using given node }
    assert(disp = invariantsubcode);    { must be invariant }
    genstring15('invariant');
    genlineid(p^.linen);            { loc of invariant }
    genstring15(', at WAIT');
    end;                    { end WITH }
end {explainwait};
begin {waitrequire}
    assert(p <> nil);                { must exist }
    with p^ do begin                { using given node }
    assert(code = vdeclop);            { must be vdecl }
                        { if EXIT assertion }
    if disp = invariantsubcode then begin
        if relevancetest(p^.arg[1],blk,[setref]) then begin { if changes }
        genrequire(wloc,p^.arg[1],explainwait);{ generate assertion }
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
end {waitrequire};
begin {waitblock}
    seqdrive(blk^.blassertions,waitrequire);    { process specifications }
    if blk^.blvarnode^.vardata.form in [programdata, moduledata] then 
    if blk^.blhasbody then            { if module/prg has body }
        innerblkdrive(blk,blkdefined);    { require inners def }
end {waitblock};
{
    genwaitnew  --  generate NEW for WAIT
}
procedure genwaitnew(p: ptn;            { WAIT node }
             blk: blocknodep);        { relevant block }
var nonewyet: boolean;                { false if NEW generated }
{
    waitvars  --  examine variables to see if relevant to WAIT

    A variable is to be mentioned in the NEW if it is declared in
    this junit and a shared variable.
}
procedure waitvars(v: varnodep);        { variable to test }
begin
    v := basevariable(v);            { use base variable }
    with v^ do begin                { using this var }
    if idunique <> 0 then begin        { if declared in this junit }
        if issharedvar(v) then begin    { if shared variable }
        gennewstart(nonewyet);        { gen NEW if needed }
        genname(v);            { gen name of var }
            setsubstitute(v, false, 0, genwithnew); { force new! }
        end;
        end;                { end declared }
    end;                    { end With }
end {waitvars};
begin
    nonewyet := true;                { no NEW yet }
    vardrive(waitvars);                { check for relevant vars }
    if not nonewyet then begin            { if a NEW was generated }
    genchar(')');                { finish NEW list }
    genspace;                { prepare for conjunction }
    genconjunction(blk^.blassertions,[invariantsubcode]);
    genline;                { finish assertions }
    end;
    clearallsubstitutes;            { clear NEW! settings }
end {genwaitnew};
{
    opwait  --  WAIT operator

    Before the wait, all relevant invariants must be proven.
    Relevant invariants are any invariants on the path outward
    to the main program which mention a variable changed in the
    block.
    After the wait, all relevant invariants may be assumed.
    The wait causes all visible shared variables to be considered changed.
}
procedure opwait(p: ptn);            { wait node }
begin
    assert(p^.code = waitop);            { must be WAIT }
    with p^.arg[1]^ do begin            { using subnode }
    if vtype^.vardata.form <> signaldata then { if not signal }
        verybadnode(p,351);         { WAIT of non-signal }
    if comments then begin            { commentary }
        gencomment(p^.linen);        { begin WAIT }
        genstring15('start of WAIT');
        genline;
        end;
    waitblock(linen,lastblockp);        { gen invariant requires }
    genwaitnew(p,lastblockp);        { gen NEW with new conj }
    if comments then begin            { commentary }
        gencomment(p^.linen);        { end WAIT }
        genstring15('end of WAIT');
        genline;
        end;
        end;                    { end With }
end {opwait};
{
    opsend  --  SEND statement

    The SEND statement has empty semantics in our system!
    All it does is to cause something else to be executed later.
}
procedure opsend(p: ptn);
begin
end;
