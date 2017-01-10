procedure WHATicall; const WHAT = '@(#)p2icall.i    2.1'; begin SINK := WHAT; end;    { Version 2.1 of 10/12/82 }
{
    icall operator - used for INIT statement

    REQUIRE module variable not defined before each INIT
    REQUIRE invariants of blocks crossed outward
    REQUIRE blocks crossed inward to be initialized
    NEW module variable as defined after INIT
    NEW all changed variables after INIT of module
}
procedure opicall(p: ptn);            { icall node }
var callee: blocknodep;                { monitor/module being called }
    caller: blocknodep;                { calling block }
    r: refnodep;                { for ref chaining }
{
    explaininitentry  --  generate explaination for ENTRY condition
                  after an INIT

    (entry assertion of xxx.pf:25)
}
procedure explaininitentry(pa: ptn);        { assertion to explain }
begin
    gensubcode(pa^.disp);            { entry or invariant }
    genstring15(' of "');
    genstring15(p^.vtype^.vardata.itemname);     { name of callee }
    genchar('"');
    genchar(' ');
    genlineid(pa^.linen);            { location }
end {explaininitentry};
begin
    caller := lastblockp;            { caller is always current }
    with p^ do begin                { using given node }
    assert(code = icallop);            { must be init operation }
    callee := vtype^.blockdata;        { get callee block }
    end;                    { With }
    if not callee^.blhasbody then begin        { if no body of callee }
    diag(p^.linen,                { INIT disallowed }
       'INIT useless - no code in monitor/module initialization part');
    end;
    requireblkinit(p^.linen,callee,false);    { REQUIRE block not init }
    {
        Generate REQUIRE statements for module invariants outward
        to the callee
    }
    requireinvariants(p, callee);        { gen for caller->callee }
    {
    Generate REQUIRE statements for initialization of blocks
    being entered.
    }
    requireinit(p, callee);            { gen for dominator->callee }
    genspecrequires(p,callee^.blassertions,[initentrysubcode],@explaininitentry);
    {    END INPUT PROCESSING  --  BEGIN OUTPUT PROCESSING    }
    genstring15('NEW');                { begin NEW }
    genspace;
    genchar('(');                { open var list }
    genname(callee^.blvarnode);            { name of module }
    r := callee^.blrefs;            { get ref list }
    while r <> nil do begin            { for ref list }
    with r^ do begin            { using this ref }
        if r^.refkind in [setref, initref] then begin { if changed }
        if visible(r^.refvar,caller) then begin { and visible }
            genspace;            { space between NEW args }
            genname(refvar);        { add to NEW list }
                        { force NEW! at use }
                setsubstitute(refvar, false, 0, genwithnew);
            end;            { end visible }
        end;                { end changed }
        r := r^.refnext;            { chain onward }
        end;                { end With }
    end;                    { end ref chaining }
    genchar(')');                { finish NEW list }
    genspace;                    { space before assertion }
    genstring15('(and!');            { begin conjunction }
    genspace;
                        { INIT part EXIT assertions }
    genconjunction(callee^.blassertions,[initexitsubcode]);
    genspace;
                        { (defined! new! <blk>) }
    gendataid(callee^.blvarnode,0,genwithnew,genwithdef);    
    genchar(')');                { finish AND }
    clearallsubstitutes;            { clear NEW! flag on vars }
    if comments then begin            { if commentary }
    gencomment(p^.linen);            { location of init }
    genstring15('INIT of');
    genchar(' ');
    genstring15(callee^.blvarnode^.vardata.itemname);
    end;
    genline;                    { finish NEW }
end {opicall};
