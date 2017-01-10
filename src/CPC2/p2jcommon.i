procedure WHATjcommon; const WHAT = '@(#)p2jcommon.i    2.7'; begin SINK := WHAT; end;    { Version 2.7 of 1/14/83 }
{
    Common generation routines used in generating Jcode for several
    kinds of statements.
}
                        { forward reference }
procedure genmexpr1(p: ptn;            { node }
            callerpred: precedence);    { precedence of caller }
            forward;        
procedure safeselector(p: ptn); forward;    { forward ref }
procedure safeexpr(p: ptn);    forward;    { forward ref }
{
    genrequire  --  common generator for REQUIRE statements

    Messages in REQUIREs are always of the form

    <expression>    (<explanation>)
}
procedure genrequire(locn: lineinfo;        { relevant line number }
             p: ptn;            { assertion to require }
             explaingen: pnoargs);    { explaination generator }
begin
    with p^ do begin                { using given assertion }
    genstring15('REQUIRE');            { REQUIRE <expr> (/ <msg> /) }
    genspace;
    genjexpr(p);                { expression }
    genspace;
    genmsgstart(locn);            { begin message for REQUIRE }
    genmexpr(p);                { expression of assertion }
    genspaces(4);                { space before explaination }
    genchar('(');                { begin explaination }
    explaingen;                { generate explaination }
    genchar(')');                { end explanation }
    genmsgend;                { end message }
    genline;                { finish line }
    end;                    { With }
end {genrequire};
{
    genassertrequires  --  generate REQUIRE assertions for
                   SUMMARY, STATE, and ASSERT operators
}
procedure genassertrequires(p: ptn);
var subcode: longint;                { ASERT subcode }
    i: 0..maxarg;                { for arg count }
    scode: longint;                { subcode for explain }
{
    explainassert   --  explaination generator
}
procedure explainassert;
begin
    case scode of                { state kind of assertion }
    statesubcode: begin genstring15('STATE'); end; 
    summarysubcode: begin genstring15('SUMMARY'); end;
    assertsubcode: begin genstring15('ASSERT'); end;
        end;
    genstring15(' assertion');            { finish explaination }
end {explainassert};
begin {genassertrequires}
    with p^ do begin                { using ASERT node }
        assert(code = asertop);            { must be ASERT }
        subcode := disp;            { subcode }
        assert(subcode in [statesubcode, summarysubcode, assertsubcode]);
        for i := 1 to nrarg do begin        { for all args }
        scode := subcode;            { set subcode for msg }
        genrequire(linen,arg[i],@explainassert);{ generate assertion }
        end;
    end;                    { With }
end {genassertrequires};
{
    genalldef  --  generate predicate indicating that
               entire object is defined

    simple vars:     (defined! <object>) 
    structures:    (alltrue! (defined! <object>))

    May be applied to any selector expression
}
procedure genalldef(p: ptn);            { given node }
begin
    with p^ do begin                { using given node }
    if vtype^.down = nil then begin     { if leaf of type }
        gendefselector(p);            { gen selector as defined }
    end else begin                { if structured object }
        genstring15('(alltrue!');        { (alltrue! object) }
        genspace;
        gendefselector(p);            { gen selector as defined }
        genchar(')');            { finish alltrue }
        end;
    end;
end {genalldef};
{
    genalldefv  --  as above, but for entire variable
}
procedure genalldefv(v: varnodep;        { given varnode }
             newmode: gennewmode);    { set if new! desired }
begin
    assert(isbasevar(v));            { must not be part }
    with v^ do begin                { using given node }
    if down = nil then begin        { if leaf of type }
        gendataid(v,nulltid,newmode,genwithdef); { (defined! x) }
    end else begin                { if structured object }
        genstring15('(alltrue!');        { (alltrue! object) }
        genspace;
        gendataid(v,nulltid,newmode,genwithdef); { (defined! x) }
        genchar(')');            { finish alltrue }
        end;
    end;
end {genalldefv};    
{
    gentrueobject  --  generate an object with same form as object
               given, but with all leaves true.

    For arrays, we construct

        (arrayconstruct! <subobject> <lowbound> <highbound>)

    For records, we construct a record with all the fields stored
    with true objects.

    This is the new value of the definedness part when an assignment
    to an object occurs.
}
procedure gentrueobject(v: varnodep);        { var object }
{
    gentruefields  --  generate list of true fields for record
}
procedure gentruefields(w: varnodep);        { remaining fields }
begin
    if w = nil then begin            { if no more fields }
    genstring15('(emptyobject!)');        { empty object }
    end else with w^ do begin            { using current field }
    genstring15('(storer!');        { store in true value }
    genspace;
    gentruefields(right);            { remaining fields }
    genspace;
    gentypeid(up);                { type of entire record }
    genchar('$');                { gen type$field }
    genfieldid(w);                { name of field }
    genspace;
    gentrueobject(w);            { true obj of correct form }
    genchar(')');                { close storer }
    end;
end {gentruefields};
begin {gentrueobject}
    assert (v <> nil);                { must exist }
    with v^ do begin                { using varnode }
    if vardata.form = arraydata then begin    { if array }
        genchar('(');            
        genstring15('arrayconstruct!');    { construct array object }
        genspace;
        gentrueobject(down);        { object }
        genspace;
        genintconst(vardata.minvalue);    { low bound }
        genspace;
        genintconst(vardata.maxvalue);    { high bound of array }
        genchar(')');            { close arrayconstruct }
    end else if vardata.form = recorddata then begin { if record }
        gentruefields(down)            { gen true field list }
    end else begin                { if simple }
        genstring15('(true!)');        { gen true object }
        end;
    end;                    { With }
end {gentrueobject};
