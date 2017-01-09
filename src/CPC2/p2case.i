{
    Case Statement Jcode Generation

    CASE x OF
    c1:  <statement 1>
    c2:  <statement 2>
    ...
    cn:  <statement n>
    END

generates

    SAFE      [.x.]
    REQUIRE   [.x=c1 OR x=c2 OR ... x=cn.]
    SPLIT     1
    WHEN      [.x=c1.]  1
    SIDE      [.x.]
    CODE      <statement 1>
    BRANCH    (/CASE c1/) 2
    WHEN      [.x=c2.] 1
    SIDE      [.x.]
    CODE      <statement 2>
    BRANCH    (/CASE c2/) 2
    ...
    WHEN      [.x=cn.]
    SIDE      [.x.]
    CODE      <statement n>
    BRANCH    (/CASE cn/) 2
    JOIN      2

The ISO standard specifies that the constants c1, c2, ... cn must
be distinct, which is important because no ordering of cases is
implied by the above J-code.  Note that pass 2 of the verifier does
not check this; Pass 1 must do so.
}
{
    opcase  --  process case statement
}
procedure opcase(p: ptn);        { case node }
const maxcases = 256;            { max cases in case statement }
var casecodes: array [1..maxcases] of targetinteger; { case codes seen }
    caselines: array [1..maxcases] of lineinfo;    { lines of case codes }
    casecount: 0..maxcases;        { number of cases }
    beforelabel: labelid;        { start of case label }
    afterlabel: labelid;        { end of case label }
    i: 1..maxarg;            { for arg scan }
    caseexpr: ptn;            { expression in CASE statement }
    caseline: lineinfo;            { source line of CASE statement }
{
    checkunique  --  make sure case label is unique
}
procedure checkunique(ap: ptn);        { value node }
var i: 0..maxcases;            { for case constant }
    duplicate: boolean;            { true if duplicate found }
begin
    duplicate := false;            { no duplicate yet }
    with ap^ do begin            { using given node }
    if code <> literop then     { if not literal }
        verybadnode(ap,403);    { case constant not literal }
        for i := 1 to casecount do     { scan list }
        if casecodes[i] = disp then begin { if duplicate case constants }
        if caselines[i] <> linen then { print duplicated line }
            printsourceline(output,caselines[i]); { so note }
        usererrorstart(linen);     { note trouble }
        write(output, 'Case label ',disp:1,' appears twice.');
        usererrorend;        { end message }
        duplicate := true;    { note duplicate }
            end;
    if not duplicate then        { if no duplicate yet }
    if casecount >= maxcases then begin { if table full }
        usererrorstart(linen);    { begin message }
        write(output,'Too many labels in CASE (limit ',maxcases:1,')');
        usererrorend;
        casecount := 1;        { avoid more messages }
    end else begin            { if more space }
        casecount := casecount + 1;    { to next entry }
        casecodes[casecount] := disp; { save current entry }
        caselines[casecount] := linen; { and its location }
        end;
    end;                { With }
end {checkunique};            { end duplicate check }
{
    gencase  --  gen code for one case in case statement
}
procedure gencase(px: ptn);        { entry node }
var 
    ll: listitem;            { list control item }
begin
    with px^ do begin            { using given node }
    if code <> entryop then verybadnode(px,402); { non entry in case }
    genstring15('WHEN');        { begin case expression }
    genspace;
    liststart(ll,'or!');        { begin disjunction }
    for i := 1 to nrarg - 1 do begin{ for all args in list }
        checkunique(arg[i]);    { make sure value is unique }
        if i <> (nrarg - 1) then listmore(ll); { emit disjunction item }
        genstring15('(equal!');    { (equal! caseexpr const) }
        genspace;
        genjexpr(caseexpr);        { expression from CASE }
        genspace;
        genjexpr(arg[i]);        { gen constant value }
        genchar(')');        { end of equal }
        end;            { arg loop }
    listend(ll);            { end disjunction }
    genspace;
    geninteger(beforelabel);    { hook up WHEN to SPLIT }
    genline;            { finish case }
    statement(arg[nrarg]);        { last arg is statement }
    genstring15('BRANCH');        { finish case }
    genspace;
    {
        Path message generation.  Messages look like

        CASE n = 1
    or     CASE (n = 1) or (n = 2)
    }
    genmsgstart(linen);        { begin path message }
    genstring15('CASE');        { CASE message }
    genchar(' ');
    for i := 1 to nrarg - 1 do begin{ for all case constants }
        if i > 1 then begin        { if not first }
        genstring15(' or');    { disjunction }
        genchar(' ');
        end;
        if nrarg > 2 then genchar('('); { enclose in parens }
        genmexpr(caseexpr);        { case variable value }
        genchar(' ');
        genchar('=');
        genchar(' ');
        genmexpr(arg[i]);        { case constant value }
        if nrarg > 2 then genchar(')'); { enclose in parens }
        end;
    genmsgend;
    genspace;
    geninteger(afterlabel);        { branch to end of cases }
    genline;
    end;                { end With }
end {gencase};
{
    gencaserequire  --  generate REQUIRE that case variable is
                one of the valid values

    generates

        REQUIRE (or! (equal! (caseexpr) (consti! 1) (equal! (caseexpr) ...
}
procedure gencaserequire;
var i,j: 0..maxarg;            { for arg loops }
    n: 1..4;                { for spacing }
    lr: listitem;            { list control item }
begin 
    genstring15('REQUIRE');        { begin require }
    genspace;
    liststart(lr,'or!');        { begin disjunct }
    for i := 2 to p^.nrarg do begin    { for all args }
    for j := 1 to p^.arg[i]^.nrarg - 1 do begin { for all case labels }
        if (i <> p^.nrarg) or (j <> (p^.arg[i]^.nrarg-1)) then listmore(lr);
        genstring15('(equal! ');    { (equal! caseexpr const) }
        genspace;
        genjexpr(caseexpr);        { case argument }
        genspace;
        genjexpr(p^.arg[i]^.arg[j]);{ case expression }
        genchar(')');        { close equal }
        end;            { end inner loop }
    end;                { end outer loop }
    listend(lr);            { finish list }
    genmsgstart(caseline);        { line of case statement }
    genmexpr(caseexpr);            { case argument }
    genstring15(' in [');        { use set notation }
    for i := 2 to p^.nrarg do begin    { for all args }
    for j := 1 to p^.arg[i]^.nrarg - 1 do begin { for all case labels }
        genmexpr(p^.arg[i]^.arg[j]);{ this constant }
        if not ((i = p^.nrarg) and (j = p^.arg[i]^.nrarg - 1)) then
        genchar(',');        { separator }
        end;            { end inner loop }
    end;                { end outer loop }
    genchar(']');            { finish expr }
    for n := 1 to 4 do genchar(' ');    { mandatory 4 spaces }
    genstring15('(CASE statement');    { explaination }
    genchar(')');
    genmsgend;                { finish REQUIRE message }
    genline;                { finish REQUIRE }
end {gencaserequire};
begin
    beforelabel := nextlabel;        { get a new label }
    afterlabel := nextlabel;        { get a new label }
    casecount := 0;            { no cases yet }
    with p^ do begin            { using given node }
    assert(code = caseop);        { must be case }
    if nrarg < 1 then verybadnode(p,401); { case with no case arg }
    caseexpr := arg[1];        { get case expression }
    caseline := caseexpr^.linen;    { line of CASE statement }
    if nrarg < 3 then begin        { if not at least two cases }
        usererrorstart(caseline);    { bad case statement }
        write(output,'CASE statement needs at least two cases.');
        usererrorend;        { end message }
        end;
    safeexpr(caseexpr);        { validate control expr }
    gencaserequire;            { control expr one of casecodes }
    genstring15('SPLIT');        { initial branch }
    genspace;
    geninteger(beforelabel);    { label of SPLIT }
    genline;            { finish SPLIT }
    for i := 2 to nrarg do begin    { for all cases }
        gencase(arg[i]);        { gen this case }
        end;
    genstring15('JOIN');        { final join }
    genspace;
    geninteger(afterlabel);        { label of join }
    if comments then begin        { if debugging }
        gencomment(linen);        { identify end of case }
        genstring15('End CASE');
        end;
    genline;            { end JOIN }
    end;                { With }
end {opcase};
