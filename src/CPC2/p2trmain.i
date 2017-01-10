procedure WHATtrmain; const WHAT = '@(#)p2trmain.i    2.1'; begin SINK := WHAT; end;    { Version 2.1 of 10/12/82 }
{
    Transitive closure subpass  --  main processing
}
{
    Diagnosis for variables referenced but nowhere unassigned

    This check is in fact redundant since the verification pass
    would catch undefined variables, but the check is made since
    this is a common error and can be diagnosed in a more meaningful
    way here.
}
{
    diagnoseunset -- diagnose variables referenced but unassigned 
}
procedure diagnoseunset;
{
    checkunset  --  check if p is not set but is used
}
procedure checkunset(q: varnodep);        { node to check }
var qbase: varnodep;                { base of p }
begin
    qbase := basevariable(q);            { get base }
    with qbase^ do begin            { using given node }
    if not (vardata.form in            { if not program object }
        [functiondata, proceduredata, monitordata, moduledata, programdata])
        then begin                { is variable }
                        { check if VAR arg }
            if vardata.loc.relocation = pointeraddr then begin
            assert(up^.vardata.by = byreference); { must really be VAR arg }
        if not (qbase^.varused or qbase^.varset) then begin
            usererrorstart(qbase^.vardata.vrsource);
            write(output, 'VAR argument "');
            writestring15(output, qbase^.vardata.itemname);
            write(output, '" is neither used nor set.');
            usererrorend;        { end message }
            end;
        end else begin            { if ordinary object }
        if varused and (not varset) then begin { if ref without set }
            usererrorstart(vardata.vrsource); { begin message }
            write(output, 'Variable "');
            writestring15(output, vardata.itemname);
            write(output,'" is used but never set.');
            usererrorend;        { end message }
            end;            { end ref without set }
        end;                { end not by-ref }
        end;                { end is variable }
    end;                    { end With }
end {checkunset};
{
    diagnoseunset1  --  recursive descent for diagnose unset 
}
procedure diagnoseunset1(p: varnodep);
begin
    if p <> nil then                 { if node exists }
    with p^ do begin                { for the given node }
    diagnoseunset1(lesser);            { left subtree }
    checkunset(p);                { check p }
        diagnoseunset1(greater);               { right subtree }
    end;                    { of with }
end {diagnoseunset1};
begin
    diagnoseunset1(vartree^.greater);        { only rh part of tree is real }
end {diagnoseunset};
{
    findrecursion  --  find recursive procedures
}
procedure findrecursion;
var b: blocknodep;                { working block }
    pass: longint;                { pass number }
{
    explorerecursive  --  explore for recursive node

    A depth-first search on the "called by" relation.
}
procedure explorecalls(btry: blocknodep);    { node to explore from }
var c: callnodep;                { for call chaining }
begin
    with btry^ do begin                { using this block }
    if blpassnum <> pass then begin        { if not already explored }
        blpassnum := pass;            { mark as explored }
        c := blcallers;            { get caller chain }
        while c <> nil do begin        { for all callers }
        with c^ do begin        { using caller node }
            if clblock = b then begin    { if recursion found }
            b^.blrecursive := true;    { mark as recursive }
            end;            { end recursion found }
            if b^.blrecursive then begin { if recursion found }
            c := nil;        { force exit }
            end else begin        { if recursion not found }
            explorecalls(clblock);    { explore further }
            c := c^.clnext;        { on to next caller }
            end;            { end recursion not found }
            end;            { With }
        end;                { caller loop }
        end;                { end not already explored }
    end;                    { end With }
end {explorerecursive};
begin {explorecalls}
    pass := 0;                    { start pass counter }
    b := blockhead;                { clear pass numbers }
    while b <> nil do begin            { for all blocks }
    b^.blpassnum := 0;            { clear pass number }
    b := b^.blnext;                { on to next block }
    end;                    { end pass number clear }
    b := blockhead;                { get head of block chain }
    while b <> nil do begin            { for all blocks }
    pass := pass + 1;            { start next pass }
    explorecalls(b);            { start depth-first search }
    b := b^.blnext;                { on to next block }
    end;                    { end block loop }
end {findrecursion};
{
    tranpass  --  transitive closure subpass
}
procedure tranpass;                { transitive closure time }
begin
    markallformals;                { mark VAR args as set/used }
    actualsetuse;                { mark VAR actuals as set/used }
    markallcalls;                { propagate set/used lists }
    findrecursion;                { detect recursion }
    blockdrive(@classifyroutine);        { classify routines by type }
    blockdrive(@propagateroutinekind);        { propagate routine class }
    sharedvarfind;                { find shared variables }   
    diagnoseunset;                { diagnose unset vars }
    if debugg then dumpall(dbg);        { debug print }
end {tranpass};
