procedure WHATloop; const WHAT = '@(#)p2loop.i    2.8'; begin SINK := WHAT; end;    { Version 2.8 of 1/14/83 }
{
    Iteration statements - FOR, LOOP, WHILE, and UNTIL

    At the Icode level, there are only two kinds; FOR loops
    and LOOP loops, as WHILE and UNTIL loops are expressed
    with the LOOP operator.
}
{
    assertconjunct  --  generate conjunction from assertion
}
procedure assertconjunct(pconj: ptn);        { assertion }
var i,parens: 0..31;                { arg number, needed parens }
begin
    parens := 0;                { clear paren counter }
    with pconj^ do begin            { using given node }
    assert(code = asertop);            { must be assertion }
    if nrarg = 0 then begin            { zero case }
        genstring15('(true!)');        { must generate boolean }
        end;
    for i := 1 to nrarg do begin        { for all elements of conj }
        if i < nrarg then begin        { if more terms }
            genstring15('(and!');        { and them together }
        parens := parens + 1;        { need parens later }
            genspace;
        end;
        genjexpr(arg[i]);            { expression }
        end;
    end;                    { With }
    while parens > 0 do begin            { balance parens }
    genchar(')');                { balance }
    parens := parens - 1;            { count down }
    end;
end {assertconjunct};
{
    opfor  -- process a FOR statement
}
procedure opfor(p: ptn);
var forvararg: ptn;                { FOR control variable arg }
    loopvar: varnodep;                { actual FOR control variable }
    startarg: ptn;                { starting bound expression }
    stoparg: ptn;                { stopping bound expression }
    incrarg: ptn;                { increment arg }
    higharg, lowarg: ptn;            { high and low bound exprs }
    incr: -1..1;                { increment amount }
    stmtarg: ptn;                { statement }
    lowtemp, hightemp: tempid;            { low bound, high bound TEMPs }
    starttemp, stoptemp: tempid;        { stop bound TEMP (lo or hi) }
    label1, label2, label3, label4: labelid;    { these numbers match Jcode doc}
    invariantfound: boolean;            { communication with internal }
    forline: lineinfo;                { line number of FOR statement }
    endline: lineinfo;                { line number of end of FOR }
{
    forinvariant  --  handle invariant of for loop
}
procedure forinvariant(pinvar: ptn);        { invariant expression }
var i: cardinal;                { arg position }
begin
    genassertrequires(pinvar);            { require invariant expr }
    genstring15('HANG');            { HANG - break at invariant }
    if comments then begin            { commentary }
    gencomment(pinvar^.linen);        { line of STATE statement }
    genstring15('FOR invariant');        { identify }
    end;
    genline;                    { finish HANG }
    genstring15('WHEN');            { WHEN for break }
    genspace;
    genstring15('(true!)');            { unconditional branch }
    genspace;
    geninteger(label1);                { from first label }
    genline;                    { finish WHEN }
    genstring15('RENEW');            { RENEW the variables changed }
    genspace;
    genstring15('(and! (gei!');            { i >= lowarg }
    genspace;
    gendataid(loopvar,nulltid, genwithoutnew, genwithoutdef);    { loop variable (i) }
    genspace;
    gendataid(nil, lowtemp, genwithoutnew, genwithoutdef);    { low bound TEMP }
    genchar(')');                { close GEI }
    genspace;
    genstring15('(and! (lei!');            { i <= higharg }
    genspace;
    gendataid(loopvar,nulltid, genwithoutnew, genwithoutdef);    { loop variable (i) }
    genspace;
    gendataid(nil, hightemp, genwithoutnew, genwithoutdef);    { high bound TEMP }
    genchar(')');                { close LEI }
    genstring15('(and!');            { join defined and conjunct }
    genspace;
                        { (defined! <loopvar>) }
    genalldefv(loopvar,genwithoutnew);
    genspace;
    assertconjunct(pinvar);            { generate STATE }
    genchar(')');                { close above and! }
    genchar(')');                { close above and! }
    genchar(')');                { close above and! }
    genline;
end {forinvariant};
{
    forbody  --  process body of FOR statement

    Called once for each statement in direct FOR body
}
procedure forbody(pstmt: ptn);            { statement to process }
begin
    with pstmt^ do begin            { using given node }
    if (code = asertop) and (disp = statesubcode)
        and (not invariantfound) then begin    { found first STATE statement }
        invariantfound := true;         { loop invariant found }
        forinvariant(pstmt);        { handle invariant }
    end else begin                { if ordinary statement }
        statement(pstmt);            { handle normally }
        end;                { ordinary statement }
    end;                    { With }
end {forbody};
begin { opfor }
    {
    Preliminary processing before starting Jcode generation
    }
    with p^ do begin                { using given node }
    forvararg := arg[1];            { get for variable }
    startarg := arg[2];            { starting value }
    stoparg := arg[3];            { stopping value }
    incrarg := arg[4];            { increment arg }
    stmtarg := arg[5];            { statement part }
    end;
    forline := forvararg^.linen;        { FOR statement location }
    endline := p^.linen;            { end of entire FOR loop }
    label1 := nextlabel;            { assign flow label }
    label2 := nextlabel;            { assign flow label }
    label3 := nextlabel;            { assign flow label }
    label4 := nextlabel;            { assign flow label }
                        { check FOR variable }
    if forvararg^.code <> varblop then badnode(p,190); { bad FOR variable }
    loopvar := basevariable(forvararg^.vtype);    { get FOR variable }
    if forvararg^.vtype <> loopvar then
    badnode(forvararg, 192);        { FOR arg not simple var }
    if mttab[forvararg^.mtype].mtkind <> numericdata then
    badnode(forvararg, 194);        { FOR arg not numeric }
    if mttab[startarg^.mtype].mtkind <> numericdata then
    badnode(startarg, 195);            { FOR start value not numeric }
    if mttab[stoparg^.mtype].mtkind <> numericdata then
    badnode(stoparg, 196);            { FOR stop arg not numeric }
    checknotfrozen(forvararg, loopvar);        { FOR arg frozen test }
                        { get increment (+1 or -1) }
    incr := 1;                    { assume +1 increment }
    if incrarg^.code <> literop then 
    badnode(incrarg,191)             { bad FOR increment }
    else if (incrarg^.disp <> 1) and (incrarg^.disp <> -1) then 
        badnode(incrarg,193)        { not +1 or -1 }
    else incr := incrarg^.disp;        { get value of increment }
    {
    Begin Jcode generation
    }
    if comments then begin            { commentary }
    gencomment(forline);
    genstring15('Begin FOR loop');
    genline;
    end;
                            { handle bounds }
    { ***NEED FUNCTION SIDE EFFECT CLASH CHECK FOR BOUND EXPRS*** }
                        { save bounds in TEMP vars }
    safeexpr(startarg);                { validate start bound }
                        { bound must fit in type }
    requirecompat(p, loopvar, loopvar^.varmtype, startarg);
    starttemp := nexttemp;            { assign new temp number }
    gentempasg(starttemp, loopvar, startarg, true);{ TEMPnn := start bound }
    stoptemp := nexttemp;            { assign new temp number }
    safeexpr(stoparg);                { validate stop bound }
                        { bound must fit in type }
    requirecompat(p, loopvar, loopvar^.varmtype, stoparg);
    gentempasg(stoptemp, loopvar, stoparg, true);{ TEMPnn := stop bound }
    case incr of                { up/down handling }
    +1: begin                    { if TO loop }
    lowtemp := starttemp;            { get low TEMP number }
    lowarg := startarg;            { get low argument }
    hightemp := stoptemp;            { get high TEMP number }
    higharg := stoparg;            { get high argument }
    end;
    -1: begin                    { if DOWNTO loop }
    lowtemp := stoptemp;            { get low TEMP number }
    lowarg := stoparg;            { get low argument }
    hightemp := starttemp;            { get high TEMP number }
    higharg := stoparg;            { get high argument }
    end;
        end;                    { end cases }
    genstring15('ASSIGN');            { ASSIGN var := start }
    genspace;
    gendataid(loopvar,nulltid, genwithoutnew, genwithoutdef);    { (v) }
    genspace;
    gendataid(loopvar,nulltid, genwithoutnew, genwithoutdef);    { lhs }
    genspace;
    gentrueobject(loopvar);            { is defined }
    genspace;
    gendataid(nil, starttemp, genwithoutnew, genwithoutdef);    { starting value }
    if comments then begin            { commentary }
    gencomment(nulllineinfo);
    genstring15('Initialize loop');        { explain }
    end;
    genline;                    { finish line }
    genstring15('SPLIT');            { SPLIT label1 }
    genspace;
    geninteger(label1);                { label1 }
    genline;
    genstring15('WHEN');            { WHEN entrycondition }
    genspace;
    genstring15('(lei!');            { lo <= hi }
    genspace;
    gendataid(nil, lowtemp, genwithoutnew, genwithoutdef);    { low temp id }
    genspace;
    gendataid(nil, hightemp, genwithoutnew, genwithoutdef);    { high temp id }
    genchar(')');                { end lei }
    genspace;
    geninteger(label1);                { WHEN from label 1 }
    genline;                    { finish WHEN }
    genstring15('BRANCH');            { BRANCH to loop prebody }
    genmsgstart(forline);            { path message }
    genstring15('Enter FOR loop');        { path message }
    genmsgend;
    genspace;
    geninteger(label2);                { Body - label 2 }
    genline;                    { finish BRANCH }
    genstring15('JOIN');            { JOIN at loop prebody }
    genspace;
    geninteger(label2);                { Body - label 2 }
    genline;                    { finish JOIN }
    genstring15('REIN');            { REIN loopname }
    genline;                    { finish REIN }
    { ***REQUIRE loop variable in bounds *** }
    freeze(loopvar);                { freeze loop argument }
                            { do loop body }
    invariantfound := false;            { no invariant yet }
    seqdrive(stmtarg, @forbody);            { process loop body }
    if not invariantfound then begin        { if no invariant found }
    usererrorstart(forline);        { start error message }
    write(output,'This FOR loop lacks a STATE statement.');
    usererrorend;
    end;
    thaw(loopvar);                { thaw loop variable }
    genstring15('SPLIT');            { SPLIT label3 }
    genspace;
    geninteger(label3);
    genline;                    { finish SPLIT }
    genstring15('WHEN');            { WHEN continuing in loop }
    genspace;
    case incr of                { loopvar comparison }
        +1: genstring15('(lti!');        { loopvar < hibound }
        -1: genstring15('(gti!');        { loopvar > lowbound }
    end;
    genspace;
    gendataid(loopvar,nulltid, genwithoutnew, genwithoutdef);        { loop control variable }
    genspace;
    gendataid(nil, stoptemp,genwithoutnew, genwithoutdef);    { high bound TEMP }
    genchar(')');                { close lti }
    genspace;
    geninteger(label3);                { one of three cases }
    genline;                    { end WHEN }
    case incr of                { check for overflow }
    -1: genboundrequire(forvararg, loopvar^.vardata.minvalue + 1,icgeop,p);
    +1: genboundrequire(forvararg, loopvar^.vardata.maxvalue - 1,icleop,p);
    end;
    genstring15('ASSIGN');            { ASSIGN i := i + incr }
    genspace;
    gendataid(loopvar,nulltid, genwithoutnew, genwithoutdef);        { loop control variable }
    genspace;
    gendataid(loopvar,nulltid, genwithoutnew, genwithoutdef);        { loop control variable }
    gentrueobject(loopvar);            { is defined }
    genspace;
    genstring15('(addi!');            { i := i + incr }
    genspace;
    gendataid(loopvar,nulltid, genwithoutnew, genwithoutdef);        { loop control variable }
    genspace;
    genintconst(incr);                { constant +1 or -1 }
    genchar(')');                { close addi }
    if comments then begin            { commentary }
        gencomment(forline);            { commentary }
    case incr of                { fan out on increment }
    +1: genstring15('increment');
    -1: genstring15('decrement');
    end;
    genstring15(' loop control');        { explain }
    end;                    { commentary }
    genline;                    { finish ASSIGN of loop ctl }
    genstring15('REOUT');            { REOUT bracket close }
    genline;                    { finish REOUT }
    genstring15('BRANCH');            { BRANCH back to top of loop }
    genspace;
    genmsgstart(endline);            { line number of end of loop }
    genstring15('Back to top');
    genstring15(' of FOR loop');
    genmsgend;
    genspace;
    geninteger(label2);                { back to loop body }
    genline;                    { finish BRANCH }
    genstring15('WHEN');            { WHEN loop complete }
    genspace;
    genstring15('(equal!');            { note = test for loop exit }
    genspace;
    gendataid(loopvar,nulltid, genwithoutnew, genwithoutdef);        { loop control variable }
    genspace;
    if incr > 0 then begin            { if upward loop }
    gendataid(nil, hightemp, genwithoutnew, genwithoutdef);    { compare with high bound }
    end else begin                { if downward loop }
    gendataid(nil, lowtemp, genwithoutnew, genwithoutdef);    { compare with low bound }
    end;
    genchar(')');                { close equal }
    genspace;
    geninteger(label3);                { other case at loop end }
    if comments then begin            { commentary }
    gencomment(nulllineinfo);        { -- loop exit path }
    genstring15('loop exit path');
    end;
    genline;                    { finish WHEN }
    genstring15('BRANCH');            { BRANCH to loop exit }
    genmsgstart(forline);            { line of FOR loop }
    genstring15('FOR loop exit');
    genmsgend;
    genspace;
    geninteger(label4);                { to loop exit }
    genline;
    genstring15('WHEN');            { WHEN loop never entered }
    genspace;
    genstring15('(gti! (');            { lo > hi }
    gentempid(lowtemp);                { low temp id }
    genstring15(') (');                { args in parens }
    gentempid(hightemp);            { high bound temp id }
    genchar(')');                { end temp }
    genchar(')');                { end lei }
    genspace;
    geninteger(label1);                { WHEN from label 1 }
    genline;                    { finish WHEN }
    genstring15('BRANCH');            { BRANCH to end of loop }
    genmsgstart(forline);            { line from FOR }
    genstring15('FOR loop never');        { "FOR loop never entered" }
    genstring15(' entered');
    genmsgend;
    genspace;
    geninteger(label4);                { to loop exit }
    genline;                    { finish BRANCH }
    genstring15('JOIN');            { JOIN exit paths }
    genspace;
    geninteger(label4);                { exit paths }
    genline;
    genstring15('ASSIGN');            { ASSIGN loopvar to nothing }
    genspace;
    gendataid(loopvar,nulltid, genwithoutnew, genwithoutdef);    { variable list }
    genspace;
    gendataid(loopvar,nulltid, genwithoutnew, genwithoutdef);    { lhs of assign }
    genspace;
    genstring15('(false!)');            { now undefined }
    genspace;
    gendataid(loopvar,nulltid, genwithoutnew, genwithoutdef);    { i := i }
    if comments then begin            { if commenting }
    gencomment(nulllineinfo);
    genstring15(loopvar^.vardata.itemname);
    genstring15(' is not defined');
    end;
    genline;                    { finish ASSIGN }
    if comments then begin            { commentary }
    gencomment(endline);            { related to FOR loop }
    genstring15('End FOR loop');
    genline;
    end;
end {opfor};
{
    oploop  --  generate Jcode for loop operator

    The LOOP operator is used for WHILE, REPEAT, and LOOP
    statements.
}
procedure oploop(p: ptn);            { loop operator }
type exititem = record                { loop EXIT item }
    excond: ptn;                { exit condition }
    exstmt: ptn;                { statement after exit }
    exlabel: labelid;            { label for Jcode for exstmt }
    end;
var statearg: ptn;                { STATE arg }
    exittab: array [1..maxarg] of exititem;    { exit information table }
    exitlim: 0..maxarg;                { exit limit }
    loopline, endline: lineinfo;        { start and end of loop }
    forever: boolean;                { true if deliberate forever }
    statefound: boolean;            { true if STATE found }
    measuretid: tempid;                { id for MEASURE variable }
    flagtid: tempid;                { id for Jcode flag }
    j: cardinal;                { for loops }
    label0, label1, label2, label3: labelid;    { these numbers match Jcode doc}
    warg: ptn;                    { working arg }
{
    loopsetup  --  Set up variables for LOOP scan
}
procedure loopsetup;
var i: 0..maxarg;                { working arg position }
    parg: ptn;                    { working node }
begin
    label0 := nextlabel;            { create unique label }
    label1 := nextlabel;            { create unique label }
    label2 := nextlabel;            { create unique label }
    label3 := nextlabel;            { create unique label }
    forever := false;                { if deliberate forever loop }
    statefound := false;            { no STATE seen yet }
    statearg := nil;                { no STATE seen yet }
    exitlim := 0;                { no EXITs yet }
    with p^ do begin                { using loop node }
    assert(code = loopop);            { must be loop operator }
    endline := linen;            { last line of loop }
    loopline := linen;            { first line of loop }
    if nrarg > 0 then            { if any args }
        if arg[1] <> nil then        { if non-null }
        loopline := arg[1]^.linen;    { first line of loop }
    end;                    { With }
{
    Infinite loop check - considered to be deliberate infinite
    loop if WHILE TRUE or REPEAT .. UNTIL FALSE
}
    if p^.nrarg in [2,3] then begin        { if only one EXIT operator }
    if p^.arg[2]^.code <> exitop then     { if EXIT not where expected }
        verybadnode(p^.arg[2],268);     { missing EXIT }
    with p^.arg[2]^.arg[1]^ do begin    { with cond arg of EXIT }
        if code = literop then         { if constant EXIT }
        if mtype = b1 then        { if boolean value }
            if disp = 0 then        { if FALSE }
            forever := true;    { this is a forever loop }
        if code = notop then        { if NOT in EXIT cond }
        if arg[1]^.code = literop then  { if NOT const }
            if arg[1]^.mtype = b1 then     { if boolean value }
            if arg[1]^.disp = 1 then{ if TRUE }
                forever := true;    { this is a forever loop }
        end;                { end With }
    end;                    { end only one EXIT operator }
end {loopsetup};
{
    loopinvariant  -- generate loop invariant and measure
}
procedure loopinvariant(pstate: ptn;        { node of STATE }
            pmeasure: ptn);     { expression from MEASURE }
begin
    if pstate = nil then begin            { if no STATE }
    assert(pmeasure <> nil);        { should not be called nil/nil }
    diag(pmeasure^.linen,'MEASURE does not immediately follow STATE');
    end else begin                { if STATE found }
    if statefound then             { if STATE already found }
        diag(pstate^.linen,'More than one STATE assertion in loop');
    statefound := true;            { note STATE now found }
    if forever and (pmeasure <> nil) then begin { if spurious MEASURE }
        diag(pmeasure^.linen,'MEASURE present in deliberate infinite loop');
        measuretid := nexttemp;        { avoid blowup later }
        end;
    genassertrequires(pstate);        { require STATE assertions }
    if pmeasure = nil then begin        { if no MEASURE statement }
        if not forever then            { if EXITs present }
        diag(pstate^.linen,'A MEASURE statement should follow here');
    end else begin                { if MEASURE present }
                        { if not numeric }
        if mttab[pmeasure^.mtype].mtkind <> numericdata then 
            badnode(pmeasure,262);        { non-numeric MEASURE }
        safeexpr(pmeasure);            { validate measure expr }
                        { check compat with type }
        requirecompat(pmeasure,cardinalvarnode,u15,pmeasure);
        genstring15('REQUIRE');        { REQUIRE measure decreasing }
        genspace;
        genstring15('(or!');        { firstflag or new < old }
        genspace;
        gendataid(nil,flagtid,genwithoutnew, genwithoutdef);    { flag }
        genspace;
         genstring15('(lti!');        { new measure < saved measure }
         genspace;
         genjexpr(pmeasure);            { new measure }
         genspace;
         gendataid(nil,measuretid,genwithoutnew, genwithoutdef);    { (<measure temp>) }
         genchar(')');                { close lti }
         genchar(')');                { close or }
         genmsgstart(pmeasure^.linen);    { message for REQUIRE }
         genstring15('MEASURE');
         genstring15(' decreased');
         genstring15(' by loop body.');
         genmsgend;
         genline;                { finish REQUIRE }
         end;                { end MEASURE found }
    genstring15('HANG');            { break loop }
    if comments then begin            { commentary }
        gencomment(pstate^.linen);
        genstring15('Break LOOP');
        end;
        genline;
        genstring15('JOIN');            { join with loop }
        genspace;
        geninteger(label1);            { loop state label }
        genline;
        genstring15('RENEW');            { Smash as required by STATE }
        genspace;
    assertconjunct(pstate);            { generate conjunction }
        genline;                { end RENEW }
    end;                    { end STATE found }
    if pmeasure <> nil then begin        { if non-forever loop }
    genstring15('REQUIRE');            { REQUIRE measure >= 0) }
    genspace;
    genstring15('(gei!');            { comparison }
    genspace;
    genjexpr(pmeasure);            { new value of measure }
    genspace;
    genintconst(0);                { constant 0 }
    genchar(')');                { close gei }
    genspace;
    genmsgstart(pmeasure^.linen);        { line number of measure }
    genstring15('MEASURE >= 0');        { message text }
    genmsgend;
    genline;                { finish REQUIRE }
    genstring15('ASSIGN');            { ASSIGN measure }
    genspace;
    gendataid(nil,measuretid,genwithoutnew, genwithoutdef);    { (TEMPnn) }
    genspace;
    gendataid(nil,measuretid,genwithoutnew, genwithoutdef);    { (TEMPnn) as lhs }
    genspace;
    genstring15('(true!)');            { MEASURE is defined }
    genspace;
    genjexpr(pmeasure);            { new value of measure }
    if comments then begin            { commentary }
        gencomment(pmeasure^.linen);    { begin comment }
        genstring15('Decrement');
        genstring15(' MEASURE');
        end;
    genline;
    genstring15('ASSIGN');            { ASSIGN not entered flag }
    genspace;
    gendataid(nil,flagtid,genwithoutnew, genwithoutdef);    { (TEMPnn) }
    genspace;
    gendataid(nil,flagtid,genwithoutnew, genwithoutdef);    { (TEMPnn) }
    genspace;
    genstring15('(true!)');            { flag is defined }
    genspace;
    genstring15('(false!)');        { new value of flag }
    if comments then begin            { commentary }
        gencomment(pmeasure^.linen);    { begin comment }
        genstring15('Clear first');
        genstring15(' time flag');
        end;
    genline;
    end;                    { end non-forever }
end {loopinvariant};
{
    genloopexit  --  handle EXIT in body of loop
}
procedure genloopexit(p: ptn);            { EXIT node }
var exitid: labelid;                { label for jcode exit part }
begin
    if p <> nil then                { if node present }
    with p^ do begin                { using EXIT node }
    if code = exitop then begin        { if this is EXIT operator }
        exitid := nextlabel;        { get label for exit }
        safeexpr(arg[1]);            { validate exit condition }
        if arg[1]^.mtype <> b1 then        { if not boolean }
        badnode(arg[1],260);        { EXIT condition not boolean }
        genstring15('SPLIT');        { SPLIT to later exit }
        genspace;
        geninteger(exitid);            { exit label for this exit }
        genline;
        genstring15('WHEN');        { WHEN not exit cond }
        genspace;
        genstring15('(not!');        { not cond }
        genspace;
        genjexpr(arg[1]);            { exit condition }
        genchar(')');            { close NOT }
        genspace;
        geninteger(exitid);            { exit label for this exit }
        if comments then begin        { commentary }
        gencomment(arg[1]^.linen);    { EXIT cond not taken }
        genstring15('EXIT not taken:');
        genmexpr(arg[1]);        { expression }
        end;
        genline;                { finish WHEN }
                        { record data for EXIT code }
        assert(exitlim < maxarg);        { cannot overflow }
         exitlim := exitlim + 1;        { increment exit count }
        with exittab[exitlim] do begin     { using exit item }
        excond := arg[1];        { save exit condition }
        exstmt := arg[2];        { save exit statement }
        exlabel := exitid;        { save exit label number }
        end;                { end With }
    end else begin                { non-null, non-exit }
        badnode(p,266);            { non-EXIT in LOOP list }
        end;
    end;                    { With }
end {genloopexit};
{
    loopstatechk  --  check for STATE statement previous

    Coordinates STATE and MEASURE
}
procedure loopstatechk;
begin
    if statearg <> nil then            { if STATE was last }
    loopinvariant(statearg,nil);        { STATE without MEASURE }
    statearg := nil;                { STATE not last now }
end {loopstatechk};
{
    genloopbody  --  handle statement in body of loop 

    Invoked, in sequence, on each statement in the loop.
}
procedure genloopbody(w: ptn);
var exitid: longint;                { generated exit number }
begin
    with w^ do begin                { using this statement }
    if (code = asertop) and (disp = statesubcode) then begin { if STATE }
        statearg := w;            { save STATE arg }
    end else if code = measop then begin    { MEASURE statement }
        loopinvariant(statearg,arg[1]);    { handle loop invariant }
        statearg := nil;            { state arg now used up }
    end else begin                { normal statement }
        loopstatechk;            { emit STATE if pending }
        statement(w);            { do normal statement }
        end;                { end normal statement }
    end;
end {genloopbody};
{
    finishloop  --  generate code for statements after EXIT
              in loop 
}
procedure finishloop;
var i: cardinal;                { for loop }
    exitline: lineinfo;                { location of exit line }
begin
    for i := 1 to exitlim do begin        { for all loop exits }
    with exittab[i] do begin        { using this one }
        genstring15('WHEN');        { WHEN exitcond }
        genspace;
        genjexpr(excond);            { exit condition }
        genspace;
        geninteger(exlabel);        { label of this exit }
        if comments then begin        { commentary }
        gencomment(excond^.linen);
        genstring15('EXIT IF');
        genchar(' ');
        genmexpr(excond);        { exit condition }
        end;                { commentary }
        genline;                { finish WHEN }    
        statement(exstmt);            { statements after THEN }
        genstring15('BRANCH');        { BRANCH loop exit }
        if exstmt <> nil then        { if statement part present }
        exitline := exstmt^.linen    { use end of statement loc }
        else                { if no statement part }
        exitline := excond^.linen;    { use end of condition }
        genmsgstart(exitline);        { to end }
        genstring15('loop exit');        { path message }
        genmsgend;
        genspace;
        geninteger(label3);            { final label of loop }
        genline;
        { ***NEED SIDE EFFECT CHECK FOR EXIT CONDITION*** }
        end;                { end With }
    end;                    { end for }
                        { final diagnosis }
    if not statefound then             { if STATE never found }
    diag(loopline,'No STATE statement in this LOOP');
end {finishloop};
begin {oploop}
    loopsetup;                    { initialize }
    begin
    if comments then begin            { commentary }
        gencomment(loopline);        { top of loop }
        if forever then begin        { if infinite loop }
        genstring15('Infinite');
        genchar(' ');
        end;
        genstring15('LOOP');        { explain }
        genline;
        end;
                        { initialize MEASURE }
    if not forever then begin        { if not infinite loop }
            measuretid := nexttemp;        { MEASURE value TEMP id }
        flagtid := nexttemp;        { flag value TEMP id }
                { initial value of old MEASURE is arbitrary }
        gentempasg(measuretid,cardinalvarnode,zeroexpr,true);
                { initial value of flag is true }
        gentempasg(flagtid,booleanvarnode,trueexpr,true);
        end;                { end not infinite loop }
    genstring15('REIN');            { begin loop body }
    if comments then begin            { commentary }
        gencomment(p^.linen);        { begin comment }
        genstring15('LOOP body');
        end;
    genline;
    genstring15('SPLIT');            { SPLIT for loop entry cases }
    genspace;
    geninteger(label0);            { label for dummy WHENs }
    genline;
    genstring15('WHEN');            { WHEN true }
    genspace;
    genstring15('(true!)');
    genspace;
    geninteger(label0);            { match SPLIT above }
    genline;
    genstring15('BRANCH');            { BRANCH to prebody }
    genspace;
    genmsgstart(loopline);            { explain path }
    genstring15('first time');
    genstring15(' through loop');
    genmsgend;
    genspace;
    geninteger(label2);
    genline;
    genstring15('WHEN');            { WHEN true }
    genspace;
    genstring15('(true!)');
    genspace;
    geninteger(label0);            { match SPLIT above }
    genline;
    genstring15('BRANCH');            { BRANCH to loop state }
    genmsgstart(loopline);            { explain path }
    genstring15('advance to');
    genstring15(' loop state');
    genmsgend;                { finish path explaination }
    genspace;
    geninteger(label1);            { to loop state }
    genline;                { finish BRANCH }
    genstring15('JOIN');            { JOIN top of loop }
        genspace;
    geninteger(label2);            { top of loop label }
    genline;
    with p^ do begin            { doing body of loop }
        for j := 1 to nrarg do begin    { for all args }
        warg := arg[j];            { get this arg }
        if warg <> nil then begin    { if non-null }
            if warg^.code = exitop then begin { if EXIT }
                genloopexit(warg);    { handle EXIT }
            end else begin        { if not EXIT }
                seqdrive(warg, @genloopbody);{process statement sequence }
                end;            { end non EXIT }
            end;            { end non-null }
        end;                { for }
        end;                { of With }
    loopstatechk;                { handle STATE at end }
    genstring15('REOUT');            { finish body of loop }
    if comments then begin            { commentary }
        gencomment(endline);        { end of loop }
        genstring15('LOOP end');
        end;
    genline;
    genstring15('BRANCH');            { do loop end }
    genmsgstart(endline);            { end of loop }
    genstring15('loop back');        { back to top of loop }
    genmsgend;                { finish message }
    genspace;
    geninteger(label2);            { to loop top }
    genline;
    finishloop;                { do EXIT clauses }
    genstring15('JOIN');            { final join }
    genspace;
    geninteger(label3);            { referenced by each exits }
    genline;
    end;                    { will generate jcode }
end {oploop};
