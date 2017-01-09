procedure WHATcall; const WHAT = '@(#)p2call.i    2.6'; begin SINK := WHAT; end;    { Version 2.6 of 1/31/86 }
{
    Procedure Call Processing   --  Improved 1982 model implementation

    Procedure/function call processing is the most complex
    part of Jcode generation.  The basic approach is simple, but
    there are complicating factors.  These factors make the
    implementation significantly more obscure than would otherwise
    be the case.

    The basic approach taken is to represent a procedure call in
    Jcode as a set of requirements imposed on the input variables
    followed by a change to the output variables such that the
    exit assertions of the procedure hold.  The Jcode generated
    can thus be summarized as

       ASSIGNs to move variables to TEMP names
       REQUIREs for entry assertions
       NEW for output globals and TEMPs for output formals
       ASSIGNs to move output formal TEMPs to actual argument expresssions.

    This structure is complicated by the following considerations:

    1.  Input arguments might be expressions whose evaluation
        would cause an error.  The routine "safeexpr" handles this.

    2.  An aliasing condition might exist between arguments to the
        routine.  This is handled in "aliaschk".

    3.  Variables may be mentioned in exit conditions in an ".old"
        form; thus the original values of formal arguments must be
        saved.  This is handled in "inputsave".

    4.  The routine might modify a variable used as a subscript in
        a VAR actual argument.  Since the implementation actually
        calculates such subscripts only once, before the call, the
        data object indicated by the VAR argument never really changes
        during the call.  The verifier emulates these semantics by
        saving the original values of all output variables and
        using these variables in subscript expressions in the final
        ASSIGNs used to move output formal TEMPs to actual argument
        expressions.  This is handled in "formaltoactual".
}
{
    localcall  --  is call to routine a local one?

    A call is local if the call does not cross any module boundary.
    The outermost exported point of the callee (the dominator of
    its calls) must be the same as the next block outward of the
    caller.
}
function localcall(caller: blocknodep;        { calling block }
           callee: blocknodep)        { called block }
           : boolean;            { true if local }
begin
    localcall := (caller^.blouterblock = callee^.bldominator);
end {localcall};
{
    bindtemps  --  set substitutions for given set of temps
}
procedure bindtemps(var tinfo: temptab;        { TEMP info - read only }
            bindset: setoftempkind;    { kinds to bind }
            bindtoold: boolean;        { bind to .old if true }
            bindasnew: gennewmode);    { bind as new! if true }
var i: 1..temptabmax;                { for scanning temp tab }
begin
    for i := 1 to tinfo.tttop do begin        { for all in table }
    with tinfo.tttab[i] do begin        { using given item }
        if tekind in bindset then begin    { if of requested type }
        setsubstitute(tevarnode,bindtoold,tenum,bindasnew);
        end;
        end;
    end;
end {bindtemps};
{
    setresulttemp  --  bind TEMP for function result to
               fncall node in icode tree.  

    Later references to expressions containing this function call
    will be generated as references to the TEMP name.
}
procedure setresulttemp(p: ptn;            { fcall node to bind to }
            tid: tempid);        { TEMP id to be bound }
begin
    with p^ do begin                { using fcall node }
    assert(code = fcallop);            { must be function call }
    assert(ndfntemp.ftstamp <> clockserial);{ must not be timestamped now }
    ndfntemp.ftstamp := clockserial;    { stamp with current time }
    ndfntemp.fttemp := tid;            { associate given TEMP num }
    end;                    { With }
end {setresulttemp};
{
    settempinfo  --  save TEMP variable info for later use
}
procedure settempinfo(var tinfo: temptab;    { temp tab to add to }
              basevar: varnodep;    { object TEMP refers to }
              tnum: tempid;        { TEMPnn nn part }
              tkind: tempkind);        { kind of TEMP }
begin
    assert(tnum > 0);                { must be valid temp }
    if tinfo.tttop >= temptabmax then         { if overflow }
    internalerror(179);            { temp tab overflow }
    tinfo.tttop := tinfo.tttop + 1;        { next in table }
    with tinfo.tttab[tinfo.tttop] do begin    { using new entry }
    tekind := tkind;            { set kind of temp }
    tenum := tnum;                { TEMP number }
    tevarnode := basevar;            { relevant variable }
    end;                    { With }
end {settempinfo};
{
    inputsave  --  save input values in TEMP variables for later use

    Values of actual arguments are saved and bound to the formal arguments.
    Values of input global arguments are saved for use in ".old" forms.
    Values of base variables of output VAR arguments and output global
        arguments are saved for use in subscript expressions of output
        arguments.
    
    The saving of base actual variables is ommitted if there are no
    subscript expressions in output VAR arguments, as only such expressions
    need the original values of the input variables.
}
procedure inputsave(p: ptn;            { call node }
            var tinfo: temptab);    { TEMP information }
var subscriptinactual: boolean;            { if subscripted }
    tnum: tempid;                { working TEMP number }
    q, baseq: varnodep;                { working varnode }
    basea: varnodep;                { base of actual selector }
    wref: refnodep;                { working ref item }
    argpos: 0..maxarg;                { actual arg position }
    callee: blocknodep;                { routine being called }
{
    savebaseactual  --  save base of actual variable
}
procedure savebaseactual(sbase: varnodep;    { object to save }
             skind: tempkind);    { kind of object }
var snum: tempid;                { new TEMP number }
begin
    snum := nexttemp;                { assign TEMP number }
    settempinfo(tinfo,sbase,snum,skind); 
    gentempasg(snum, sbase, nil, false);    { save input formal value }
end {savebaseactual};
begin {inputsave}
    with p^ do begin                { using call node }
    callee := vtype^.blockdata;        { get called block }
                        { formals processing }
    q := firstformal(vtype);        { get first formal }
    argpos := 0;                { start arg count }
    subscriptinactual := false;        { if not subscripted }
    while q <> nil do begin            { for all formals }
        argpos := argpos + 1;        { get actual arg number }
        baseq := basevariable(q);        { get base of formal }
        tnum := nexttemp;            { assign TEMP number }
        settempinfo(tinfo,baseq,tnum,inputformaltemp); 
                        { save input formal value }
        gentempasg(tnum, baseq, arg[argpos], q^.vardata.by = byactualvalue);
        if q^.vardata.by = byreference then { if VAR arg }
        if baseq^.varset then        { if output arg }
                if not subscriptinactual then { if no subscript seen }
            if ifsubscripted(arg[argpos]) then { if subscripted }
                subscriptinactual := true; { is difficult case }
        q := q^.right;            { on to next formal }
        end;
                        { base actuals saving }
    if subscriptinactual then begin        { if difficult case }
        argpos := 0;            { start actual count }
        q := firstformal(vtype);        { get first formal arg }
        while q <> nil do begin        { for all args }
            argpos := argpos + 1;        { get actual arg number }
            baseq := basevariable(q);    { base of q }
            if q^.vardata.by = byreference then { if VAR arg }
            if baseq^.varset then begin    { if output VAR arg }
                basea := basevariable(arg[argpos]^.vtype); { base var }
            savebaseactual(basea,inputbaseactualtemp); 
            end;
        q := q^.right;            { on to next formal }
        end;                { loop on args }
                        { output globals saving }
        wref := callee^.blrefs;        { get first ref }
        while wref <> nil do begin        { for all refs }
        with wref^ do begin        { using this ref }
            if refkind = setref then begin { if output }
                        { if visible to caller }
            if visible(refvar,lastblockp) then 
                savebaseactual(refvar,inputglobaltemp); 
            end;
            wref := wref^.refnext;    { on to next ref }
            end;            { end With }
        end;                { end ref loop }
        end;                { end must save old val of outs}
                        { input globals saving }
    wref := callee^.blrefs;            { save input values of globals }
    while wref <> nil do begin        { for all refs }
        with wref^ do begin            { using this ref }
        if refkind = useref then begin    { if input }
            if visible(refvar,lastblockp) then { if visible to caller }
                savebaseactual(refvar,inputglobaltemp);{ save this ref }
            end;
        wref := wref^.refnext;        { on to next ref }
        end;                { end With }
        end;                { end ref loop }
    end;
end {inputsave};
{
    inputentrygen  --  generate REQUIREs for entry conditions
}
procedure inputentrygen(callnode: ptn);        { relevant call node }
var callee: blocknodep;                { relevant block }
{
    entryexplaingen  --  generate explaination for ENTRY condition

    (entry assertion of call at xxx.pf:25)
    Entry conditions and routine invariants must be proven.
    Module/monitor invariants are assumed.
}
procedure entryexplaingen(p: ptn);        { assertion to explain }
begin
    gensubcode(p^.disp);            { entry or invariant }
    genstring15(' of "');
    genstring15(callnode^.vtype^.vardata.itemname); { name of callee }
    genchar('"');
    genchar(' ');
    genlineid(p^.linen);            { location }
end {entryexplaingen};
begin {inputgenrequries}
    with callnode^ do begin            { using call node }
    assert(code = callop);            { must be call }
    callee := vtype^.blockdata;        { called block }
    { generate the entry requries, using entryexplaingen to gen msgs }
    genspecrequires(callnode, callee^.blassertions, 
        [entrysubcode, entryexitsubcode], entryexplaingen);
    end;                    { With }
end {inputentrygen};
{
    requireinvariants  --  require invariants on call path

    For each block crossed going outward to the dominator of all
    calls to the callee, the block invariants must be generated.
}
procedure requireinvariants(callnode: ptn;    { call operator }
                caller: blocknodep);{ calling block }
var b: blocknodep;                { working block }
    callee: blocknodep;                { called block }
    dom: blocknodep;                { dominator of caller/callee }
{
    invarexplaingen  --  generate explanations for these requires

e.g.:    invariant of module "xyz" at call of procedure "xyz" at p.pf:92
}
procedure invarexplaingen(p: ptn);        { relevant vdecl operator }
begin
    genstring15('invariant of');
    genchar(' ');
    case b^.blvarnode^.vardata.form of        { monitor or module? }
    monitordata: genstring15('monitor "');
    moduledata: genstring15('module "');
    end;
    genstring15(b^.blvarnode^.vardata.itemname); { name of monitor/module }
    genstring15('" at');
    genchar(' ');
    genstring15('call of');            { if before call }
    genchar(' ');
    genchar('"');
    genstring15(callnode^.vtype^.vardata.itemname); { name of callee }
    genchar('"');
    genchar(' ');
    genlineid(p^.linen);            { location }
end {invarexplaingen};
begin { requireinvariants }
    callee := callnode^.vtype^.blockdata;    { get called block }
    dom := callee^.bldominator;            { get outermost common block }
    b := caller;                { scan from caller }
    if b <> dom then b := b^.blouterblock;    { do not want own block invars }
    assert(b <> nil);                { may not overrun main prog }
    while b <> dom do begin            { scan to dominator block }
                        { generate requires }
    assert(b <> nil);            { must find dominator }
    if b^.blvarnode^.vardata.form in [moduledata, monitordata] then
        genspecrequires(callnode, b^.blassertions,
        [invariantsubcode],invarexplaingen);
    b := b^.blouterblock;            { get next outer block }
    end;                    { end search for dominator }
end {requireinvariants};
{
    outputimplicitchk  --  true if implicit arg is an output argument
}
function outputimplicitchk(callee: blocknodep;    { procedure being called }
            vref: refnodep;        { implicit arg }
            caller: blocknodep)    { caller }
            : boolean;        { true if relevant }
begin
    outputimplicitchk := (vref^.refkind in [setref, initref]) { if output }
    and visible(vref^.refvar,caller);    { and visible in caller }
end {outputimplicitchk};
{
    outputnewlist  --  generate variable list for NEW operator
               for exit assertion.
}
procedure outputnewlist(p: ptn;            { call node }
            fnnode: ptn;        { function call node or nil }
            var tinfo: temptab;    { TEMP info }
            var anyout: boolean);    { true if anything output }
var warg: varnodep;                { base of varg }
    varg: varnodep;                { working arg }
    tnum: tempid;                { working temp number }
    vref: refnodep;                { position in ref list }
    callee: blocknodep;                { block being called }
    outvar: varnodep;                { arg from ref list }
{
    outputnewstart  --  start NEW statement if not already started
}
procedure outputnewstart;
begin
    if not anyout then begin            { if NEW needed }
        genstring15('NEW (');            { begin NEW statement }
    anyout := true;                { there is output }
    end else begin                { if NEW already }
        genspace;                { space between args }
        end;
end {outputnewstart};
{
    outputfnnew  --  output function entry in NEW list
}
procedure outputfnnew;
var tnum: tempid;                { TEMP number for fn result }
    resultv: varnodep;                { result of function }
begin
    assert(fnnode <> nil);            { must have fcall node }
    tnum := nexttemp;                { get next temp cell }
    resultv := basevariable(fnnode^.vtype);    { result of function }
    settempinfo(tinfo,resultv,tnum,outputresulttemp); { note for later ref }
    setresulttemp(fnnode,tnum);            { tag result node with expr }
    outputnewstart;                { start NEW( }
    gentempid(tnum);                { gen TEMPnn }
    genchar(':');                { prepare for type }
    genspace;
    genjtype(p^.vtype^.down);            { gen type of result }
end {outputfnnew};
begin {outputnewlist}
    anyout := false;                { true if no output yet }
    with p^ do begin                { using given call node }
    callee := vtype^.blockdata;        { relevant block }
    if isfunction(callee^.blvarnode) then    { if is function }
        outputfnnew;            { output function NEW }
    varg := firstformal(vtype);        { get first formal }
    while varg <> nil do begin        { for all explicit args }
        warg := basevariable(varg);        { get base of arg }
        tnum := 0;                { assume no temp number }
        if varg^.vardata.by = byreference then { if VAR arg }
            if warg^.varset then begin    { if this is an output arg }
            outputnewstart;        { start NEW if needed }
                tnum := nexttemp;        { assign new temp number }
                        { bind formal }
            settempinfo(tinfo,warg,tnum,outputformaltemp); 
            gentempid(tnum);        { put TEMPnn in NEW list }
            genchar(':');        { for type }
            genspace;
            genjtype(warg);        { type of formal arg }
            end;
        varg := varg^.right;        { get next arg }
        end;
    vref := callee^.blrefs;            { for all implicit args }
    while vref <> nil do begin        { scan ref chain }
                        { if output arg }
        if outputimplicitchk(callee, vref, lastblockp)
        then begin
        outvar := vref^.refvar;        { get name of output var }
        outputnewstart;            { start NEW if needed }
        genname(outvar);        { name of variable }
            setsubstitute(outvar, false, 0, genwithnew);
        end;
        vref := vref^.refnext;        { on to next ref }
        end;
    end;                    { With }
    if anyout then begin            { if NEW generated }
        genchar(')');            { close NEW }
        genspace;                { space before exit }
        end;
end {outputnewlist};
{
    formaltoactual  --  move formal value to actual selector expression

    An ASSIGN operation is generated with the TEMP value associated with
    the formal on the right and the actual selector expression on the left.
    Within the selector expression, but not for the outermost variable
    (the one actually being substituted into) the input variables of the
    procedure are substituted for those in the selector expression.
    This substitution is performed so that VAR arguments containing
    subscript expressions are bound to a specific data item before the
    call and do not change during the call.
}
procedure formaltoactual(var tinfo: temptab;    { TEMP information (read only) }
             formal: varnodep;    { base of formal }
             actual: ptn);        { actual selector expr }
var sl: selstack;                { selector information }
    tid: tempid;                { temp of formal }
    i: 1..temptabmax;                { for temp tab search }
begin
    genstring15('ASSIGN');            { ASSIGN (var) (lhs) ... }
    genspace;
    gendataid(basevariable(actual^.vtype),
    nulltid, genwithoutnew, genwithoutdef); { (var) }
    genspace;
    bindtemps(tinfo, [inputbaseactualtemp, inputformaltemp], 
    false, genwithoutnew);
    buildselstack(actual, sl);            { build sel stack for lhs }
    sl.sesubstitute := false;            { disable first substitution }
    substackselect(sl, 1);            { generate lhs }
    clearallsubstitutes;            { clear subs }
    genspace;                    { end of lhs part }
    for i := 1 to tinfo.tttop do begin        { find binding for formal }
    with tinfo.tttab[i] do begin        { using this entry }
        if tevarnode = formal then        { if find }
        if tekind = outputformaltemp then begin { if output formal }
        assert(tid = 0);        { must be only find }
        tid := tenum;            { get temp number }
        end;                { end output formal }
        end;                { end With }
    end;                    { end search loop }
    assert(tid > 0);                { must have found } 
    gendataid(formal, tid, genwithoutnew, genwithdef);{ (defined! TEMPnn) }
    genspace;
    gendataid(formal, tid, genwithoutnew, genwithoutdef);    { (TEMPnn) }
    if comments then begin            { annotation }
    gencomment(nulllineinfo);        { begin comment }
    genstring15('(Return)');        { actual expr }
    genchar(' ');                { begin expression }
    genmexpr(actual);            { lhs }
    genstring15(' :=');
    genchar(' ');
    genstring15('TEMP');            { TEMPnn }
    geninteger(tid);            { nn }
    end;
    genline;
end {formaltoactual};
{
    routinecall  --  generate jcode for procedure call
}
procedure routinecall(fcallnode: ptn;        { fcall node if function }
        p: ptn);            { call node }
var tinfo: temptab;                { temporary variable info }
    callee: blocknodep;                { block being called }
    anyout: boolean;                { any output arguments }
    parens: longint;                { parens needed in jcode }
{
    outputformals  --  output formal arguments 
}
procedure outputformals;
var q, baseq: varnodep;                { working varnodes }
    argnum: 0..maxarg;                { position in actuals }
begin
    with p^ do begin                { using call node }
    argnum := 0;                { start arg count }
    q := firstformal(vtype);        { get first formal }
    while q <> nil do begin            { for all args }
        argnum := argnum + 1;        { update actual arg pos }
        baseq := basevariable(q);        { get base of formal }
        if q^.vardata.by = byreference then begin { if VAR arg }
        if baseq^.varset then begin    { if this is an output arg }
                        { move formal to actual}
            formaltoactual(tinfo, baseq, arg[argnum]); 
            end;
            end;                { end VAR arg }
        q := q^.right;            { on to next formal }
        end;                { end arg loop }
    end;                    { end With }
end {outputformals};
{
    outputfnresult  --  output function result in EXIT conjunction

    generates the value for the TEMP cell for the result, as an
    uninterpreted function.

    The arguments to the uninterpreted function are the
    TEMP variables from the procedure call actual to formal
    substitution.

    This applies to pure functions and safe functions only.
    Functions with side effects do not get an uninterpreted
    function result.  But functions with global input variables
    are processed here.
}
procedure outputfnresult;
var i: 1..temptabmax;                { for scanning temp tab }
    callee: blocknodep;                { called block }
begin
    assert(fcallnode^.code = fcallop);        { must be fcall node }
    callee := p^.vtype^.blockdata;        { get called routine }
    if callee^.blfnkind in [purefunction,saferoutine]
    then begin                 { if pure or safe function }
        genstring15('(equal!');            { equate result }
        genspace;                { to (TEMPnn) }
    gendataid(nil,fcallnode^.ndfntemp.fttemp,genwithnew, genwithoutdef);
    genspace;
    with p^ do begin            { using subnode }
        assert(code = callop);        { must be call }
        callee := vtype^.blockdata;        { get called block }
        genchar('(');            { uninterpreted fn call }
        genname(vtype);            { name of function }
            for i := 1 to tinfo.tttop do begin    { for all in table }
        with tinfo.tttab[i] do begin    { using given item }
                if tekind in [inputformaltemp, inputglobaltemp]
            then begin            { if input var }
            genspace;        { space between args }   
            gendataid(nil,tenum,genwithoutnew,genwithoutdef);
            end;            { end in set }
            end;                { end with }
        end;                { end loop on temps }
        genchar(')');            { finish uninterpreted fn }
    end;                    { no tagged temp }
    genchar(')');                { close equal! }
    end else begin                { if not pure fn }
        genstring15('(true!)');            { no value here }
    end;
end {outputfnresult};
begin {routinecall};
    tinfo.tttop := 0;                { no temp info yet }
    with p^ do begin                { using call node }
    assert(code = callop);            { must be call }
    callee := vtype^.blockdata;        { block node being called }
    {
        Generate beginning of call
    }
    if comments then begin            { annotate jcode }
        gencomment(linen);            { begin comment }
        genstring15('Call of');        { "Call of <name>" }
        genchar(' ');
        genstring15(vtype^.vardata.itemname); { name of routine }
        genline;                { finish comment line }
        end;
    {
        Insure that evaluation of actuals is legal
    }
    safeactuals(p);                { check that actuals are safe }
    {
        Check for aliasing
    }
    aliaschk(p);                { checks for aliasing }
    {
        Save inputs for later use
    }
    inputsave(p,tinfo);            { gen TEMP assignments }
    {
        Generate REQUIRE statements for module invariants outward
        to the callee
    }
    requireinvariants(p, lastblockp);    { gen for caller->callee }
    {
        Generate REQUIRE statements for initialization of blocks
        being entered.
    }
    requireinit(p, lastblockp);        { gen for dominator->callee }
    {
        Generate REQUIRE statements for entry assertions
    }
                        { bind formals, non-old }
    bindtemps(tinfo, [inputformaltemp], 
        false, genwithoutnew); 
    inputentrygen(p);            { generate the REQUIRES }
    calldepthcheck(lastblockp,p);        { generate DEPTH require }
    clearallsubstitutes;            { undo bindtemps }
    {
        END INPUT PROCESSING  --  BEGIN OUTPUT PROCESSING
    }
    {
        Generate big NEW statement with EXIT assertions as givens
    }
    outputnewlist(p,fcallnode,tinfo,anyout);{ gen output NEW list }
    if anyout then begin            { if procedure has outputs }
                        { set up ".old" objects }
        bindtemps(tinfo, [inputformaltemp, inputglobaltemp], 
        true, genwithoutnew);
                        { out formals are "new!" }
                        { so is funct result }
        bindtemps(tinfo, [outputresulttemp, outputformaltemp], 
        false, genwithnew);
        {
        If this call left the module and returned, then the caller's
        invariants that were true at entry to the unit are now
        true again and must be proclaimed.
        Note that these are the invariants of the callee.
        PROBLEM:  Is this valid for recursive calls out of and back
              into the same module?
        }
        parens := 0;            { clear parens needed }
        if not localcall(lastblockp, callee) then begin{ if not local call }
        genstring15('(and!');        { join terms }
        parens := parens + 1;        { cause matching paren at end }
        genspace;
        genconjunction(lastblockp^.blassertions,[invariantsubcode]);
        genspace;    
        end;                { end non-local call }
        if isfunction(callee^.blvarnode) then begin    { if function call }
        genstring15('(and!');        { join terms }
        parens := parens + 1;        { cause matching paren at end }
        genspace;    
        outputfnresult;            { output function result }
        genspace;    
        end;                { end is function call }
        genconjunction(callee^.blassertions,  { EXIT assertions }
            [exitsubcode, entryexitsubcode]);
                        { caller INVARIANTs }
        while parens > 0 do begin        { balance AND operators }
        genchar(')');            { one right paren }
        parens := parens - 1;        { count down parens }
        end;
        clearallsubstitutes;        { undo bindings }
        if comments then begin        { annotation }
        gencomment(nulllineinfo);    { "-- exit asserts"}
        genstring15('exit asserts');
        end;
        genline;                { finish line }
        end;
    clearallsubstitutes;            { undo bindings }
    {
        Generate assignments for actual args of caller for output
        formal args
    }
    outputformals;                { output formal args }
    {
        Finish generation of call
    }
    if comments then begin            { annotation }
        gencomment(linen);            { line number }
        genstring15('End call of');        { "End call of <name>" }
        genchar(' ');
        genstring15(vtype^.vardata.itemname); { name of routine }
        genline;                { end comment line }
        end;
    end;                    { With }
    if debugg then begin            { if debugging }
        dumptemptab(p, tinfo);        { dump TEMP usage summary }
        end;
end {routinecall};
{
    proccall  --  generate jcode for procedure call
}
procedure proccall(p: ptn);            { procedure call node }
begin
    assert(p^.code = callop);            { must be call }
    routinecall(nil,p);                { use common routine }
end {proccall};
{
    functcall  --  generate jcode for function call
}
procedure functcall(p: ptn);           { function call node }
var callee: blocknodep;                { object being called }
begin
    assert(p^.code = fcallop);            { must be fncall }
    assert(p^.arg[1]^.code = callop);        { with call below }
    callee := p^.arg[1]^.vtype^.blockdata;    { get called routine }
    if callee^.blfnkind = rulefunction then begin { if rule function }
    usererrorstart(p^.linen);        { begin error msg }
    write(output,'Rule function "');
     writestring15(output,callee^.blvarnode^.vardata.itemname);
    write(output,'" may only be called in assertions.');
    usererrorend;
    end;
    routinecall(p,p^.arg[1]);            { use common routine }
end {functcall};
