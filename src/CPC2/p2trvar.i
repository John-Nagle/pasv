procedure WHATtrvar; const WHAT = '@(#)p2trvar.i    2.1'; begin SINK := WHAT; end;    { Version 2.1 of 10/12/82 }
{
    Transitive closure subpass  --  variable set/used processing
}
{
    findrefs --  find all references for a given routine 

    Findrefs scans the body of a routine, and is used
    during pass 2a.
}
procedure findrefs(blk: blocknodep;        { blocknode to use }
           body: ptn);            { code to scan }
{
    noteref  --  note a reference to a single object
}
procedure noteref(p: ptn;            { node of reference }
          rkind: varrefkind;        { kind of reference }
          proc: varnodep;        { proc if varref }
          procarg: integer);        { which arg if varref }
var sink: boolean;                { sink for addref return }
    formal: varnodep;                { formal arg found }
    actual: varnodep;                { actual arg }
    basev: varnodep;                { base of referenced object }
begin
    assert(p <> nil);                { must exist }
    with p^ do begin                { using given node }
    if debugg then begin            { debug printout }
        write(dbg,'REF: ',vtype^.vardata.itemname); { name of variable }
        write(dbg,'     File/line ',linen.filenumber:1, 
        '/',linen.linenumber:1,' (',rkind,')'); { what }
        if proc <> nil then begin        { if proc arg }
        write(dbg,' as arg #',procarg:1,' to ',proc^.vardata.itemname);
        end;
        writeln(dbg);
        end;
    basev := basevariable(vtype);        { get base variable }
                    { update outermost direct mention }
    setdominator(basev^.varblock,blk);    
    case rkind of                { fan out on refkind }
    setref:     begin                { set of variable }
            sink := addref(blk, basev, rkind, nil,directmention);
        setdominator(basev^.varmaster,blk);    { update outermost setting blk }
        end;
    initref: begin                { INIT of monitor/module }
            sink := addref(blk, basev, rkind, nil,directmention);
        addcallnode(basev^.blockdata, blk); { add call info }
                        { update outermost call/init }
        setdominator(basev^.blockdata^.bldominator, blk);
        setdominator(basev^.varmaster,blk);    { update outermost setting blk }
        end;
    useref: begin                { use of variable }
                        { add to ref list }
            sink := addref(blk, basev, rkind, nil,directmention);
        end;
    pcallref, fcallref: begin        { routine call }
        sink := addref(blk, basev, rkind, nil, directmention);
        addcallnode(basev^.blockdata, blk); { add call info }
                        { update outermost call/init }
        setdominator(basev^.blockdata^.bldominator, blk);
        end;
    varref: begin                { ref of VAR arg }
                        { add to chain }
                        { get relevant formal }
        formal := basevariable(formalarg(proc, procarg));      
        actual := basev;            { get relevant actual }
        addargnode(formal, actual);     { add arg node }
                        { add ref node }
        sink := addref(blk, actual, varref, formal,directmention); 
        end;
        end;                { end cases }
        end;                    { end WITH }
end {noteref};
{
    findreference  --  descend code tree, noting references
}
procedure findreference(p: ptn;            { subtree being examined }
            rkind: varrefkind;    { storekind, etc }
            proc: varnodep;        { proc if inside VAR arg }
            procarg: integer);    { which arg number }
var i: 0..maxarg;                { for arg loop }
begin
    if p <> nil then begin
    with p^ do begin            { using given node }
        if code = dtempop then fixdtemp(p);    { note WITH expressions }
        if code = rtempop then fixrtemp(p);    { expand WITH references }
        if code in 
            [varblop, paramop, rdataop,    { data objects }
             callop, dvadop, icallop,    { calls }
             stolop, stofop, movemop,     { store operators }
        vinitop,            { test if monitor/module defnd }
             indexop,            { 2-operand selectors }
        sendop, waitop,            { send and wait operators }
             forop]                { for statement }
        then begin            { if special cases }
        case code of            { handle special cases }
        varblop, paramop, rdataop: begin    { data objects }
        noteref(p,rkind,proc,procarg);    { reference to object }
        end;
        callop: begin            { call to procedure/fn }
        if isfunction(vtype) then    { if function }
            noteref(p,fcallref,nil,0)    { so note }
        else                { must be procedure }
            noteref(p,pcallref,nil,0);    { note call-type reference }
        for i := 1 to nrarg do begin    { for all params }
            assert(arg[i] <> nil);    { must exist }
            if arg[i]^.code = referop then begin { if by-reference arg }
                        { process var param }
            findreference(arg[i],varref,p^.vtype,i); 
            end else begin        { if by-value arg }
            findreference(arg[i],useref,nil,0); { general expr }
            end;
            end;            { end arg loop }
        end;
        dvadop: begin            { device reference }
        noteref(p,rkind,proc,procarg);    { reference to object }
        blk^.bldoesdevio := true;    { note block does device I/O }
        end;
        icallop: begin            { monitor/module init }
        noteref(p,initref,nil,0);    { init reference }
        end;
        sendop: begin            { SEND operation }
        blk^.bldoessend := true;    { note block does SEND }
        end;
        waitop: begin            { WAIT operation }
        blk^.bldoeswait := true;    { note block does WAIT }
        end;
        vinitop: begin            { monitor/module defined test }
        noteref(p,useref,nil,0);    { use reference }
        end;
        stolop, stofop, movemop: begin    { store operators }
        assert(nrarg = 2);        { always two args }
        findreference(arg[1],setref,nil,0);{ first arg is object of := }
        findreference(arg[2],useref,nil,0);{ general ref }
        end;
        forop: begin            { FOR statement }
        findreference(arg[1],setref,nil,0);{ index var set and used }
        findreference(arg[1],useref,nil,0);{ index var set and used }
        for i := 2 to nrarg do begin    { for rest of args }
            findreference(arg[i],useref,nil,0); { general way }
            end;
        end;
                        { selector operators }
        indexop: begin            { array index }
        assert(nrarg = 2);        { must have two args }
                        { user caller kind for object }
        findreference(arg[1],rkind,proc,procarg);
        findreference(arg[2],useref,nil,0);{ subscript is just expr }
        end;
        end;                { end cases }    
        end else begin            { non special cases }
            for i := 1 to nrarg do begin    { for all operands }
                        { continue descending }
            findreference(arg[i],rkind,proc,procarg);
            end;
        end;                { end non special cases }
        end;
    end;
end {findreference};
begin {findrefs}
    clearrtemptab;                { clear WITH info }
    if debugg then begin            { debug print }
    write(dbg, 'Set/Used information for ');
    writestring15(dbg, blk^.blvarnode^.vardata.itemname);
    writeln(dbg); writeln(dbg);
    end;   
    findreference(body,useref,nil,0);        { descend ref tree }
    if debugg then writeln(dbg);        { more space }
    if debugg then dumprefs(dbg,blk);         { dump refs this proc }
end {findrefs};
{
    VAR argument transitive closure

    The object of this exercise is to determine for each VAR arg,
    whether the argument is an input argument, an output argument,
    or both.  During pass 2a, all variables were marked as set or
    used as the code was examined.  This includes VAR formal arguments.
    There remains one case requiring special handling.  A VAR argument
    may be passed as a VAR argument to another procedure.  For such
    formal arguments the set/used information may be incomplete.
    We do not consider passing a variable as a VAR argument to be a
    set or a use of the variable; we merely record the fact that it
    was so passed for later resolution.

    For each formal VAR argument there exists a set of corresponding
    actual arguments.  These arguments are represented by vararg items
    linked to the formal argument.  For each formal argument in the
    program, it is necessary to propagate the set/used information
    along the actual-formal chains.
}
{
    markformal  --  mark one formal arg

    Follows the actual-formal links and marks accordingly
}
procedure markformal(v: varnodep;        { formal argument }
             vset: boolean;        { v is to be marked as set }
             vused: boolean);        { v is to be marked as used }
var q: argnodep;                { working arg node }
begin
    with v^ do begin                { using given node }
    if vset then                 { if marking as set }
        if not varset then begin        { if not already set }
        varset := true;            { mark as set }
        examined := false;        { force propagation }
        end;
    if vused then                { if marking as used }
        if not varused then begin        { if not already used }
        varused := true;        { mark as set }
        examined := false;        { force propagation }
        end;
    if not examined then begin        { if examination needed }
        examined := true;            { now examined }    
        q := actuallist;            { start actual list }
        while q <> nil do begin        { for all matching actuals }
        markformal(q^.aractual, varset, varused); { mark actual }
        q := q^.arnext;            { on to next one }
        end;
        end;                { end examination }
    end;                    { end With }
end {markformal};
{
    markallformals  --  mark all formal args in program
}
procedure markallformals;
var b: blocknodep;                { working block }
    v: varnodep;                { working routine var item }
    formal: varnodep;                { working formal arg }
begin
    b := blockhead;                { get first block }
    while b <> nil do begin            { for all blocks }
    v := b^.blvarnode;            { get variable info }
    with v^ do begin            { using varnode }
        if vardata.form in [proceduredata, functiondata] then begin 
        formal := firstformal(v);    { get first arg }
        while formal <> nil do begin    { for all formal args }
            if formal^.vardata.by = byreference then begin { if VAR }
            assert(formal^.vardata.form = pointerdata); { pnt }
                        { propagate }
            markformal(formal^.down, false, false);
            end;            { end is VAR arg }
            formal := formal^.right;    { next formal arg }
            end;            { end formal loop }
        end;                { end is routine }
        end;                { end With }
    b := b^.blnext;                { on to next block }
    end;                    { end block loop }
end {markallformals};
{
    actualsetuse  --  fix references to formal arguments

    The reference list items for actual arguments do not contain set/used
    information but refer to the formal argument, because when the  
    reference information is first collected the set/used information
    for formal arguments is unavailable.
    During this routine the set/used state of actuals is obtained from
    the corresponding formals.
}
procedure actualsetuse;
var ref: refnodep;                { working ref item }
    blk: blocknodep;                { working block }
    sink: boolean;                { unneeded info }
begin
    blk := blockhead;                { get first block }
    while blk <> nil do begin            { for all blocks }
    ref := blk^.blrefs;            { get ref list head }
    while ref <> nil do begin        { for all refs }
        with ref^ do begin            { for this ref }
        if refkind = varref then begin    { if VAR arg }
            with refformal^ do begin    { using formal arg }
            if varset or varused then begin { if any use }
                        { update outermost direct ref }
                setdominator(refvar^.varblock,blk);
                end;
            if varset then begin    { if set ref }
                sink := addref(blk, refvar, setref, nil, 
                directmention); 
                { note that calling block set a variable }
                setdominator(refvar^.varmaster,blk);
                end;
            if varused then        { if use ref }
                sink := addref(blk, refvar, useref, nil,
                directmention); 
            end;
            end;            { end VAR arg }
        end;                { end With on ref }
        ref := ref^.refnext;        { on to next ref }
        end;                { end ref loop }
    blk := blk^.blnext;            { on to next block }
    end;                    { end block loop }
end {actualsetuse};
{
    Reference list construction for routines.

    The reference list for a routine is the list of all the
    objects it references plus the objects referenced by
    the routines it calls, except that an object local to a
    called routine is not a referenced object for its callers.

    Processing is similar to that for VAR argument resolution.
    There is a list of callers for each routine, and the reference
    information for each routine is propagated back to its callers.
}
{
    refunion  --  construct union of reference lists  

    The ref list of the first block is replaced with
    the modified union of both lists.  If this process changes
    the list, true is returned.
    The modified union of the reference lists is defined as
    the union of the references less the local objects
    of the second block.  See addref.
}
function refunion(b1: blocknodep;        { first block }
            b2: blocknodep)        { second block }
            : boolean;            { true if change }
var ref: refnodep;                { working ref node }
    changed: boolean;                { true if changed b1 list }
begin
    changed := false;                { initially, no changes }
    ref := b2^.blrefs;                { head of b2 ref chain }
    while ref <> nil do begin            { for all b2 refs }
    with ref^ do begin            { using ref node }
                        { add new info }
        changed := changed or
            addref(b1, refvar, refkind, refformal, transitivemention); 
        end;
    ref := ref^.refnext;            { on to next ref }
    end;                    { end ref-adding loop }
    refunion := changed;            { return true if any adds }
end {refunion};
{
    markcall  --  mark one formal arg

    Follows the actual-formal links and marks accordingly
}
procedure markcall(blk: blocknodep);        { current procedure }
var caller: callnodep;                { working call list pointer }
    changed: boolean;                { if change found }
begin
    with blk^ do begin                { using given node }
    blexamined := true;            { now examined }    
    caller := blcallers;            { those who call this routine }
    while caller <> nil do begin        { for all callers of blk }
                        { add my list to his list }
        changed := refunion(caller^.clblock, blk);
                        { if this did something }
        if changed or not caller^.clblock^.blexamined then         
            markcall(caller^.clblock);    { do that block }
        caller := caller^.clnext;        { on to next one }
        end;
    end;                    { end With }
end {markcall};
{
    markallcalls  --  propagate ref list for all blocks
}
procedure markallcalls;
var b: blocknodep;                { working block }
begin
    b := blockhead;                { get first block }
    while b <> nil do begin            { for all blocks }
    markcall(b);                { mark this routine }
    b := b^.blnext;                { on to next block }
    end;                    { end block loop }
end {markallcalls};
