procedure WHATtrutil; const WHAT = '@(#)p2trutil.i    2.4'; begin SINK := WHAT; end;    { Version 2.4 of 1/12/83 }
{
    Transitive closure subpass  --  utility routines
}
{
    localvar  --   is variable local to routine?  

    This is a test for DIRECT locality, i.e. a variable is local only
    to the block that defines it.
    Localvar is always false for static variables.
}
function localvar(routine: blocknodep;        { routine }
          variable: varnodep)        { variable }
          : boolean;            { true if local }
var addr: addressitem;                { address of variable }
begin
    localvar := false;                { assume not local }
    with variable^ do begin            { using variable }
                        { if VAR arg, get pointer }
        if vardata.loc.relocation = pointeraddr then begin
        assert(up^.vardata.by = byreference); { must really be VAR arg }
        addr := up^.vardata.loc;        { use address of pointer }
        end else begin                { if not VAR arg }
        addr := vardata.loc;        { use address of variable }
        end;
        end;
    with routine^ do begin            { with everybody }
    assert(addr.relocation <> offsetaddr);    { only on base variables }
    if blpin = addr.blockn then        { if block matches }
        if addr.relocation in [stackaddr, paramaddr] then
        localvar := true;        { this is a local var }
    end;                    { end withs }
end {localvar};
{
    addref  --  note a reference to a single object

    The set/used flags in the varnode are updated.  A ref node is
    created if one does not already exist for the given kind of
    ref and if the reference is not to an object local to the
    scope of the referrer.  Local references are not stored because
    the ref list is intended to indicate the global objects affected
    by calls to a routine.
}
function addref( referrer: blocknodep;        { routine doing reference }
          vobject: varnodep;        { object referred to }
          rkind: varrefkind;        { kind of reference }
          rformal: varnodep;        { relevant formal if varref }
          rmention: mentionkind)    { direct or indirect mention }
          : boolean;            { true if changed referrer }
var r: refnodep;                { working ref pointer }
    nofind: boolean;                { true until find }
begin
    assert(referrer <> nil);             { last block must be parsed }
    addref := false;                { initially, no change }
    case rkind of                 { set set/used flags for var }
    setref: begin                { object has been set }
        vobject^.varset := true;        { so note }
        assert(isbasevar(vobject));        { must be base }
        end;
    useref: begin                { vobject has been used }
        vobject^.varused := true;        { so note }
        assert(isbasevar(vobject));        { must be base }
        end;
    varref: begin                { vobject passed as VAR }
        assert(isbasevar(vobject));        { must be base }
        assert(isbasevar(rformal));        { must be base }
        end;
    initref: begin end;            { init applied to object }
    pcallref, fcallref: begin end;        { calls }
    end;
    r := referrer^.blrefs;            { prepare to search }
    if (rkind in [setref, useref]) and         { if set or use }
    localvar(referrer, vobject) then begin    { and local to referrer }
    end else begin                { if non-local or var or init }
        nofind := true;                { no find so far }
        while (r <> nil) and (nofind) do begin    { search for duplicate }
            with r^ do begin            { using this node }
            if vobject = refvar then        { if correct variable }
            if refkind = rkind then    { and correct ref kind }
                nofind := false;    { found }    
                        { if new direct mention }
            if refmention = transitivemention then
                if rmention = directmention then begin
                refmention := directmention;
                addref := true;    { change occured }
                end;
            end;                { end WITH on node }
        r := r^.refnext;            { link forward }
        end;                { end while }
        if nofind then begin            { if no find }
        addref := true;            { return change made }
            new(r);                { allocate a ref item }
            with r^ do begin            { using new ref item }
            refvar := vobject;        { variable }
            refkind := rkind;        { set kind of ref }
            refformal := rformal;        { set formal if applicable }
        refmention := rmention;        { local or tran close ref }
            refnext := referrer^.blrefs;    { put at head of chain }
            referrer^.blrefs := r;        { new one at head of chain }
            end;
        end;                { end insertion }
    end;                    { end non-local, etc. }
end {addref};
{
    addcallnode  --  add node to caller chain

    Note that the caller chain refers to direct callers only;
    transitive closure is not performed.

    Duplicates are not added 
}
procedure addcallnode(callee: blocknodep;    { called routine }
              caller: blocknodep);    { calling routine }
var q: callnodep;                { working node }
    find: boolean;                { true if duplicate }
begin
    q := callee^.blcallers;            { get head of callers chain }
    find := false;                { assume not a duplicate }
    while q <> nil do begin            { search for duplicate }
    if q^.clblock = caller then begin    { if find }
        find := true;            { so note }
        q := nil;                { stop search }
    end else begin                { if no find }
        q := q^.clnext;            { on to next one }
        end;
    end;                    { of search loop }
    if not find then begin            { if no find, insert }
    newcallnode(q);                { allocate new call node }
    with q^ do begin            { using it }
        clblock := caller;            { put block in entry }
        clnext := callee^.blcallers;    { link at head of chain }
        callee^.blcallers := q;        { new head }
        end;
    end;                    { insertion }
end {addcallnode};
{
    addargnode  --  add node to VAR argument actual-formal chain

    Duplicates are not added 
}
procedure addargnode(formal: varnodep;        { formal arg }
              actual: varnodep);    { actual arg }
var q: argnodep;                { working node }
    find: boolean;                { true if duplicate }
begin
    q := formal^.actuallist;            { get head of actuals chain }
    find := false;                { assume not a duplicate }
    while q <> nil do begin            { search for duplicate }
    if q^.aractual = actual then begin    { if find }
        find := true;            { so note }
        q := nil;                { stop search }
    end else begin                { if no find }
        q := q^.arnext;            { on to next one }
        end;
    end;                    { of search loop }
    if not find then begin            { if no find, insert }
    newargnode(q);                { allocate new call node }
    with q^ do begin            { using it }
        aractual := actual;            { put block in entry }
        arnext := formal^.actuallist;    { link at head of chain }
        formal^.actuallist := q;        { new head }
        end;
    end;                    { insertion }
end {addargnode};
{
    diagnosepriority  --  diagnose caller/callee priority errors

    1.  It is forbidden to call down (to smaller numbers).
    2.  A no-priority routine cannot call anything but another
        no-priority routine.
}
procedure diagnosepriority(caller, callee: blocknodep);
{
    writeblkpriority  --  write priority for diagnostics
}
procedure writeblkpriority(b: blocknodep);    { block name }
var n: priority;            { priority }
begin
    n := b^.blpriority;            { get priority }
    write(output,'"');
    diagblockname(b);            { name of block }
    write(output,'" (priority ');
    if n = nopriority then write(output,'irrelevant')
    else if n = unknownpriority then write(output,'UNKNOWN')
    else write(output,n:1);        { use number }
    write(output,')');            { close parens }
end {writeblkpriority};
begin
    if callee^.blpriority <> nopriority then begin { if priority matters }
        if (caller^.blpriority=nopriority) { check for nopri->pri call }
        or
        (caller^.blpriority > callee^.blpriority) { check for call down }
        then begin                { diagnose }
        usererrorstart(callee^.blvarnode^.vardata.vrsource);
        write(output,'Call of ');
        writeblkpriority(callee);
        write(output,' by ');
        writeblkpriority(caller);
        write(output,' violates priority rules.');
        usererrorend;
        end;
    end;
end {diagnosepriority};
{
    classifyroutine  --  classify function by type

Type            Allowed in    Priority    Has     Global  Side
        Code  Assert  Matters   Body    inputs    Effects

Rule            No    Yes    No    No    No    No
Pure        Yes    Yes    ?    Yes     No    No
Safe        Yes    Yes    Yes    Yes    Yes    No
General        Yes    No    Yes    Yes    Yes    Yes

A side effect is
  1)  Changing a global variable
  2)  Performing a WAIT
  3)  Calling any procedure or function at a different priority
  4)  Calling any procedure or function that does any of the above.

  1, and 2 are detected here.  3 and 4 are handled in propagateroutinekind.
}
procedure classifyroutine(blk: blocknodep);    { block to classify }
var r: refnodep;                { for ref chaining }
    v: varnodep;                { for formal chaining }
    sideeffects, globals: boolean;        { effects of fn }
    basev: varnodep;                { working base of varnode }
begin
    with blk^ do begin                { using given block }
    if blk^.blvarnode^.vardata.form in [proceduredata, functiondata]
        then begin                { if proc or fn }
        if blhasbody then begin        { if function has body }
        sideeffects := bldoeswait or bldoesdevio; { if WAIT or dev use }
        globals := false;        { assume no globals }
                        { look for output VAR arg }
        v := firstformal(blvarnode);    { get first formal arg }
        while v <> nil do begin        { scan formal chain }
            basev := basevariable(v);    { get base variable }
            case v^.vardata.by of    { how passed }
            byactualvalue: begin end;    { don't care }
            byreference: begin        { if by reference }
            if basev^.varset then    { if output var }
                blhasoutputvararg := true; { has output VAR arg }
            end;
            end;            { of cases }
            v := v^.right;        { get next formal }
            end;
                        { now look for side effects }
        r := blrefs;            { start ref chain scan }
        while r <> nil do begin        { scan ref chain }
                        { if set, side effects }
            if r^.refkind = setref then sideeffects := true;
                        { if init, side effects }
            if r^.refkind = initref then sideeffects := true;
                        { if use, side effects }
            if r^.refkind = useref then globals := true;
            r := r^.refnext;        { on to next ref }
            end;            { end ref scan }
                        { classify function }
        if sideeffects then blfnkind := generalroutine
        else if globals then blfnkind := saferoutine
        else blfnkind := purefunction;
        end else begin            { if no body }
        assert(blrefs = nil);        { if no body, cannot ref }
        if blassertions = nil then begin { if no assertions }
            blfnkind := rulefunction;    { is rule function }
            blpriority := nopriority;    { clear priority }
        end else begin            { if assertions }
            blfnkind := purefunction;    { pure, but maybe useless }
            end;
        end;                { end no body }
        end;                { end is function }
    end;                    { With }
end {classifyroutine};
{    
    propagateroutinekind  --  make function kind prophagate to
                   callers

    The kinds of routines are general, safe, pure, and rule.
    Procedures are limited to kinds safe and general.
    Where a function is seen to be called by a function of greater
    kind, the kind of the calling function must be changed to that
    of the callee.

    A rule function should never be changed to any other type
    of function, because rule functions do not call other functions.

    Also, there is a check here to find calls across priority
    boundaries; this marks the callee as a general routine.
}
procedure propagateroutinekind(b: blocknodep);    { block to check }
var cl: callnodep;                { for caller chaining }
    caller: blocknodep;                { calling block }
begin
    with b^ do begin                { using given block }
    cl := blcallers;            { get caller chain }
    while cl <> nil do begin        { for all call nodes }
        caller := cl^.clblock;        { get the caller }
        assert(caller^.blfnkind <> rulefunction); { rule fn calls routine }
        if caller^.blfnkind > blfnkind then begin { if caller > us }
        caller^.blfnkind := blfnkind;    { use our kind }
        propagateroutinekind(caller);    { and propagate it }
        end;                { end caller > us }
                        { check for differing priority}
        diagnosepriority(caller,b);        { check for trouble }
        if blpriority <> nopriority then    { if our priority matters }
        if caller^.blfnkind <> generalroutine then
        if caller^.blpriority <> blpriority then begin
                        { priorities differ }
            caller^.blfnkind := generalroutine; { caller is general }
            propagateroutinekind(caller); { and propagate it }
            end;
        cl := cl^.clnext;            { get next call node }
        end;                { end call node loop }
    end;                    { end With }
end {propagateroutinekind};
