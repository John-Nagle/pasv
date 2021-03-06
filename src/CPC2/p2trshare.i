procedure WHATtrshare; const WHAT = '@(#)p2trshare.i    2.1'; begin SINK := WHAT; end;    { Version 2.1 of 10/12/82 }
{
    Shared Variable Detection
}
{
    checkprocessref  --  note changing of variable on per-process basis

                Shared variables are detected here.

    When a variable is changed by more than one process, it is
    considered shared.  If a variable is changed from shared
    code, it is also considered shared.  
    Otherwise, the variable is updated with the owning process
    of the variable, which is thus determined strictly by use,
    not by scope.
    Note that we cannot use the full reference lists created during
    transitive closure, since transitive closure can cross process
    boundaries through INIT statements.  We must thus use only
    the direct references for each block, and as a separate operation,
    the owning process for each block must be determined.
    This information is used in WAIT statement processing.
}
procedure checkprocessref(blk: blocknodep);    { block being checked }
var pin: sharedinfo;                { sharing info of blk }
    r: refnodep;                { for ref chaining }
{
    noteprocessref  --  check for one variable
}
procedure noteprocessref(v: varnodep);        { variable being considered }
begin
    assert(isbasevar(v));            { only on base vars }
    with v^ do begin                { using given node }
    if varshared <> isshared then begin    { if not known to be shared }
        if varshared = unknownshared then begin { if no present owner }
        varshared := pin;        { set to current block num }
        end else if varshared <> pin then begin { if not same as previous }
        if debugg then begin        { debugging }
            writeln(dbg,'    Shared between processes #',varshared:1,
            ' and #',pin:1,': ',vardata.itemname);
            end;            { end debugg }
        varshared := isshared;        { so note }
        end;                { end found sharing }
        end;                { end not previously shared }
    end;                    { With }
end {noteprocessref};
begin {checkprocessref}
    pin := blk^.blshared;            { pin is block sharing info }
    if pin = unknownshared then begin        { if unreaachable from main }
    if debugg then begin            { if debugging }
        writeln(dbg,'Unreachable block: ',
        blk^.blvarnode^.vardata.itemname);
        end;
    pin := isshared;            { force worst case }
    end;
    assert(pin <> unknownshared);        { must be determined }
    r := blk^.blrefs;                { get ref chain }
    while r <> nil do begin            { for each ref }
    with r^ do begin            { using this ref }
        if refmention = directmention then    { if direct reference }
        if refkind = setref then        { if setting var }
        noteprocessref(refvar);        { so note }
        end;                { With }
    r := r^.refnext;            { on to next ref }
    end;                    { end ref linking }
end {checkprocessref};
{
    processreftoblock  --  handle and propagate references to blocks

    The job of processreftoblock is to determine the owning process
    of each block.  Where more than one process can reach the block,
    the block is said to be shared.  Monitors are processes; so is the
    main program.
}
procedure processreftoblock(blk: blocknodep;    { block to check }
                callerpin: pinnum);    { pin of caller }
var r: refnodep;                { for ref chain }
begin
    with blk^ do begin                { using this block }
    if  (blshared <> callerpin) and
        (blshared <> isshared) then begin    { if not already processed }    
        if blshared = unknownshared then    { if previously unknown }
        blshared := callerpin        { now caller }
        else begin blshared := isshared;    { otherwise shared }
        if debugg then 
            writeln(dbg,'    Shared block: ',
            blvarnode^.vardata.itemname);
        end;
        r := blrefs;            { begin propagation }
        while r <> nil do begin        { for all refs }
        with r^ do begin        { using this ref node }
            r := refnext;        { chain forward }
                        { if direct call ref }
            if (refmention = directmention) and
            (refkind in [pcallref, fcallref,initref]) and
                        { to non-monitor }
            (refvar^.vardata.form <> monitordata) then begin
            processreftoblock(refvar^.blockdata,blshared);
            end;
            end;            { With }
        end;                { While }
        end;                { not already processed }
    end;                    { With }
end {processreftoblock};
{
    markprocess  --  mark process with process number,
              and start the processreftoblock scan
}
procedure markprocess(blk: blocknodep);
begin
    with blk^ do begin                { appears to work }
    case blvarnode^.vardata.form of        { fan out on block type }
    proceduredata, moduledata, functiondata: begin end; { no action }
    programdata: begin            { the main program }
        assert(blpin = 0);            { by defn, block 0 }
        processreftoblock(blk,0);        { propagate process 0 }
        end;
    monitordata: begin            { a process }
        assert(blpin <> 0);            { must have block number }
        processreftoblock(blk,blpin);    { so use it }
        end;
    end;                    { cases }
    end;                    { With }
end {markprocess};
{
    Shared variable processing
}
procedure sharedvarfind;
begin
    if debugg then writeln(dbg,'Shared Data Summary');
    blockdrive(@markprocess);            { mark blocks with process }
    blockdrive(@checkprocessref);        { note shared variables }
    if debugg then writeln(dbg);
end {sharedvarfind};
