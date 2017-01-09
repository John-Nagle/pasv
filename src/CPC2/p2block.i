{
    block utility routines
}
procedure WHATblock; const WHAT = '@(#)p2block.i    2.3'; begin SINK := WHAT; end;    { Version 2.3 of 1/12/83 }
{
    findblock  --  get varnode of block given block serial number

    Terminates program if fails.
}
function findblock(pin: integer): blocknodep;    { returns block node }
var addr: addressitem;            { working address item }
    v: varnodep;            { node of block }
begin
    addr.relocation := routineaddr;    { construct address item }
    addr.address := 0;            { address irrelevant }
    addr.blockn := pin;         { block number is block number }
    v := findvar(nil, addr);        { find varnode for block }
    findblock := v^.blockdata;        { return block node }
    assert(v^.blockdata <> nil);    { block node must be present }
end {findblock};
{
    blockend  --  get block info from block end in icode
}
procedure blockend( b: blocknode);        { block info from end stmt }
begin
    with currentblockp^ do begin            { clear block node }
    bldsize := b.bldsize;            { d size from end }
    blpsize := b.blpsize;            { p size from end }
    bllsize := b.bllsize;            { l size from end }
    blrsize := b.blrsize;            { r size from end }
    if blpin <> b.blpin then        { if bad procedure number }
        badvarnode(blvarnode,268);    { icode/varfile block numbers differ }
    end;
end {blockend};
{
    dominator  --  returns common parent block of both args
}
function dominator(source, dest: blocknodep): blocknodep;
var s1, d1: blocknodep;                { working pointers }
    depth: 0..blockdepthmax;            { depth into block tree }
begin
    assert(source <> nil);            { must exist }
    assert(dest <> nil);            { must exist }
                            { find first common parent }
    depth := min(source^.blblockdepth, dest^.blblockdepth);{ find 1st possible }
    s1 := source;                { start source runback }
    while s1^.blblockdepth > depth do        { while too deep }
    s1 := s1^.blouterblock;            { back through outer blocks }
    d1 := dest;                    { start dest runback }
    while d1^.blblockdepth > depth do        { while too deep }
    d1 := d1^.blouterblock;            { back through outer blocks }
    while s1 <> d1 do begin            { find common ancestor }
    assert(s1 <> nil);            { must not be nil }
    assert(d1 <> nil);            { must not be nil }
    assert(s1^.blblockdepth = d1^.blblockdepth);{ must be in sync }
    s1 := s1^.blouterblock;            { back out one block }
    d1 := d1^.blouterblock;            { back out one block }
    end;
    dominator := s1;                { found common ancestor }
end {dominator};
{
    setdominator  --   update dominator based on another reference

    This is used to discover the owning blocks of routines and 
    variables, based solely on their references.
}
procedure setdominator(var bdom: blocknodep;    { dominator to update }
               bref: blocknodep);    { new reference }
begin
    if bdom = nil then begin            { if no dominator yet }
    bdom := bref;                { this is new dominator }
    end else if bdom <> bref then begin        { if changing dominator }
    bdom := dominator(bdom, bref);        { compute new dominator }
    end;
end {setdominator};
{
    block number manipulation

    These routines construct the stack of nested block numbers,
    allowing the conversion of nesting depth (as found in VARBL
    items) into block numbers (as found in the variable file).

    There are two stacks.  The scope stack is pushed only
    for blocks with local storage (procedures and main programs).
    The block stack is pushed for all blocks.
}
{
    resetscopes  --  reset scope info for new pass

    Must be called before reading icode file from beginning
}
procedure resetscopes;
begin
  blockdepth := 0;            { no blocks yet }
  blocksequence := 0; scopedepth := 0;    { reset scoping machinery }
  scopestack[0].nonscopes := 0;        { level 0 modules }
  lastblockp := nil;            { no last block }
  currentblockp := nil;            { no current block }
end {resetscopes};
{
    pushblock  --  enter new block
}
procedure pushblock(n: integer ;
            programunittype: unittype; { type of unit }
            priority: integer;    { priority of program unit }
            datablock: boolean); { true if proc, not monitor/module }
var blk: blocknodep;            { working block node }
begin
                    { push block stack }
    blk := findblock(n);        { find varnode for block }
    assert(blk <> nil);            { must exist } 
    if programunittype <> monitorunit then begin { if priority not given }
    if blockdepth = 0 then begin    { if main program }
        priority := backgroundpriority; { is background }
    end else begin            { if not main }
        assert(currentblockp <> nil);{ current block must exist }
        priority := currentblockp^.blpriority;{get from outer}
        end;
    end;                { end don't know yet }
    blockdepth := blockdepth + 1;    { push block stack }
    assert(priority <> unknownpriority);{ we must know now }
    with blk^ do begin            { using relevant block node }
    blpin := n;            { procedure number }
    blblockdepth := blockdepth;    { block depth }
    blscopedepth := scopedepth;    { scope depth }
        assert((blouterblock = nil) or (blouterblock = currentblockp));
        blouterblock := currentblockp;    { remember parent block }
    if blpriority = unknownpriority then { set priority if unknown }
        blpriority := priority;        { priority (monitors only) }
    blunittype := programunittype;    { type of program unit }
    end;
                    { push scope stack }
    if datablock then begin        { if real block }
        scopedepth := scopedepth + 1;    { increment nesting depth }
        if debugg then
            writeln(dbg,' ':14,'(Block ',n:1,', depth ',scopedepth:1,')');
        if scopedepth > scopedepthmax then begin    { if too big }
        internalerror(21);        { "scopedepthmax" limit exceeded }
        halt;
        end;
    with scopestack[scopedepth] do begin
        scopepin := n;        { record block number }
        nonscopes := 0;        { no dummies at this level yet }    
        end;
    end else begin            { block without variables }
    scopestack[scopedepth].nonscopes := scopestack[scopedepth].nonscopes+1;
    end;
    currentblockp := blk;        { this block is now current }
    lastblockp := nil;            { only meaningful after end of build }
    end {pushblock};
{
    popblock - called upon leaving a block  

    The use of two stacks and a linked list is a historical accident;
    the linked list is sufficient, but came later.

    Note that this is called both during pass 2a and pass 2b, since
    the code tree is built twice.
}
procedure popblock;
begin
                    { pop block stack }
    if blockdepth < 1 then begin    { if stack empty }
    internalerror(22);        { block stack underflow }
    end;    
    lastblockp := currentblockp;    { set last block for further processing}
    currentblockp := currentblockp^.blouterblock; { get next outer block }
    blockdepth := blockdepth - 1;    { pop stack }
                    { pop scope stack }
    if scopestack[scopedepth].nonscopes > 0 then begin    { dummy block }
    scopestack[scopedepth].nonscopes :=
    scopestack[scopedepth].nonscopes -1;
        if debugg then writeln(dbg,'    (End monitor/module)');
    end else begin
        if scopedepth < 1 then begin    { if too small }
        internalerror(22);        { extra END in icode }
        end;
        if debugg then writeln(dbg,'          (End block ',
        scopestack[scopedepth].scopepin:1,')');    { comment listing }
        scopedepth := scopedepth - 1;    { pop stack }
    end;    
end { popblock } ;
{
    depthconv  --  convert depth to procedure number
}
function depthconv(d: integer):integer;
begin
    if d > scopedepth then begin    { if too big }
    internalerror(23);        { scope in address out of range }
        depthconv := -1;
    end else begin
        depthconv := scopestack[d].scopepin;
        end;
end {depthconv};
