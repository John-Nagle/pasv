procedure WHATvarfile; const WHAT = '@(#)p2varfile.i    2.3'; begin SINK := WHAT; end;    { Version 2.3 of 5/30/83 }
{
        Variable file processing routines
}
{
    diagnoseoverlap  --  print diagnostic when two vars are at same loc

    If the variables are device variables, this is a user error.
    If the variables are non-device variables, this is an
    internal compiler error.
}
procedure diagnoseoverlap(v1,v2: varnodep); { bad varnodes }
begin
    assert(v1 <> nil);                { must exist }
    assert(v2 <> nil);                { must exist }
    printsourceline(output, v1^.vardata.vrsource);    { print source line #1 }
    usererrorstart(v2^.vardata.vrsource);    { line #2 and msg start }
    if v1^.vardata.loc.relocation = deviceaddr then { if device variable }
    write(output,'User error: ')        { grumble }
    else                    { if non-device variable }
    write(output,'INTERNAL ERROR: ');    { trouble }
    write(output,'variables "');
    writestring15(output,v1^.vardata.itemname); 
    write(output,'" and "');
    writestring15(output,v2^.vardata.itemname); 
    write(output,'" overlap in memory.');    { finish message }
    usererrorend;                { finally close out message }
end {diagnoseoverlap};
{
    setmtype  --  get machine type from variable type

    When, for example, an "iadd" operator is applied to
    a variable, the "iadd" operator expects a machine type.
    The variable, though, will be of type "data".  This routine
    finds the smallest machine type that will contain a data type
    with appropriate bounds.  This machine type is returned 
    as the machine type of the variable.
}
procedure setmtype(vtype: varnodep);        { item to be machine-typed }
const firstmtype = b1;
      lastmtype = s16;                { bounds of valued mtypes }
var m, wtype: machinetype;            { working machine types }
begin
    wtype := unp;                { assume bad type }
    assert (vtype <> nil);            { must exist }
    with vtype^.vardata do begin        { using given type }
        m := firstmtype;            { first of enumeration }
    if form in [arraydata, recorddata, pointerdata,
        moduledata, programdata,
        proceduredata, functiondata, monitordata] then begin
        case form of            { non-elementary types }
        arraydata, recorddata: begin    { structured types }
        wtype := data;            { untyped data }
        end;
        monitordata,            { monitor }
        moduledata,                { module }
        programdata,            { program }
        proceduredata,             { procedure }
        functiondata: begin            { function }
        wtype := xxx;            { not data at all }
        end;
        pointerdata: begin            { pointer (VAR arg) }
        wtype := pnt;            { pointer }
        end;
        end;                { of cases }
    end else begin                { if elementary type }
        repeat                { search mttab }
        with mttab[m] do begin        { using this entry }
            if form = mtkind then    { if form matches }
            if mtmax >= maxvalue then    { if high bound can contain }
            if mtmin <= minvalue then    { if low bound can contain }
                wtype := m;        { find }
            end;
        if wtype = unp then        { if no find yet }
            if m < lastmtype then m := succ(m) else begin 
            usererrorstart(vrsource); { message }
            write('INTERNAL ERROR: Cannot find a machine ',
            'representation for "');
            writestring15(output,itemname);
            write('".');
            usererrorend;
                wtype := xxx;        { fix error }
                end;
            until wtype <> unp;        { until find or give up }
        end;                { end elementary type }
    end;                    { end WITH }
    vtype^.varmtype := wtype;        { set type of object }
end {setmtype};    
{
    fixvarnode  --  special modifications for special types of vars
}
procedure fixvarnode(vp: varnodep);    { varnode to be fixed }
const rvsent = '.';            { identifies function return dummy }
      rvname = 'return';        { use better name }
begin
                    { find machine representation }
    setmtype(vp);            { and store into node }
    with vp^ do begin            { using varnode }
    if vardata.itemname[1] = rvsent then begin { if .rv return name }
        vardata.itemname := rvname;    { use more readable name }
        end;
        if vardata.form in        { if block }
        [proceduredata, functiondata, monitordata, moduledata, programdata] 
            then newblock(vp);        { handle new block }
        if vardata.by = byactualvalue then begin { if non-VAR parameter }
        varset := true;        { implicitly set }
        end;
    if vardata.loc.relocation = deviceaddr then 
        varset := true;        { devices are always set }
    if vardata.loc.relocation = valueaddr then
        varset := true;        { VALUE constants are always set }
    end;                { end With }
end {fixvarnode};
{
        insertinvardict  --  insert variable in variable dictionary

        The variable dictionary is a balanced binary tree.
        The insertion algorithm is that of Adel'son-Vel'skii and
        Landis as described by Knuth in Searching and Sorting, section
        6.2.3.
}
procedure insertinvardict(pnode: varnodep); { node to be inserted }
{
        compare  --  function to compare two field definitions

        Any overlap returns findcomp.
}
function compare(p,q: varnodep): compresult;
begin
    if p^.vardata.loc.relocation > q^.vardata.loc.relocation then 
        compare := greatercomp 
    else if p^.vardata.loc.relocation < q^.vardata.loc.relocation then
        compare := lesscomp
    else if p^.vardata.loc.blockn > q^.vardata.loc.blockn then
        compare := greatercomp
    else if p^.vardata.loc.blockn < q^.vardata.loc.blockn then
        compare := lesscomp
    else if p^.vardata.loc.address >= 
        q^.vardata.loc.address + q^.vardata.size then
        compare := greatercomp
    else if p^.vardata.loc.address + p^.vardata.size <=
        q^.vardata.loc.address then
        compare := lesscomp
    else compare := overlapcomp;
end {compare};
procedure insertnonempty;
var t, s, p, q, r: varnodep;        { working pointers }
    comp: compresult;            { result from compare }
    sbalance,alpha: -1..1;        { balance direction }
begin {insertnonempty}
    t := vartree;            { t is always parent of s }
    s := vartree^.greater;        { s is place for possible rebalance }
    assert(s <> nil);            { must be two entries at least }
    p := s;                { p is possible insert point }
    repeat                { search loop }
        comp := compare(pnode, p);    { compare items }
        case comp of            { handle comparison cases }
        lesscomp: q := p^.lesser;    { advance search }
        greatercomp:  q := p^.greater;    { advance search }
        overlapcomp: begin        { variables overlap }
        diagnoseoverlap(p, pnode);    { report problem }
        terminateprogram;        { does not return }
            end;
        end;                { of cases }
        if q = nil then begin        { if null insert point }
            q := pnode;            { we will insert }    
            if comp = lesscomp then p^.lesser := pnode 
                       else p^.greater := pnode;
        end else begin            { if not ready to insert }
            if q^.balance <> 0 then begin { if nonzero balance factor }
            t := p;            { adjust parent of rebalance point }
            s := q;            { adjust rebalance point }
            end;
            p := q;            { update possible insert point }
            end;            { end not ready to insert }
        until q = pnode;        { repeat until insert takes place }    
                    { insertion complete - begin balancing }
    if compare(pnode,s) = lesscomp then { if new one is less than balance pt }
        p := s^.lesser else p := s^.greater; { select lesser ptr, else gtr }
    r := p;                { ? }
    while p <> q do begin        { adjust balance factors }
    assert(p <> nil);        { p must exist }
    assert(p^.balance = 0);        { p must be balanced }
        case compare(pnode,p) of    { handle comparison cases }
        lesscomp: begin
            p^.balance := - 1;         { set balance count }
            p := p^.lesser;        { link to left }
            end;
        greatercomp: begin
            p^.balance := 1;         { set balance count }
            p := p^.greater;        { link to right }
            end;
        overlapcomp: assert(false);    { p=q in this case }
        end;                { of cases }
        end;                { of while loop }    
                        { balance tree }
    if compare(pnode,s) = lesscomp then alpha := -1 else alpha := 1;
    sbalance := s^.balance;        { get balance value of s }
    if sbalance = 0 then begin        { the tree has grown higher }
        s^.balance := alpha;        { use new balance value }
    end else if sbalance = -alpha then begin { tree is more balanced }
        s^.balance := 0;        { use new balance value }
    end else begin            { the tree is out of balance }
    assert (t <> nil);        { balance point must exist }
    assert(r^.balance <> 0);    { must be +1 or -1 }
        if r^.balance = alpha then begin { need single rotation }
            p := r;
            if alpha = 1 then begin    { must fix rlinks }
                s^.greater := r^.lesser;
                r^.lesser  := s;
            end else begin        { must fix llink }
                s^.lesser := r^.greater;
                r^.greater := s;
                end;
            s^.balance := 0;
            r^.balance := 0;
        end else begin        { need double rotation }
            assert(r^.balance = -alpha);
            if alpha = 1 then begin    { using llinks first }
               p := r^.lesser;
                r^.lesser := p^.greater;
                p^.greater := r;
                s^.greater := p^.lesser;
                p^.lesser := s;
            end else begin
                assert (alpha = -1);
                p := r^.greater;
                r^.greater := p^.lesser;;
        p^.lesser := r;
                s^.lesser := p^.greater;
            p^.greater := s;
            end;
            s^.balance := 0;        { clear balance counts }
            r^.balance := 0;        { clear balance counts }
            if p^.balance = alpha then s^.balance := -alpha;
            if p^.balance = -alpha then r^.balance := alpha;
            p^.balance := 0;
            end;                        { of double rotation }
                                        { finish up }        
        if s = t^.greater then t^.greater := p
        else t^.lesser := p;
        end;                            { end tree was out of balance }
end {insertnonempty};
begin {insertinvardict}            { main insert routine }
    assert(vartree <> nil);        { dummy node must exist }
    if vartree^.greater = nil then begin{ if first real node }
    vartree^.greater := pnode;    { link it in }
    assert(vartree^.lesser = nil);    { must really be second }
    end else                { if at least two entries }
    insertnonempty;            { do general case }
end {insertinvardict};
{
        readvars  --  read variables file and build variables tree

    The indented structure of the variable file is replicated
    in the tree.  
    All items with actual addresses (not offsets) are entered
    into the variable dictionary.
}
procedure readvars;
var tailstack: array [vitemdepth] of varnodep;  
    vp: varnodep;            { working node }
    top: 0..itemmax;            { depth in variable structure }
begin
    top := 0;                { nothing yet }
    while not eof(vars) do begin    { for entire var file }
    newvarnode(vp);            { acquire a new var item }
        with vp^ do begin        { using the new item }
        read(vars, vardata);    { read the variable file data }
                    { if non-relative address }
        if not (vardata.loc.relocation in [offsetaddr, pointeraddr])
        then begin            { or if formal parameter }
        assert(vardata.itemdepth <= 2); { must be level 1 or 2 }
        assert((vardata.itemdepth = 1) 
            or (vardata.loc.relocation = paramaddr));
        insertinvardict(vp);    { put in variable dictionary }
        end else begin        { if offset address }
        assert(vardata.itemdepth > 1); { must be offset from something }
        end;
            if vardata.itemdepth = 1 then begin    { if top level variable }
        tailstack[1] := vp;    { this is head at level 1 }
        top := 1;        { new stack top }
            end else begin        { if field of something else }
                    { must not be more than one lower }
        if vardata.itemdepth > top + 1 then 
            verybadvarnode(vp,11); { varfile format error - (depth) }
        if vardata.itemdepth <= top then begin { if sibling }
            assert(tailstack[vardata.itemdepth]^.right = nil);
            tailstack[vardata.itemdepth]^.right := vp;
        end else begin        { if child, link down }
            assert(tailstack[vardata.itemdepth-1]^.down = nil);
            tailstack[vardata.itemdepth-1]^.down := vp;
            end;        { end child }
        top := vardata.itemdepth; { last is always top }
        tailstack[top] := vp;    { last entry at this level }
        vp^.up := tailstack[top-1]; { link to parent }
            end;            { end non-level-one }
        fixvarnode(vp);        { perform any special fixups }
            end;            { end with }
        end;                { end not EOF }
end {readvars};
{
        findfield  --  find a field based on an address

        An exact match with the correct size is required.
        The search descends structures until the correct size
        is found.  If two structures exist at the same address
    and both are of the same size, the deepest is returned.
    This is required to make records with one element work
    correctly.

    When findfield is called with a size of 0, the largest
    field beginning at the indicated offset is returned.
}
function findfield(pnode: varnodep;    { starting field }
               offset: bitaddress;    { relevant address }
               size: sizeinbits)    { size of field sought }
               : varnodep;        { returned field }
var q, foundfield: varnodep;        { working pointers }
    eltsize: sizeinbits;        { size of element }
    eltnum: targetinteger;        { subscript }
begin
    assert(pnode <> nil);        { must be valid starter }
    foundfield := nil;            { assume failure }
    with pnode^ do begin        { using the given node }
        if (offset = 0) and (size = vardata.size) then begin {if exact }
            foundfield := pnode;    { return given item }
        q := down;            { next one down }
        while q <> nil do begin    { case for single-item record }
                    { check for first item below identical }
        if (q^.vardata.loc.address = 0) 
            and (q^.vardata.size = size) then begin
            assert(q^.vardata.form <> pointerdata); { not pointer }
            foundfield := q;
            q := q^.down;    { continue search }
        end else begin        { not identical }
            q := nil;        { force search exit }
            end;
        end;            { end single-item search }
    end else if (offset = 0) and (size = 0) then begin { if want biggest }
        foundfield := pnode;        { return given item }
        end else if offset < 0 then begin { if cannot possibly fit }
            foundfield := nil;        { report failure }
        end else if offset >= vardata.size then begin {field begins after item }
        foundfield := nil;        { this check needed when size=0 }
        end else if (offset + size) > vardata.size then begin 
            foundfield := nil;        { field ends beyond end of item }
        end else case vardata.form of    { request is subpart of pnode }     
            recorddata: begin        { record }
            q := down;        { get first field }
            while q <> nil do begin { for all fields }
                foundfield := findfield(q, { try this subfield }
                    offset - q^.vardata.loc.address,
                    size);    { look in subfield }
                if foundfield <> nil then begin { if find }
                    q := nil;    { force escape from loop }
                end else begin    { if field does not contain goal }
                q := q^.right;    { link to next field }
                end;
                end;        { of while loop }
           end;            { end recorddata }
            arraydata: begin        { array }
            assert(down <> nil);    { valid tree requried }
            eltsize := down^.vardata.size; { size of element }
            eltnum := offset div eltsize;    { which element }
            if eltnum + q^.vardata.minvalue 
              <= q^.vardata.maxvalue then begin { if subscript in range }
                offset := offset mod eltsize; { offset into element }
                        { recursively examine element }
                foundfield := findfield(down, offset, size); 
                end;
            end;            { end arraydata }
            setdata: begin        { set }
            assert(false);        { ***UNIMPLEMENTED*** }
            end;
        monitordata,         { callables }
        moduledata,
        proceduredata, functiondata,
            signaldata: begin { misc types }
            foundfield := nil;    { fails }
            end;
        booleandata,        { Boolean field }
            numericdata: begin        { numeric field }
            foundfield := nil;    { no subparts possible }
            end;
            end;            { end cases }
        end;                { end WITH }
    findfield := foundfield;    { return result }
end {findfield};
{
        findvar  --  find a variable based on an address
}
function findvar(diagptn: ptn;        { relevant icode node for diag }
    key: addressitem): varnodep;    { returns variable node }
var p: varnodep;            { working pointer }
    state: compresult;            { result of comparison }
    quit: boolean;            { when to quit }
    offset: longint;            { offset into variable }
{
    compaddr  --   compare address with address and size
}
function compaddr(paddr, qaddr: addressitem; size: sizeinbits): compresult;
begin
    if paddr.relocation > qaddr.relocation then 
        compaddr := greatercomp 
    else if paddr.relocation < qaddr.relocation then
        compaddr := lesscomp
    else if paddr.blockn > qaddr.blockn then
        compaddr := greatercomp
    else if paddr.blockn < qaddr.blockn then
        compaddr := lesscomp
    else if paddr.address >= qaddr.address + size then
        compaddr := greatercomp
    else if paddr.address < qaddr.address then
        compaddr := lesscomp
    else compaddr := overlapcomp;
end {compaddr};
begin {findvar}
    p := vartree;            { start at beginning of tree }
    quit := false;            { until told to stop }
    repeat                { until find or fail }
                        { compare addresses }
        state := compaddr(key,p^.vardata.loc,p^.vardata.size); 
        case state of            { fan out on state }
        lesscomp: p := p^.lesser;    { key is less than item }
        greatercomp:  p := p^.greater;    { key is greater than item }
        overlapcomp: quit := true;    { quit when find }
        end;
        if p = nil then quit := true;    { quit if no find }
        until quit;            { do until told to quit }
    if p = nil then begin        { if find failed }
    verybadnode(diagptn, 12);    { variable address lookup failed }
    end else begin            { find succeeded }
        offset := key.address - p^.vardata.loc.address; { offset into var }
    if offset <> 0 then begin    { if not at beginning of variable }
        assert(offset > 0);        { must be in variable, not before it }
        p := findfield(p,offset,0); { find the referenced field }
        if p = nil then 
        verybadnode(diagptn,13);{ field address lookup failed }
        end;    
    end;
    findvar := p;            { return item found }
end {findvar};
{
    findsimpletype  --  find underlying simple type if any

    The simple type must be the same size and start 
    at the same address as the given item.
    The purpose of this is to resolve the ambiguity between
    a record or array of one element and the underlying 
    element.  This is a pathological case, but valid Pascal-F.
}
function findsimpletype(vtype: varnodep): varnodep;
var q: varnodep;                { for search }
begin
    assert(vtype <> nil);            { must not pass nil }
    q := vtype;                    { start search }
    while q^.vardata.form in [arraydata, recorddata] do begin
    q := q^.down;                { look for component }
    assert(q <> nil);            { must not be nil }
    if q^.vardata.loc.address <> 0 then 
        verybadvarnode(vtype, 56);        { address lookup fail }
    if q^.vardata.size <> vtype^.vardata.size then 
        verybadvarnode(vtype, 57);        { address/size lookup fail }
    end;
    findsimpletype := q;            { return found item }
end {findsimpletype};
{
    initvars  --  initialize variable info
}
procedure initvars;
begin
                    { create empty var info }
    new(vartree);            { create dummy initial node }
    with vartree^ do begin        { required for balanced tree insert }
    lesser := nil;            { no links }
    greater := nil;            { no links }
    vardata.loc.address := addressmin; { identify illegal data }
    vardata.loc.relocation := absoluteaddr;
    vardata.loc.blockn := 0;    
    end;
                    { create empty block info }
    blockhead := nil;            { no head yet }
    blocktail := nil;            { no tail yet }
    name := 'variables file ';        { for internal error }
    namesize := 14;            { for internal error message }
    readvars;                { read the varfile }
    if debugg then dumpallvars(dbg);    { if debugging, dump }
end {initvars};
