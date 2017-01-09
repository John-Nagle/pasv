procedure WHATmain; const WHAT = '@(#)p2main.i    2.4'; begin SINK := WHAT; end;    { Version 2.4 of 12/10/85 }
{
    The Main Program
}
{
    initglobals  --  initialize global variables

    Called once, at startup.
}
procedure initglobals;
begin
  rewrite(dbg,'p2-debug');        { ***TEMP*** }
  reset(vars,'pasf-vars');        { ***TEMP*** }
  debugg:=true;             { ***TEMP*** }
  comments := true;            { generate comments }
  seriouserror := false;        { no serious errors yet }
  fatalerror := false;            { no fatal errors yet }
  usererrors := 0;            { non-internal errors }
  nulllineinfo.linenumber := 0;        { null line number }
  nulllineinfo.filenumber := 0;        { null file number }
  srcbuf.lineid := nulllineinfo;    { mark as closed }
  lastsourcefile := 0;            { last file index printed }
  lastfilepath.lpfnum := 0;        { last file path in cache }
  lastrdataaddr := maxint;        { force reset of dat file }
  nodesallocated := 0;            { count of allocated nodes for debug }
  clockserial := 1;            { current clock tick }
end {initglobals};
{
    getbody  --  get body given VHEAD node

    The head of the tree looks like

        vhead
         <specifications>
         block
          <initialization>
          <body>

    The use of the BLOCK node will change when exceptions are 
    properly implemented in the compiler.
}
function getbody(p: ptn): ptn;        { returns body or nil }
var
    blknod: ptn;            { BLOCK node }
begin
    assert(p^.code = vheadop);        { must begin with vhead }
    blknod := p^.arg[2];        { get body part }
    if blknod^.code <> blockop then     { if no block arg }
    verybadnode(blknod,167);    { missing BLOCK node }
    if blknod^.nrarg < 2 then        { if too few args }
    verybadnode(blknod,168);    { too few args to BLOCK node }
    getbody := blknod^.arg[2];        { get body part }
end {getbody};
{
    startpass  --  reset for next pass
}
procedure startpass(passid: char);
begin
    if debugg then begin            { if debugging }
    page(dbg);                { new page in debug file }
    writeln(dbg);
    writeln(dbg,'Begin Pass 2',passid);
    writeln(dbg);
    end;
    reset(int,'pasf-icode');            { ***TEMP*** }    
    resetscopes;                { reset scope decode info }
end {startpass};
{
    pass2a  --  variable reference collection

    Makes one pass over the Icode.
    The routine headers are associated with the routine nodes and
    saved for later use.
}
procedure pass2a;
var header, body: ptn;                { header and body of block }
begin
    startpass('a');                { start pass 2a }    
    while not eof(int) do 
    begin                    { read the Icode and build tree}
        buildtree;   
        if debugg then begin                          
        write(dbg, 'Unmodified Tree for ');
        writestring15(dbg, lastblockp^.blvarnode^.vardata.itemname);
        writeln(dbg);
        treeprint(tree,0);
        writeln(dbg); writeln(dbg); writeln(dbg);
        end;
    if tree^.code <> vheadop then       { if no vhead arg }
        verybadnode(tree, 166);        { no vhead arg at top of tree }
    header := tree^.arg[1];            { header (specification) }
    body := getbody(tree);            { body (code) }
    if not seriouserror then begin        { if no disaster yet }
        lastblockp^.blhasbody := body <> nil; { note if has body }
        findrefs(lastblockp, body);        { build SET/USED list }
        disposetree(body);            { body now expendable }
        assert(lastblockp^.blassertions = nil);{ no assertions yet }
        lastblockp^.blassertions := header;    { save assertions }
        augment(lastblockp^.blassertions);     { augment assertions }
        end;
        end;
end {pass2a};
{
    pass2b  --  Jcode generation

    Makes one pass over the Icode.
    In pass 2b, the header is ignored, and the body is used.
}
procedure pass2b;
var header, body: ptn;            { parts of tree }
begin
    startpass('b');            { start pass 2b }    
    genopen;                { open jcode file for writing }
    while not eof(int) do 
    begin                { compile the current program unit}
        buildtree;   
        if not seriouserror then begin     { if tree read OK }
        if tree^.code <> vheadop then { if no vhead arg }
        verybadnode(tree, 166);    { no vhead arg at top of tree }
        validateblockname(firstline,lastblockp); { validate name of block }
        header := tree^.arg[1];    { get header part of code tree }
        body := getbody(tree);    { get body part }
        disposetree(header);    { header unneeded }
        disposenode(tree);        { vhead node unneeded }
        augment(body);        { augment the tree }
            if debugg then begin                          
        dumpblocknode(lastblockp); { block summary }
            write(dbg, 'Augmented Tree for ');
            writestring15(dbg, lastblockp^.blvarnode^.vardata.itemname);
            writeln(dbg);
            treeprint(body,0);
            writeln(dbg); writeln(dbg); writeln(dbg);
            end;
            if not seriouserror then 
        if lastblockp^.blfnkind <> rulefunction then
        junit(lastblockp, body);{ generate the junit }
        disposetree(body);        { get rid of tree }
        end;
        end;
    genclose;                { close jcode file }
end {pass2b};
{
    main procedure
}
procedure mainprocedure;    
begin
  initglobals;                { initialize global variables }
  initdummies;                { initialize dummy nodes }
  inittables;                { initialize tables }
  initvars;                { read variables file }
  pass2a;                { run pass 2a }
  if usererrors = 0 then tranpass;      { run transitive closure }
  if usererrors = 0 then specifications;{ runs specification check }
  if usererrors = 0 then relevantinvars;{ associate relevant invariants }
  if usererrors = 0 then pass2b;    { run pass 2b }
  if usererrors <> 0 then begin        { if any errors }
    write(usererrors:1, ' error');    { print message }
    if usererrors <> 1 then write('s'); { plural }
    writeln('.');
    genabort;                { delete the jcode file }
    terminateprogram;            { quit }
    end;
  assert(not seriouserror);        { must not exit normally with serious }
end {mainprocedure};
{
    main program
}
begin {main program}
    mainprocedure;            { call main procedure }
end.
