procedure WHATjgen; const WHAT = '@(#)p2jgen.i    2.2'; begin SINK := WHAT; end;    { Version 2.2 of 12/10/85 }
{
    Jcode generation routines
}
{
    genline - finish generation of a line
}
procedure genline;
begin
    if gencnt > 0 then begin            { if anything on line }
    writeln(jcd);                { finish line }
    gencnt := 0;                { reset position on line }
    end;
end {genline};
{
    genchar - generate an arbitrary character on a line 
}
procedure genchar(ch: char);
begin
    write(jcd,ch);                { output the char }
    gencnt := gencnt + 1;            { advance char count }
    assert(gencnt <= jlinelengthmax);        { absolute maximum size }
end {genchar};
{
    genopen  --  open jcode file for writing
}
procedure genopen;
begin
    gencnt := 0;                { zero char position }
    assign(jcd,'p2jcode');
    rewrite(jcd);               { ***TEMP*** }
end {genopen};
{
    genclose  --  close jcode file
}
procedure genclose;
begin
    genline;                    { flush current line }
end {genclose};
{
    genabort  --  abort jcode file, preventing further processing.
}
procedure genabort;
begin
    genopen;                    { open and clear }
    genclose;                    { close }
    remove('p2jcode');                { delete the file }
end {genabort};
{
    gencontinuation  --  break for a new line permitted now 
}
procedure gencontinuation;
var i: 1..jlineindent;                { for loop }
begin
    genline;                { force out current line }
    for i := 1 to jlineindent do genchar(' '); { indent new line }
end {gencontinuation};
{
    genspace   --  generate a space, perhaps starting a continuation line
}
procedure genspace;
begin
    if gencnt > jlinelengthbreak then        { if line full }
    gencontinuation                { start new line }
    else
    genchar(' ');                { otherwise just space }
end {genspace};
{
    genspaces  --  generate specific number of spaces in message
               Will not start continuation line.
}
procedure genspaces(n: cardinal);
var i: cardinal;
begin
    for i := 1 to n do genchar(' ');        { n spaces }
end {genspaces};
{
    genstring15 --  generate 15-character string

    Multiple and trailing blanks are suppressed.
}
procedure genstring15(s: string15);
var ch: char;                    { char read }
    i: 1..15;                    { for loop }
    blank: boolean;                { if last was blank }
begin
    blank := false;                { no blank found yet }
    for i := 1 to 15 do begin            { for entire string }
    ch := s[i];                { get next char }
    if (ch = ' ') or (ch = NUL) then begin    { if nonuseful }
        blank := true;            { note blank found }
    end else begin                { if useful }
        if blank then begin            { if blank needed }
        genchar(' ');            { generate blank }
        blank := false;            { no blank needed }
        end;
        genchar(ch);            { generate desired char }
        end;
    end;
end {genstring15};
{
    geninteger -- generate signed decimal longint

    Note that this will not work for numbers < -maxint
}
procedure geninteger(n: longint);
const maxdigits = 12;                { largest possible longint }
var s: array [1..maxdigits] of char;        { string for reverse edit }
    i: 0..maxdigits;                { position in s }  
    j: 1..maxdigits;                { for loop }
begin
    if n < 0 then begin                { if < 0 }
    genchar('-');                { emit sign }
    n := -n;                { positive value }
    end;
    i := 0;                    { position in work }
    repeat                    { editing loop }
    i := i + 1;                { advance pos in work }
    s[i] := chr(ord('0') + (n mod 10));    { next low order digit }
    n := n div 10;                { remainder of number }
    until n = 0;                { stop when done }
    for j := i downto 1 do genchar(s[j]);    { put out string }
end {geninteger};
{
    genfileid  --  generate name of source file
}
procedure genfileid(n: cardinal);        { given file number }
var i: 0..maxfilepath;                { index into name }
begin
    getpathname(n);                { get name into cache }
    for i := 1 to lastfilepath.lpsize do     { for all chars of name }
    genchar(lastfilepath.lppath[i]);    { generate }
end {genfileid};
{
    genlineid  --  generate line id in Jcode.

    Generates

        <filename>:<linenumber>

    enclosed within curly braces.
    For line number 0, no output occurs.
}
procedure genlineid(line: lineinfo);
begin
    if line.linenumber > 0 then begin        { if line number given }
        genchar('{');                { begin line number }
    genfileid(line.filenumber);        { file name }
    genchar(':');                { filename:linenumber }
    geninteger(line.linenumber);        { line number }
        genchar('}');                { end line number }
        genchar(' ');                { space before message }
    end;
end {genlineid};
{
    gencomment  --  generate comment start
    
    Note that when generating a comment, the gen routines which
    may cause continuations must not be invoked.
}
procedure gencomment(n: lineinfo);        { line number or 0 }
begin
    if gencnt > 0 then genspace;        { allow new line start }
    if gencnt > 0 then                 { if not at new line start }
    while gencnt < jlinecomment do genchar(' '); { indent }
    genchar('-'); genchar('-'); genchar(' ');    { begin comment }
    genlineid(n);                { source line ident if any }
end {gencomment};
{
    genmsgstart  --  generate start of message
}
procedure genmsgstart(n: lineinfo);        { relevant line number or 0 }
begin
    genspace;
    genchar('('); genchar('/');            { open message }
    genchar(' ');
    genlineid(n);                { generate line number }
end {genmsgstart};
{
    genmsgend  --  generates end of message
}
procedure genmsgend;
begin
    genchar(' ');
    genchar('/');  genchar(')');        { close message }
end {genmsgend};
{
    genname  --  generate unique name from vnode
}
procedure genname(v: varnodep);
var decorate: boolean;                { unique name gen flag }
begin
    decorate := true;                { assume decoration }
    with v^ do begin                { using given node }
    genstring15(vardata.itemname);        { generate name }
    if vardata.form = functiondata then    { if function }
        if blockdata^.blfnkind = rulefunction then    { if rule function }
        decorate := false;        { do not decorate }
    if decorate then begin            { if to be decorated }
       genchar('~');            { delimits user and gen part }
       geninteger(idunique);        { unique name part }
       end;
    if idunique <= 0 then             { if id is zero }
        internalerror(167);            { name not declared }
    assert(vardata.form <> pointerdata);    { must not be pointer }
    end;
end {genname};    
{
    genintconst  --  generate longint constant
}
procedure genintconst(n: longint);
begin
    genstring15('(consti!');            { (consti! n ) }
    genspace;
    geninteger(n);
    genchar(')');
end {genintconst};
{
    genblockid  --  generate name of block

    The name is built up from the names of the outer blocks.

    Typical output:  "outerproc~innerproc".

    The main program and blocks at the outermost level in the program
    are not decorated; thus, it is an error for the name of
    a block at the outermost level within the program to
    duplicate the program name.
}
procedure genblockid(blk: blocknodep);        { block node }
procedure genblockid1(b: blocknodep);        { block node }
begin
    assert(b <> nil);                { must exist }
    with b^ do begin                { using given block node }
    if blouterblock <> nil then begin    { if contained in outer block }
        if blouterblock^.blouterblock <> nil then begin { and yet another }
        genblockid(blouterblock);    { gen outer block }
            genchar('~');            { separate block names }
        end;                { end 3rd level name }
        end;
    genstring15(blvarnode^.vardata.itemname); { name of this block }
    end;                    { end With }
end {genblockid1};
begin
    assert(blk <> nil);                { must exist }
    with blk^ do begin                { using given node }
    if blouterblock = nil then begin    { if this is main program }
        genstring15(blvarnode^.vardata.itemname); { gen its name }
    end else begin                { if not }
        genblockid1(blk);            { gen name string }
        end;
    end;
end {genblockid};
{
    gentempid  --  generate temp item
}
procedure gentempid(tid: tempid);        { given temp id }
begin
    assert(tid > 0);                { must be valid temp number }
    genstring15(tempstring1);            { "TEMP" }
    geninteger(tid);                { nn }
    genstring15(tempstring2);            { "~" }
end {gentempid};
{
    gendataid  --  generate reference to entire variable

    Temp id, new!, and defined!, are exactly as specified by caller.
}
procedure gendataid(v: varnodep;        { data object }
           tid: tempid;            { gen TEMP$n if nonzero }
           newmode: gennewmode;        { if expr is inside NEW stmt }
           definedmode: gendefmode);    { if expr is DEFINED test only }
begin
    genchar('(');                { parens around var }
    if definedmode = genwithdef then begin    { if DEFINED }
        genstring15('defined!');        { (defined! <var>) }
        genspace;
        end;            
    if newmode = genwithnew then begin        { if NEW }
        genstring15('new!');            { (new! <var>) }
        genspace;
        end;                    { all 4 possibilities are legal}
    if tid = 0 then begin            { if not substituting }
    assert(v <> nil);            { must exist }
        genname(v);                { emit name of variable }
        assert(v = basevariable(v));        { must be base variable }
    end else begin                { if substituting }
    gentempid(tid);                { TEMP$nn }
    end;
    genchar(')');
end {gendataid};
{
    gentypeid   --  generate type of variable 
}
procedure gentypeid(v: varnodep);        { variable node }
const anontype = 'TYPE';            { anonomous type prefix }
var tid: recindex;                { working record index }
begin
    assert(v <> nil);                { must exist }
    with v^ do begin                { using given node }
    if vardata.recordname[1] <> ' ' then begin { if user name given }
        genstring15(vardata.recordname);    { generate name of type }
    end else begin                { if not named by user }
        genstring15(anontype);        { use canned name }
        end;
    genchar('~');                { separator }
    tid := recindexsearch(v);        { get type index }
    assert(tid > 0);            { type not declared }
    geninteger(tid);            { generate type index }
    end;
end {gentypeid};
{
    genfieldid  --  generate id of record field
}
procedure genfieldid(v: varnodep);        { relevant varnode }
begin
    with v^ do begin                { for node }
    assert(up <> nil);            { must be subpart }
    assert(up^.vardata.form = recorddata);    { of record }
    genstring15(vardata.itemname);        { field name }
    end;
end {genfieldid};
{
    gennewstart  --  start NEW statement if not already started
}
procedure gennewstart(var nonewyet: boolean);    { true if NEW not yet gen }
begin
    if nonewyet then begin            { if NEW needed }
    genstring15('NEW (');            { begin NEW statement }
    nonewyet := false;            { there is a NEW out }
    end else begin                { if NEW already }
    genspace;                { space between args }
    end;
end {gennewstart};
{
    Utilities for constructing lists in Jcode expressions

    These are used for constructing strings of the form

        (and! a (and! b (and! c d)))
}
{
    liststart  --  start list
}
procedure liststart(var ll: listitem;        { relevant list }
            opstring: string15);    { joining operator }
begin
    ll.lldepth := 0;                { no depth yet }
    ll.llop := opstring;            { save operator }
end {liststart};
{
    listmore  -- preface next item on list

    This routine is NOT called for the last item on the list.
}
procedure listmore(var ll: listitem);        { relevant list }
begin
    genchar(' ');                { begin new subexpr }
    genchar('(');                { begin new subexpr }
    genstring15(ll.llop);            { generate the operator }
    genspace;
    ll.lldepth := ll.lldepth + 1;        { for paren balance }
end {listmore};
{
    listend  --  finish list
}
procedure listend(var ll: listitem);        { relevant listitem }
begin
    while ll.lldepth > 0 do begin        { balance parens }
    genchar(')');                { gen paren }
    ll.lldepth := ll.lldepth - 1;        { decrement }
    end;
end {listend};
