procedure WHATerrors; const WHAT = '@(#)p2errors.i    2.2'; begin SINK := WHAT; end;    { Version 2.2 of 1/13/86 }
{
    User error message generation, and utilities therefor
}
{
    Low-level utility routines
    Routines in this group should not call other routines.
}
{
    max - maximum
}
function max(i,j: longint): longint;
begin
    if i > j then max := i else max := j;    { usual maximum function }
end {max};
{
    min - minimum
}
function min(i,j: longint): longint;
begin
    if i < j then min := i else min := j;    { usual minimum function }
end {min};
{
    extractsigned  --  extract signed value from 16-bit unsigned quantity.

    This routine directly reflects the target machine implementation,
    which is 16-bit twos complement.
}
function extractsigned(i: longint)        { input value }
              : longint;        { output signed value }
const negbias = 65536;            { this - value is representation of neg}
      posmax = 32767;                { max positive value }
begin
    assert(i >= 0);                { input must be positive }
    assert(i <= 65535);                { and unsigned 16-bit }
    if i <= posmax then                { if positive }
    extractsigned := i            { return given value }
    else
    extractsigned := i - negbias;        { return negative int value }
end {extractsigned};
{
    isfunction  --  test if function (vs procedure)
}
function isfunction(proc: varnodep):        { which procedure }
    boolean;                { true if function }
begin
    assert(proc^.vardata.form in [proceduredata, functiondata]);
    isfunction := proc^.vardata.form = functiondata; { if function }
end {isfunction};
{
    end low-level utility routines
}
{
    terminateprogram  --  terminate such that next pass does not run
}
procedure terminateprogram;
begin
    halt;                    { ***TEMP*** }
end {terminateprogram};
{
    dumpfatal  --  fatal error dumping
}
procedure dumpfatal;    forward;            { FORWARD }
{
    errorterminate  --  terminate in error, dumping if requested
}
procedure errorterminate;
begin
    writeln(output,'*** Above error is unrecoverable. ***');
    dumpfatal;
    terminateprogram;            { give up }
end {errorterminate};
{
    internalerror  --  report an internal error

    Not to be used for internalerrors which the user could induce.
}
procedure internalerror(n: longint);
begin {internalerror}
    writeln(output,'Pass 2 internal error ',n:1,' in ',name: namesize); 
    errorterminate;
end {internalerror};
{
    getpathname  --  get pathname given path number

    The returned name is left in the global variable
    "lastfilepath".
}
procedure getpathname(n: longint);        { the pathname }
var fnames: text;                { file of file names }
    i: longint;                    { for line loop }
    ch: char;                    { for copying }
begin
    if n <> lastfilepath.lpfnum then begin    { if not the current one }
    assign(fnames,'pasf-files');     
    reset(fnames);              { open file name file }
    for i := 1 to n-1 do readln(fnames);    { skip n-1 lines }
    lastfilepath.lpsize := 0;        { chars in path name }
        while not (eoln(fnames) or eof(fnames)) do begin { for one line }
        read(fnames,ch);
        if ch <> ' ' then begin        { if non-space }
        if lastfilepath.lpsize < maxfilepath then begin    { if fits }
                        { advance char pointer }
            lastfilepath.lpsize := lastfilepath.lpsize + 1;
                        { put into buffer }
            lastfilepath.lppath[lastfilepath.lpsize] := ch; 
            end;            { end fits }
        end;                { end non-space }
        end;                { end read loop }
    lastfilepath.lpfnum := n;        { update cache key }
    end;                    { end had to read new one }
end {getpathname};
{
    printsourcefile  --  print source file name in diagnostic message
}
procedure printsourcefile(var f: text;        { output file }
              n: longint);        { file number }
var fnames: text;                { file of file names }
    i: longint;                    { for line loop }
    ch: char;                    { for copying }
begin
    if n <> lastsourcefile then begin        { if new source file }
    getpathname(n);                { get into cache }
    for i := 1 to lastfilepath.lpsize do     { for file name }
        write(f,lastfilepath.lppath[i]);    { write it out }
    writeln(f,':');                { finish line }
    lastsourcefile := n;            { remember last file printed }
    end;    
end {printsourcefile};
{
    printsourceline  --  print desired line from source line file

    Standard Pascal Dumb Non-Random-Access Version
}
procedure printsourceline(var f: text;        { output file }
              n: lineinfo);        { desired line }
const
    srcfilename = 'pasf-source';        { ***TEMP*** }
var i,j: longint;
    resets: 0..1;                { number of resets done }
begin
    resets := 0;                { no resets yet }
    if n.linenumber <= 0 then begin        { if no such line }
    writeln('*** Error on unknown source line ***');
    end else begin                { if new line }
    while (srcbuf.lineid.linenumber <> n.linenumber) or
          (srcbuf.lineid.filenumber <> n.filenumber) do begin
        if srcbuf.lineid.linenumber <> 0 then { if source open }
            if eof(src) then srcbuf.lineid.linenumber := 0;{ check for EOF }
        if srcbuf.lineid.linenumber = 0 then begin    { if source not open }
        if resets > 0 then         { if more than one reset }
            internalerror(254);        { cannot find source line }
        assign(src,srcfilename);
        reset(src);                 { rewind source file }
        resets := resets + 1;        { count resets }
        end;
        read(src,srcbuf);            { read next record }
        end;
                        { now at correct line }
        printsourcefile(f,srcbuf.lineid.filenumber);{ print file name involved }
    write(f,srcbuf.lineid.linenumber:4,'.  ');  { line number }
    i := linetextmax;        { find last nonblank }
    while (i>1) and (srcbuf.linetext[i] = ' ') do i := i-1;
    for j := 1 to i do write(f,srcbuf.linetext[j]); { print line }
    writeln(f);                { finish line }
    end;
end {printsource};
{
    usererrorstart  --  start printing of user error message

    Between calling usererrorstart and usererrorend
    the caller prints the error message on the file 'output'.
}
procedure usererrorstart(nline: lineinfo);    { line number }
begin
    printsourceline(output,nline);        { print source line if any }
    write('*** ');                { begin error message }
    usererrors := usererrors + 1;        { increment error counter }
end {usererrorstart};
{
    usererrorend  --  finish printing of error message
}
procedure usererrorend;
begin
    writeln(' ***');                { finish message }
end {usererrorend };
{
    writestring15 --  generate 15-character string

    Multiple and trailing blanks are suppressed.
}
procedure writestring15(var f: text; s: string15);
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
        write(f,' ');            { generate blank }
        blank := false;            { no blank needed }
        end;
        write(f,ch);            { generate desired char }
        end;
    end;
end {writestring15};
{
    diag  --  generate diagnostic

    Used for simple error messages with canned text
}
procedure diag(where: lineinfo;            { line number info }
           what: string60);            { text of diag }
var i,lim: 1..60;                { position in text }
begin
    usererrorstart(where);            { begin msg }
    lim := 60;                    { search for end of string }
    while (lim > 1) and (what[lim] = ' ') do    { scan for space }
    lim := lim - 1;                { from end of string }
    for i := 1 to lim do write(output,what[i]);    { write text of diag }
    usererrorend;                { finish message }
end {diag};
{
    unimplemented  --  report use of unimplemented feature
}
procedure unimplemented(nline: lineinfo);    { line number }
begin
    usererrorstart(nline);            { start error message }
    write(output,'Feature temporarily unimplemented');
    usererrorend;
end {unimplemented};
{
    badnode --  print info about bad icode node

    Not to be used for errors which the user could cause from
    source language.
}
procedure badnode(p: ptn; errno: longint);
var badop: string6;
    i: 1..6;
    nline: lineinfo;                { troubled line }
begin
    nline.linenumber := 0;            { null line number }
    nline.filenumber := 0;            { null file number }
    if p <> nil then nline := p^.linen;        { get line number if any }
    usererrorstart(nline);            { print usual error message }
    write('INTERNAL ERROR #',errno:1);        { numeric error code }
    if p <> nil then begin            { if error identified }
        write(' (Icode operator ''');        { icode operator }
    badop := optab[p^.code].opname;        { name of internal op }
    for i := 1 to 6 do if badop[i] <> ' ' then write(badop[i]);
    write(''')');
    end;
    usererrorend;
    seriouserror := true;            { prevent next subpass }
end {badnode};
{
    badvarnode  --  bad variable node
}
procedure badvarnode(v: varnodep;        { the bad varnode }
             errno: longint);        { error number }
var i: 0..15;                    { for loop }
    nline: lineinfo;                { troubled line }
begin
    nline.linenumber := 0;            { null line number }
    nline.filenumber := 0;            { null file number }
    if v <> nil then nline := v^.vardata.vrsource; { get line number }
    usererrorstart(nline);            { edit error message }
    write('INTERNAL ERROR #',errno:1);        { numeric error code }
    if v <> nil then begin            { if variable exists }
    write(' (Variable "');            { which variable }
    writestring15(output, v^.vardata.itemname);    { name of string }
    write('")');                { finish message }
    end;                    { end variable identified }
    usererrorend;                { finish error message }
    seriouserror := true;            { prevent next subpass }
end {badvarnode};
{
    verybadnode  --  fatal badnode
}
procedure verybadnode(p: ptn; errno: longint);
begin
    badnode(p,errno);            { print message }
    errorterminate;            { terminate and dump }
end {verybadnode};
{
    verybadvarnode  --  bad variable node

    Always terminates program.
}
procedure verybadvarnode(v: varnodep;        { relevant variable }
             errno: longint);    { error number }
begin
    badvarnode(v, errno);            { report error }
    errorterminate;                { consider as fatal }
end {badvarnode};
{
    diagvarname  --  print name of varnode in quotes on diagnostic file
}
procedure diagvarname(v: varnodep);        { varnode to print }
begin
    write(output,'"');
    writestring15(output,v^.vardata.itemname);    { name of variable }
    write(output,'"');
end {diagvarname};
{
    writeblockname  --  write name of block given block node

    This seems to be a commonly required function
}
procedure writeblockname(var f: text;        { output file }
             b: blocknodep);    { desired block }
begin
    writestring15(f, b^.blvarnode^.vardata.itemname); { output name of block }
end {writeblockname};
{
    diagblockname  -- print type and name of block on diagnostic file
}
procedure diagblockname(b: blocknodep);        { block to print }
begin
    with b^ do begin                { using block node }
    with blvarnode^ do begin        { using varnode of block }
        case vardata.form of         { fan out on form }
        monitordata: begin write(output,'monitor '); end;
        moduledata: begin write(output,'module '); end;
        programdata: begin write(output,'program '); end;
        proceduredata: begin write(output,'procedure '); end;
        functiondata: begin write(output,'function '); end;
        end;                { end of cases }
        end;                { end inner With }
    diagvarname(blvarnode);            { name of block }
    end;                    { With }
end {diagblockname};
{
    diagblockkind  --  print kind of block in diagnostic
}
procedure diagblockkind(b: blocknodep);        { block }
begin
    case b^.blfnkind of                { fan out }
    unknownroutine: write(output,'UNKNOWN KIND');
    generalroutine: write(output,'has side effects');
    saferoutine: write(output,'has global input variables');
    purefunction: write(output,'has no side effects or global variables');
    rulefunction: write(output,'is for a Rule Builder definition');
    end;                    { cases }
end {diagblockkind};
