procedure WHATtprint; const WHAT = '@(#)p2tprint.i    2.1'; begin SINK := WHAT; end;    { Version 2.1 of 10/12/82 }
{
    treeprint  --  print the code tree for one procedure
}
procedure treeprint(p: ptn; indent: longint);
const tabstop = 12;            { where to end indentation }
var i: longint;                { for indentation }
begin
    for i := 1 to indent do write(dbg,' ');{ indent 1 space per indent }
    if p = nil then begin        { if nil node }
    writeln(dbg,'null');        { so state }
    end else begin             { if not nil }
    with p^ do begin        { using the node }
        write(dbg,optab[code].opname);    { name of operator }
        for i := indent to tabstop do write(dbg,' '); { align numbers }
        write(dbg,linen.linenumber:4,'.');    { source line number }
        write(dbg,size:7,segnr:7, disp:7);
        if scalefactor <> nil then begin    { if fixed point constant }
        write(dbg,'  (');
        write(dbg,'FIXED POINT CONSTANT'); { ***UNIMPLEMENTED***}
        write(dbg,')');
        end;
        if mtype <> xxx then begin    { if result type meaningful }
        write(dbg,' ',mtype:5);{ type of result }
        end else write(dbg,'      '); { align }
        if vtype <> nil then begin    { if variable entry present }
        write(dbg,' ',vtype^.vardata.itemname); { write name }
        write(dbg,' ',vtype^.vardata.form);    { write kind }
        end; 
        writeln(dbg);        { finish this item }
        for i := 1 to nrarg do    { for indicated number of args }
        treeprint(arg[i],indent+1); { print subtree }
        end;            { end WITH }
    end;                { end not nil }
end {treeprint};

