procedure WHATdummies; const WHAT = '@(#)p2dummies.i    2.1'; begin SINK := WHAT; end;    { Version 2.1 of 10/12/82 }
{
    Dummy Code to make Berkeley Pascal Compiler happy
}
procedure initdummy;                { totally useless procedure }
{
    dummy1 -- never-called procedure for unneeded code 
}
procedure dummy1;                { never called }
begin
{
    Code to call procedures which exist solely to provide a place
    to put WHAT strings.
}
    WHATalias;
    WHATblock;
    WHATbound;
    WHATbuild;
    WHATcall;
    WHATdump;
    WHATexpr;
    WHATjgen;
    WHATjunit;
    WHATjvars;
    WHATmain;
    WHATselect;
    WHATsetused;
    WHATstmt;
    WHATtables;
    WHATtprint;
    WHATutil;
    WHATvarfile;
{
    Code to reference objects read or written with file I/O, for
    which compiler generates spurious "field not referenced" messages.
}
end {dummy1};
begin {initdummy};
    zzz := 1;
    if zzz = 0 then dummy1;            { never executed but generated }
end {initdummy};
