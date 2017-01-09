const
    filefilename = 'pasf-files';        { name of file of files }
    sourcefilename = 'pasf-source';        { name of source file }
#include "P1X-VER00.h"
type
#include "P1X-VER01.h"
{
    print routines for printing source files
}
var
    lastsourcefile: longint;            { last source file printed }
    lastsourceline: longint;            { last source line printed }
    src: file of sourceline;            { file of source statements }

#include "source.h"

procedure WHATsource; const WHAT = '@(#)source.p    1.2'; begin writeln(WHAT); end;    { Version 1.2 of 11/24/81 }
{
    initsourceprint  --  initialize source printer
}
procedure initsourceprint;
begin
    lastsourcefile := 0;            { files not open }
    lastsourceline := 0;            { at line 0 }
end {initsourceprint};
{
    printsourcefile  --  print source file name
}
procedure printsourcefile(var f: text;        { output file }
              n: longint);        { file number }
var fnames: text;                { file of file names }
    i: longint;                    { for line loop }
    ch: char;                    { for copying }
begin
    if n <> lastsourcefile then begin        { if new source file }
        reset(fnames,filefilename);        { open file name file }
        for i := 1 to n-1 do readln(fnames);    { skip n-1 lines }
        while not (eoln(fnames) or eof(fnames)) do begin { for one line }
        read(fnames,ch);
        write(f,ch);            { copy name to output }
        end;
    writeln(f);                { finish line }
    lastsourcefile := n;            { remember last file printed }
    end;    
end {printsourcefile};
{
    printsourceline  --  print desired line from source line file

    Standard Pascal Dumb Non-Random-Access Version
}
procedure printsourceline;
var i,j: longint;
    srcbuf: sourceline;                { source line record }
begin
    if n <= 0 then begin            { if no such line }
    writeln('*** Error on unknown source line ***');
    end else if lastsourceline <> n then begin    { if new line }
                        { see if rewind requried }
    if (lastsourceline = 0) or (lastsourceline > n) then begin
        reset(src,sourcefilename);        { rewind file }
        lastsourceline := 0;        { now open at line 0 }
        end;
    while lastsourceline < n do begin    { read up to desired line }
        read(src,srcbuf);            { read next record }
        lastsourceline := lastsourceline + 1; { keep position counter }
        end;
        printsourcefile(f,srcbuf.lineid.filenumber);{ print file name involved }
    write(f,srcbuf.lineid.linenumber:4,'.  ');  { line number }
    i := linetextmax;        { find last nonblank }
    while (i>1) and (srcbuf.linetext[i] = ' ') do i := i-1;
    for j := 1 to i do write(f,srcbuf.linetext[j]); { print line }
    writeln(f);                { finish line }
    lastsourceline := n;            { avoid double printing }
    end;
end {printsource};
