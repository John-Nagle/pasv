{
	System-dependent routines for Pascal-F compiler, phase 1

	UNIX version
}
{
	Functions added for compatability
}
{
	imin - minimum of two integers
}
function imin(a,b: integer): integer;		{ Perhaps should be built-in }
begin {imin}
    if a>b then imin := b else imin := a;
end {imin};
{
	imax - maximum of two integers
}
function imax(a,b: integer): integer;		{ Perhaps should be built-in }
begin {imax}
    if a<b then imax := b else imax := a;
end {imax};
{
	rmin - minimum of two reals
}
function rmin(a,b: real): real;		{ Perhaps should be built-in }
begin {rmin}
    if a>b then rmin := b else rmin := a;
end {rmin};
{
	rmax - maximum of two reals
}
function rmax(a,b: real): real;		{ Perhaps should be built-in }
begin {rmax}
    if a<b then rmax := b else rmax := a;
end {rmax};
{
	makeuppercase -- force character into upper case

	Will work for any representation in which characters are dense
	and upper and lower case characters have the same sequence.
}
procedure makeuppercase(var ch: char);	{ char to force }
begin
    if (ch >= 'a') and (ch <= 'z') then { if lower case }
	ch := chr(ord(ch) + (ord('A') - ord('a'))); { convert to upper }
end {makeuppercase};
{
	Simulation of mark-release allocation mechanism

	This really doesn't do anything
}
procedure mark(n: integer); {covers built in procedure of TOPS-20 pascal}
  begin
  end;
 
procedure release(n: integer);
  begin
  end;
{
	break  --  flushes output buffer of textfile
}
procedure break(var f: text);
begin
    flush(f);			{ flush buffering }
end {break};
{
	terminateprogram  --  terminate execution, preventing next pass start
}
procedure terminateprogram;
begin
	vinitialize;		{ destroy all output files to inhibit pass 2 }
	halt;			{ abort the program }
end {terminateprogram};
{
	Invoke pass two of compiler
}
procedure callpass2;
begin
			{ ***TEMP UNIMPLEMENTED*** }
end {callpass2};
{
	Input file processing
}
{
	Input file reading and inclusion processing
}
{
	opentopfile  -- open top file on file stack
}
procedure opentopfile;
begin
    assert(filestackdepth > 0);		{ must be something there }
    with filestack[filestackdepth] do begin { using top of stack }
	assert(state = unopened);	{ must not be opened yet }
	assign(infile,fname);       { associate name with file object}
	reset(infile);		        { open the file }
	state := opened;		{ mark as opened }
	fileserial := fileserial + 1;	{ count files opened }
	filenumber := fileserial;	{ save sn of this file on stack }
	if verifier then writefilfile(fname); { save file name for diags }
	end;
end {opentopfile};
{
	pushfile  --  push a file name on the file stack

	The top file on the stack is the one currently being read from.
}
procedure pushfile(fn: pathname);	{ file name to be pushed }
begin
    if filestackdepth >= filestackmax then begin { if too many }
	write(output,'Too many file names or included files.');
	terminateprogram;
    end else begin			{ new name will be accepted }
	filestackdepth := succ(filestackdepth); { move to next slot }
	with filestack[filestackdepth] do begin	{ using new slot }
	    fname := fn;		{ file name }
	    state := unopened;		{ current state of file }
	    linenumber := 0;		{ at line zero }
	    end;
	end;
end { pushfile };
{
	popfile  --  called at end of input file to get next file, if any

	Popfile uses files from the argument line or from include items,
	as indicated.  Nested includes are supported.
}
procedure popfile;
var argwork: pathname;			{ pathname-size string }
begin
					{ find another file if required }
    if filestackdepth > 0 then begin	{ if file open now }
	filestackdepth := pred(filestackdepth); { done with this one }
	end;
    if filestackdepth = 0 then begin	{ if out of include files }
	while (currentarg < ParamCount) and (filestackdepth < 1) do begin
	    argwork := ParamStr(currentarg); { read next argument }
	    if argwork[1] <> '-' then begin { if not a flag }
		filestackdepth := 1;	{ put on the file stack }
		with filestack[1] do begin { using first entry }
		    state := unopened;	{ not yet opened }
		    fname := argwork;	{ this is the file name }
		    linenumber := 0;	{ we are at line zero }
		    end;		{ of with }
		end;			{ of if }
	    currentarg := succ(currentarg); { on to next arg }
	    end;			{ of while loop }
	end;				{ of else part }
    if filestackdepth = 0 then begin	{ if no more files }
	    atendoffile := true;	{ tell parser }
	end;
end {popfile};
{
	readargs  --  read UNIX control line and record information
}
procedure readargs;
var argwork: pathname;			{ working argument string }
    i: 0..1000;				{ arg count }
    key: char;
begin
    for i := 1 to ParamCount-1 do begin	{ scan for keyletters }
    argwork := ParamStr(i);  { read an argument }
	if argwork[1] = '-' then begin	{ this is a keyletter }
	    key := argwork[2];		{ get keyletter }   
	    makeuppercase(key);		{ force into upper case }
	    if (argwork[3] = ' ') and (key in validkeyletters) then begin
		option[key] := true;	{ set keyletter }
	    end else begin		{ if not }
		write(output,'Bad keyletter argument: ',argwork);
		terminateprogram;	{ considered fatal }
	        end;
	    end;
        end;
end {readargs};
{
	readline  --  read one line from indicated file
}
procedure readline(var inf: text;	{ file to be read }
		   var ateof: boolean);	{ returns true if EOF reached }
const tab = #9;             { tab char }
var chr: char;				{ last char actually read }
begin
    chavail := 0;			{ number of chars read }
    ateof := eof(inf);			{ and sense eof again }
    if not ateof then			{ if more reading to do }
    begin
	while (not eoln(inf)) and (chavail < mbuf) do begin
	    read(inf,chr);		{ read a character }
	    if (chr = tab) then begin	{ if this is a tab }
		chavail := ((chavail div 8)+1)*8; { adv to next tab stop }
	    end else begin		{ if not a tab }
		chavail := succ(chavail);	{ on to next char }
		bline[chavail] := chr;	{ put char in buf }
		end;
	    end;
	    readln(inf);		{ to beginning of next line if any }
	end;
end { readline };
{
	getnextline --  get next line from input

	Reads a line from the appropriate file
}
procedure getnextline;
var      
    readdone: boolean;				{ quit flag }
    ateof: boolean;				{ eof-reached flag }
    i: 1..mbuf;					{ for clearing line }
begin
    for i := 1 to mbuf do bline[i] := ' '; 	{ clear input line }
    if not atendoffile then			{ if not all read }
    repeat					{ get line from some file }	
	with filestack[filestackdepth] do begin	{ using top file }
	    if state <> opened then opentopfile;{ open if required }
	    readline(infile,ateof);		{ read a line }
	    end;
	if ateof then begin			{ if EOF this file }
	    popfile;				{ try another file }
	    readdone := atendoffile;		{ quit if final EOF }
	    end else begin			{ successful read }
		with filestack[filestackdepth] do begin { for given file }
		    linenumber := linenumber + 1; { increment line count }
		    lastlinenumber := linenumber; { associate with src file }
		    lastfilenumber := filenumber; { and which file }
		    linenum := linenumber;	{ line number for listing }
		    end;
		readdone := true;		{ exit, a line has been read }
		end;
	until readdone;				{ until read or final EOF }
    if verifier then				{ if verifier only }
	if not atendoffile then writesrcfile;	{ make source file }
end {getnextline};
{
	initinput  --  called at startup to find and get input file
}
procedure initinput;
begin
    validkeyletters := ['D','F','L','N','S','T','U','X'];
    linenr := '-----';			{ no line numbers in UNIX }
    chavail := 0;			{ no chars available in buffer }
    chcnt := mbuf;			{ force read of first line }
    atendoffile := false;		{ EOF has not been read }
    currentarg := 1;			{ next arg to read on call line }
    fileserial := 0;			{ count of files read }
    filestackdepth := 0;		{ depth of includes if any }  
    lstfilen := 0;			{ last name listed on lst }
    outputfilen := 0;			{ ditto for output }
    readargs;				{ read input arguments }
    popfile;				{ open first file }
end {initinput};
{
	resetmsgfile -- reset file of canned error message
}
procedure resetmsgfile(var f: text);
begin
    assign(f, errortextfile);           { use UNIX file name }
    reset(f);			    { open file }
end {resetmsgfile};
{
	initout --  initialize output files
}
procedure initoutput;
begin
  if option['L'] then begin				{ if Listing option }
      assign(lst, lstfilename);
      rewrite(lst);			{ user listing file }
  end;
  assign(int, intfilename);         { icode file }
  rewrite(int); 
  assign(dat, datfilename);        
  rewrite(dat);			                { rdata object file }
  assign(symfil, symfilename);  
  rewrite(symfil);			            { interpreter symbols file }
end {initoutput};
{
	writesourceline  --  write current source line to specified file
}
procedure writesourceline(var outf: text;		{ use specified file }
			var lastheader: integer);	{ last hdr printed }
const margin = 10;				{ margin before source line }
var i: 1..mbuf;					{ for printing line }
begin
    if linenum <> 0 then begin			{ omit printing of line 0 }
	if lastheader <> lastfilenumber then begin { if new file }
	    {  The purpose of this search is non-obvious.  The problem is
	       that when the current line contains an include command,
	       the top file on the stack is not the last one read.
	       The last file read is always the one whose file serial number
	       appears in the srcitem.
	    }
	    for i := 1 to filestackdepth do	{ search file stack }
		with filestack[i] do		{ for needed file }
		    if filenumber = lastfilenumber then { if find }
			writeln(outf,fname);	{ write header }
	    lastheader := lastfilenumber;	{ prevent duplicate header }
	    end;
        write(outf,linenum:6,'.',' ':margin-1);	{ print line number }
        for i := 1 to chcnt do write(outf,bline[i]);{ print text line }
	writeln(outf);				{ finish line }
	end;
end {writesourceline};
