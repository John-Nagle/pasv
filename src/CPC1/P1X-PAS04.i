{
	Input token processing, listing generation, and dictionary

	System-dependent routines have been removed from this file.
}
 
function match(var s1: buffer; l1: longint; var s2:buffer; l2 :longint):longint;
    { Lexical compare of two identifiers  .................}
	{ RETURNS: match=0 if s1=s2; match<0 if s1<s2; match> 0 if s1>s2}
	{ RESTRICTION: must call with L1<=alfaleng,L2<=alfaleng  }
var
  i, n: longint;
begin {match}
  n := imin(l1, l2);
  i := 1;
  while (i < n) and (s1[i] = s2[i])  do  i := i + 1;
  if (i = n) and (s1[i] = s2[i]) then
    match := l1 - l2		{prefix equal; match on length}
  else
    match := ord(s1[i]) - ord(s2[i])
end {match};
{
	error  --  cause an error message to be printed

	The file ERMSG-TXT must contain a line explaining
	each error number.  This line will be retrieved
	and printed by the compiler.
}
 
 
procedure error(n: longint);
begin {error}
  if (errinx < 7) and errorfreecharacter  then 
   begin
    errinx := succ(errinx);
    errorfreecharacter := false;
    with errlist[errinx] do 
     begin
      pos := imax(chcnt-1,1); { final character of token just completed or left margin} 
      nmr := n
     end
   end
end {error};
 
function firstclass( setc: setclass ): classes;
  var c: classes;
  begin
    c := types;				{ WAS first(classes) }
    while  not( (c in setc) or (c = xports) )  do { xports WAS last(classes) }
      c := succ(c);
    if not(c in setc) then error( 400 {"compiler error"} );
    firstclass := c
  end;   {firstclass}
{
	english  --  retrieve error message text from ERMSG-TXT file
}

procedure english( n: longint );
	 {-------		prints an error message in english }
  var i: longint; 
  // c: char;
  var s: string;    { new }
  begin
    if option['L'] or not option['N']  then
      begin
	resetmsgfile(errormsg);		{ go to beginning of message file }
	read( errormsg, i );
	while i <> n do
	  begin  readln(errormsg); read( errormsg, i) end; { reading up to desired record }
	if option['L'] then write(lst,i:4,':');
	if not option['N'] then write(output,i:4,':');
	readln(errormsg, s);        { read rest of line as entire error message }
    if option['L'] then write(lst,s);
	if not option['N'] then write(output,s);
	//repeat
	// get(errormsg);
	//  c := errormsg^;
	//  if option['L'] then write(lst,c);
	//  if not option['N'] then write(output,c);
	//  until eoln(errormsg);
	if option['L'] then writeln(lst);
	if not option['N'] then writeln(output);
      end
  end;		{english}
procedure printlist( k: chain; nm: longint {error number} );
	 {---------		procedure to print a list of identifiers
				according to the error message options.
				(used to list undeclared exported idents)}
  const indent = 6; linelimit = 65;
  var i: longint;
      o,t: boolean;
      sep: char;
  begin
    i := 0;  o := option['L'];  t := not option['N'];

    while k <> nil do
      begin
	if k^.next = nil then sep := ' ' else sep := ',';
	if i = 0 then
	  begin		{print indentation spaces}
	    if o then write(lst, '*', ' ':indent);
	    if t then write(output, '*', ' ':indent);
	    i := i + indent
	  end;
	with k^.this^.name^ do
	  begin		{print the next identifier}
	    if o then write(lst, s:ord(l), sep:1);
	    if t then write(output, s:ord(l), sep:1);
	    i := i + ord(l) + 1
	  end;
	if i >= linelimit then
	  begin
	    if o then writeln(lst);
	    if t then writeln(output);
	    i := 0
	  end;
	k := k^.next
      end;		{while k <> NIL }

    if o and (i<>0) then writeln(lst);
    if t and (i<>0) then writeln(output);

	{ now print error message }
    if o then writeln( lst, '****', nm:4 );
    if t then writeln( output, '****', nm:4 );
    english( nm )

  end;	{ printlist }
{
	endofline  --  end of input line processing

	The listing file, consisting of all source lines with error messages,
	and the standard output, consisting of error lines only, are generated
	here.
}
procedure endofline;
  const  margin = 7;	{see also getnextline,  below}
  var k: longint;
begin {endofline}
    symbolsprohibited := false;		{ symbols allowed now }
    if option['L'] then writesourceline(lst,lstfilen); { listing file }
    if (not option['N']) and (errinx > 0) then  { if error and not suppressed }
	writesourceline(output,outputfilen);{ write bad line }
  if errinx > 0 then 			{ if any errors on line }
    begin
      for k := 1 to errinx do
      with errlist[k] do 
	begin
	{shorten margin so pos + 1 (for circumflex) is under character}
	  if option['L'] then 
	    writeln(lst,' ****',nmr:4,' ':margin-1,' ':pos,'^');
          if not option['N'] then
               writeln(output,' ****',nmr:4,' ':margin-1,' ':pos,'^') 
	end ;
      errtot := errtot + errinx;
	{now print the message corresponding to each error}
      for k := 1 to errinx do english( errlist[k].nmr );
      errinx := 0
    end
end {endofline};
{
	insymbol -- get next input symbol

	This is the basic scan routine called from many places in the
	parsing routines.
}
procedure insymbol;
const
  digmax = 9;
  zero = 0.0;  {***TEMP***}
  ten = 10.0;  {***TEMP***}
var
  quitloop: boolean;                    { LOOP statement removal }
  i, j, k, n, scale, radix: longint;
  r, sf, fac: real;
  digits: array[1..digmax] of 0..9;
  terminator: char;
  prevcomchr: char;			{ last char of comment for runaway chk }
  getnuchar, maxstr, found, sign, useful: boolean;
  nondigit: boolean;			{ for hex scanner }
 
{
	nextch -- get next input character

	Characters are obtained from the bline buffer.
	This routine is system-independent.  Getnextline is the
	system-dependent input routine.
}
procedure nextch;
begin {nextch}
    errorfreecharacter := true;		{ suppress mult msgs at one location }
    ch := chr(0);			{ assume EOF character }
    if not atendoffile then begin	{ if EOF not already reached }
	if chcnt < chavail then	{ if not at EOL }
	    begin chcnt := succ(chcnt);	{ advance to next char }
	          ch := bline[chcnt];	{ return current char }
	end else begin			{ if at end of line }
	    endofline; 			{ produce error messages }
	    getnextline;		{ get next line }
	    {  Following is to cause final EOF diags to be on last real line }
	    if not atendoffile then begin { if not at EOF }
                ch := ' ';		{ simulate space between lines }
		chcnt := 0;		{ set to beginning of next line }
	        end;
	    chcnt := 0;			{ but are at beginning of line }
	end;				{ end at EOL }
    end;
end {nextch};
{
	getinclude  --  get name of file to be included

	At entry, the last character read was an 'I' within a
	command comment.

}
procedure getinclude;
var i: 0..pathnamemax;			{ pos in pathname }
    argwork: pathname;			{ string representing filenames }
begin
    if symbolsprohibited then error(1000 {Include illegal on include line});
    for i := 1 to pathnamemax do argwork[i] := ' '; { clear filename area }
    i := 0;				{ count of chars in filename }
    while ch = ' ' do nextch;		{ pass over spaces }
    if ch = ':' then begin		{ expect : before filename }
	nextch;				{ pass over : }
	while ch = ' ' do nextch;	{ pass over spaces }
	while ch in validpathchars do begin { scan filename }
	    if i < pathnamemax then begin	{ if not too long }
		i := succ(i);		{ increment position }
		argwork[i] := ch;	{ put char in arg }
		nextch;			{ on to next char }
	    end else begin		{ if overflow }
		i := 0;			{ discard string }
		ch := ' ';		{ force exit }
	        end;
	    end;			{ end while }
	end;				{ end found : }
    if i > 0 then begin			{ if filename obtained }
        pushfile(argwork);		{ perform include }
					{ include must be last thing on line }
	symbolsprohibited := true;	{ no symbols until next line, please }
    end else begin			{ if not }
	error(396 { illegal character }); { report error }
	end;
end {getinclude};
{
	get options -- get options from Pascal comment fields
}
procedure getoptions;
var lch: char; serror : boolean;
begin {get_options}
  repeat
    nextch;
    if (ch >= 'a') and (ch <= 'z')
      then ch := chr(ord(ch)-32); {convert l.c. option to u.c.}
    if (ch >= 'A') and (ch <= 'Z') then begin
      lch := ch;
     nextch;
      if lch = 'I' then begin
	  getinclude;		{ read name and set up include file }
	  end
      else         option[lch] :=not(ch='-');
      if (ch  = '+') or (ch = '-')  then nextch
      end
  until ch <> ','
end {get_options};
 
 
begin {insymbol}
repeat
  quitloop := false;                            { remove LOOP statement }
  repeat
    while (ch <= ' ') and (ch <> chr(0)) do	{skip over blanks and controls}
      nextch;
  if ch <> '{' then quitloop := true            { remove LOOP statement }
  else begin
        literalcase := true;
    nextch;
    if ch = '$' then getoptions;
    while (ch <> '}') and (ch <> chr(0)) do begin	{ gobble up comment }
      if ch = '{' then 				{ if comment in comment }
	error(1040 {comment start inside comment});
      nextch;
      end;
    literalcase :=false; 
    nextch
  end until quitloop;                           { remove LOOP statement }
  getnuchar := true;
  useful := true;
  case chartab[ch] of
    dig:begin
      sym.sy := intconst; {assume integer until shown otherwise}
      i := 0;
      repeat
	i := i + 1;
	if i <= digmax then digits[i] := ord(ch) - ord('0');
	nextch
      until chartab[ch] <> dig;
      if i > digmax then begin error(203); i := digmax end;
      n := 0; radix := 10;
      if (ch = 'b') or (ch = 'B') then begin
	nextch;
	radix := 8
	end
      else begin
	scale := 0;
	if ch = '.' then begin
	  nextch;
	  if ch = '.' then
	    ch := ':'
	  else 
	   begin
	    sym.sy := fixconst;
	    while chartab[ch] = dig do 
	     begin
	      i := i + 1;
	      if i <= digmax then digits[i] := ord(ch) - ord('0');
	      scale := scale - 1;
	      nextch
	     end;
	    if i > digmax then begin error(203); i := digmax end
	   end
	  end;		{CH = '.' }
	if (ch = 'e') or (ch = 'E') then begin
	  sym.sy := fixconst;
	  nextch;
	  sign := false;
	  if ch = '+' then nextch
	  else if ch = '-' then begin
	    sign := true;
	    nextch
	    end;
	  while chartab[ch] = dig do begin
	    n := n*10 + (ord(ch) - ord('0'));
	    nextch
	    end;
	  if sign then scale := scale - n else scale := scale + n
	  end;
	end;
      if sym.sy = intconst then begin
	for k := 1 to i do
	  begin
	    if digits[k] >= radix then error( 204  {"illegal digit in octal number"});
	    n := n*radix + digits[k]
	  end;;
	val.kind := lit; val.ival := n
	end
      else 
	begin	{ fixconst }
	  r := zero;
	  for k := 1 to i do 
	    r := r*ten + digits[k];
	  sf := 1.0;  fac := 10.0;
	  sign := scale < 0;  scale := abs(scale);
	  while scale > 0 do
	    begin	{compute decimal scale factor}
	      if odd(scale) then  sf := sf*fac;
	      fac := fac*fac;
	      scale := scale div 2
	    end;
	  val.kind := reel;
	  if sign then
	    begin 		{negative scale factor}
	      val.prcsn := 1.0/sf;
	      val.rval := r/sf;
	    end
	  else			{positive scale factor}
	    begin
	      val.prcsn := sf;
	      val.rval := r*sf;
	    end
	end;
      getnuchar := false
      end;
    xhex:
      begin	{ hexidecimal number delimiter (") }
	sym.sy := intconst;	{ integer constant }
	n := 0; i := 0;		{ initialize value, digit count }
	nondigit := false;
	repeat		{ loop until char is not (0-9, a-f) or we have seen too many }
	  nextch;
	  if ('0' <= ch) and (ch <= '9') then
	    n := n*16 + ord(ch)-ord('0')
	  else if ('A' <= ch) and (ch <= 'F') then
	    n := n*16 + ord(ch) - ord('A') + 10
	  else if ('a' <= ch) and (ch <= 'f') then
	    n := n*16 + ord(ch)-ord('a')+10
	  else
	    nondigit := true;	{end of number}
	until nondigit;
	val.kind := lit; val.ival := n;
	getnuchar := false
      end;	{ hexidecimal number case }

    let: begin
      k := 0;
      repeat
	if k < alfaleng then begin
	  {if option['U'] then convert u.c. identifier to l.c.}
	    if (ch >= 'A') and (ch <= 'Z') then ch := chr(ord(ch)+32);
	  k := succ(k); id.s[k] := ch;
	end else begin			{ if oversize identifier }
	  if enforce then		{ if in restrictive mode }
	    if k = alfaleng then	{ only print error once }
	      error(1037 {Identifier too long}); 
	  k := succ(k);			{ may become oversize }
	  end;
	nextch
      until (chartab[ch] <> let) and (chartab[ch] <> dig) and (ch <> '_') ;
      if k > alfaleng then k := alfaleng; { limit to allowed imax }
      id.l := k;
      j := 0;
      repeat
	found := true;
	j := succ(j);
	if keyword[j].id.l = id.l then begin
	  i := 1;
	  while found and (i <= k) do
	    if keyword[j].id.s[i] <> id.s[i] then found := false
	    else i := succ(i)
	  end
	else found := false
      until found or (j >= nrkeywords);
      if found then sym := keyword[j].sym
      else begin    sym.sy := ident; sym.op := noop end ;
      getnuchar := false
      end;
    quo: begin
      terminator := ch; literalcase :=true;
      sym.sy := stringconst;
      k := 0; maxstr := false;
      repeat
	repeat
	  nextch;
	  if not maxstr then
	    if k < strlen then begin
	      stringvar[k] := ch; k := succ(k)
	      end
	    else begin
	      error(205);
	      maxstr := true
	      end
	until (ch = terminator) or (ch = chr(0));
	nextch
      until ch <> terminator;
      if terminator = '"' then begin
	stringvar[k-1] := chr(0); lgth := k
	end
      else lgth := pred(k);
      getnuchar := false; literalcase := false;
      end;
    db0: begin {':' or ':='}
      nextch;
      if ch = '=' then begin sym.sy := becomes;sym.op :=noop end
      else begin
	sym.sy := colon; sym.op := noop;
	getnuchar := false
	end
      end;
    db1: begin {'<' or '<=' or '<>'}
      nextch;
      if ch = '=' then begin sym.sy := relop; sym.op := leop end
      else if ch = '>' then begin sym.sy := relop; sym.op := neop end
      else begin
	sym.sy := relop; sym.op := ltop;
	getnuchar := false
	end
      end;
    db2: begin {'>' or '>='}
      nextch;
      if ch = '=' then begin sym.sy := relop; sym.op := geop end
      else begin
	sym.sy := relop; sym.op := gtop;
	getnuchar := false
	end
      end;
    db3: begin {'..' or '.'; '..' is changed to ':'}
      nextch;
      if ch = '.' then begin sym.sy := colon; sym.op := noop end
      else begin
	sym.sy := period; sym.op := noop;
	getnuchar := false
	end
      end;
    s00,s02,s03,s04,s05,s06,s07,s08,s09,s11,s12:
      sym := chartok[chartab[ch]];
    s01: begin {'('; sym.op :=  check for ( *   * ) comment}
      nextch;
      if ch <> '*' then
      begin
        sym := chartok[s01];
        getnuchar := false
      end
      else begin
        literalcase :=true;
        nextch;
        if ch = '$' then getoptions;
        prevcomchr := ' ';			{ used only to find (* in com }
        repeat
          while (ch <> '*') and (ch <> chr(0)) do begin
	    prevcomchr := ch;			{ save last comment char }
	    nextch;
	    end;
	  if (ch = '*') and (prevcomchr = '(') then begin { if "(*" }
	    error(1040 {comment start inside comment});
	    end;
          nextch
        until (ch = ')') or (ch = chr(0));
        useful := false ; literalcase:=false
      end;
      end;
    oth, ctl: begin
      error(396 {"illicit character"});
      sym.sy := othersy; sym.op :=  noop
      end;
    eos: begin
      sym.sy := eofsy; sym.op := noop;
      getnuchar := false
      end
  end;
  {	If getnuchar is set, the current character has been expressed
	in sym and a new character must be read.  By changing ``ch'' to
	a space, we force a ``nextch'' on the next call to insymbol
	but leave the character pointer correctly positioned for
	error messages.
  }
  if getnuchar then ch := ' ';		{ cause nextch on next insymbol }
 if option['D'] then begin
     write(output,'  sym.sy=',ord(sym.sy):5); numline :=numline+1 ;	{ ***DEBUG***}
     if numline mod 5 = 0 then writeln(output) end;
until useful;
if symbolsprohibited then error(1000 {Include command must end input line});
end {insymbol};
{	
	skip  --  skip symbols until desired symbol reached

	Used for recovering position after a syntax error
}
procedure skip(tosymbol: symbol);
begin {skip}
  while (sym.sy <> tosymbol) and (sym.sy <> eofsy) do insymbol
end {skip};
procedure header;
	 {------	write a header to the listing file  }
  begin
    writeln(lst, '******  ', compilerversion, ' ** compilation of ',
		' on ', cdate:15,
		 '  ******');
  end;
