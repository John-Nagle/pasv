{
	genintbyte  --  put byte in int file

	We cannot use genbyte here, because genbyte is local to the
	procedure block, to allow diversion to the icode diversion
	file for verifier reordering of icode.
}
procedure genintbyte(n: integer); begin putbyte(n,int); end;
{
	Main Program of Pass One
}
begin {pass 1}

  timezero := Now;	{ get cpu time for this compilation}
  nl := chr(10);	{line feed is newline character }
   if not verifier then begin			{ signon for compiler only }
    writeln(output, compilerversion); break(output);
    end;
  prterr := true; literalcase := false;
  symbolsprohibited := false;		{ not in include-follow mode }
  errtot := 0; errinx := 0; linenum := 0; maxpin := 0; dc := 0; pagenum :=1;
  firststmnt := 0;
  idserial := 0;	{ identifier serializer }
  typeserial := 0;	{ type serializer }
  pin := 0;				{ block serial number }
  initglobal;				{ initialize global constants }
  initinput;				{ initialize input file }
  initoutput;				{ initialize output files }
					{ verifier language initialization }
  vmodestacktop := 0;			{ set top of vmode stack }
  vmodestack[vmodestacktop] := codemode;{ start off in code mode }
  vmode := codemode;			{ yes, in code mode }
  vmodes := vmodetab[codemode];		{ with code mode switch settings }
  if verifier then vinitialize;		{ initialize verifier part }
  cdate := DateToStr(Date); ctimer := TimeToStr(Time);
  if option['T'] then  option['L'] := true;  {force consistency}

  if  option['L'] then
    begin  { page(lst); OBSOLETE - no line printer page eject any more }
        header  
   end;
  chcnt := 0; ch := ' ';
  numline :=0; level :=0; top := 0;
  inittables;
  lcp := nil;
{	
	read program header
}
   ch:=' ';  insymbol;
  if sym.sy = programsy then
   begin
    ismain := true;	{ this is the main program }
    insymbol;
    if sym.sy = ident then begin
      newid(proc,nil,nil,lcp);
      insymbol
      end
    else error(2);
    if sym.sy = lparen then
    begin { ignore program parameters }
      skip(rparen);
      insymbol
    end;
    if sym.sy = semicolon then insymbol else error(14)
    end
  else if sym.sy = modulesy then
    begin	{ This is a separately compiled module }
      ismain := false;
      insymbol;
      if sym.sy = ident then
	begin		{ get the module header }
	  newid(modul,nil,nil,lcp);
	  insymbol
	end
      else
	error( 2 {identifier expected} );
      if sym.sy = semicolon then insymbol else error( 14 { ";" expected});
    			{ look for an  export list }
			{ this only does a syntax check for now }
      if sym.sy = exportsy then
        begin
	  insymbol;	{advance past the keyword }
	  mchain := nil;
	  repeat	{loop through identifier list }
	    if sym.sy = ident then
	      begin	{save the identifier}
		newid(xports,nil,nil,idptr);
		idptr^.enclosedident := nil;
		new(mchain1);
		mchain1^.next:=mchain;
		mchain1^.this:=idptr;	{hang ident on the chain}
		mchain:=mchain1;
		insymbol;	{advance past identifier}
	      end
	    else
		error(2 {identifier expected} );
	    if sym.sy = comma then
		begin  endxlist := false; insymbol end
	    else
		endxlist := true
	  until endxlist;
	end	{export list parse}
    end	{ separately compiled module }

  else
    error(  1  { expected "PROGRAM" } );

  if lcp = nil then begin
    id.l := 6; id.s := '.main.         ';
    newid(proc,nil,nil,lcp)
    end;
  with lcp^ do begin
    pkind := decl;
    plev := 0;
    paddr := 0
    end;
  level := 1; top := 1;
  with blockstack[1] do begin		{ block serial number }
	blockpin := pin;		{ needed for varfilegen }
	end;
  with display[1] do begin
    fname := nil;
    scope := blck
    end;

		{ declare implicit globals }
  if ismain then
    begin	{ output program operator to the Rcode file }
      genintbyte(6 {proc - main program});  genintbyte(0 {not extern});
      genintbyte(0 {ptype = program});
      lc := 2;
    end
  else
    begin	{ separately compiled module }
      genintbyte( 4 {MONIT} );  genintbyte( 0 {not extern});
      genintbyte(3 {ptype = module});
      lc := 0	{assumes no local vars}
    end;


  signalcount := 0;		{ count of signals declared in program }

  ac := 0;

  block(lcp,main);

  if sym.sy <> period then error(21);	{ should be at a period }
  while sym.sy <> eofsy do insymbol;	{ run out input file if not at EOF }
  if errinx > 0 then endofline;		{ flush any err detected at EOF }

  if errtot > 0 then 
    writeln(output,'Compilation complete - ',errtot,' errors detected');

  if option['L'] then
    begin	{output number of errors to listing file}
	writeln(lst);
	writeln(lst,'  Compilation complete - ',errtot,' errors detected.');
    end;

  if not verifier then begin		{ message for compiler only }
    elapsedtime := Int64(TimeStampToMSecs(DateTimeToTimeStamp(Now)) - TimeStampToMSecs(DateTimeToTimeStamp(timezero)));
    writeln(output,'   Runtime: ',elapsedtime div 60000:3,':',
	      (elapsedtime div 1000) mod 60:2,'.',
	      elapsedtime mod 1000:3   );
     end;
  if errtot > 0 then terminateprogram;	{ fail if errors }
end {pass 1}.
