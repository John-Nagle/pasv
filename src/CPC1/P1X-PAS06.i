{*******************************************************************
 *			UTILITY  ROUTINES			   *
 *******************************************************************}
function ceil(fa, fb: integer): integer;	{***ALSO SHOULD BE BUILT-IN?}
begin {ceil}
  ceil := (fa + (fb - 1)) div fb * fb
end {ceil};
  function power2 (p : integer) : real;
	  {-------			computes  2**p  }
  begin
    if p = 0 then power2 := 1.0
    else if p = 1 then power2 := 2.0
    else if p = -1 then power2 := 0.5
    else power2 := power2(p div 2) * power2(p - (p div 2))
  end  {power2} ;
{
	newtyp  --  allocate and clear type node
}
procedure newtyp(var typnode: stp; subtype: forms);
begin
    case subtype of				{ allocate node }
    {   Variant initialization in new is not supported in freepascal. }
	scalar:   	begin new(typnode); typnode^.form := scalar; end;
	booleant:   begin new(typnode); typnode^.form := booleant; end;
	chart:   	begin new(typnode); typnode^.form := chart; end;
	integert:   begin new(typnode); typnode^.form := integert; end;
	longintt:   begin new(typnode); typnode^.form := longintt; end;
	fixedt:   	begin new(typnode); typnode^.form := fixedt; end;
	signalt:   	begin new(typnode); typnode^.form := signalt; end;
	pointer:   	begin new(typnode); typnode^.form := pointer; end;
	sett:   	begin new(typnode); typnode^.form := sett; end;
	arrayt:   	begin new(typnode); typnode^.form := arrayt; end;
	recordt:   	begin new(typnode); typnode^.form := recordt; end;
	devicet:   	begin new(typnode); typnode^.form := devicet; end;
	tagfield:   begin new(typnode); typnode^.form := tagfield; end;
	xcptnt:   	begin new(typnode); typnode^.form := xcptnt; end;
	variant:   	begin new(typnode); typnode^.form := variant; end;
	end;
    with typnode^ do begin			{ using new item }
	form := subtype;			{ set variant id }
	typeid := nil;				{ clear constant part }
	case form of				{ clear variants }
	scalar,booleant,chart,integert,longintt:	
			begin maxconst := nil; maxvalue := 0; minvalue := 0;end;
	fixedt:		begin rlow := 0.0; rhigh := 0.0; precsn := 0.0; end;
	signalt:	begin addresspresent := false; trapvec := 0; end;
	pointer:	begin eltype := nil; end;
	sett:		begin settyp := nil; end;
	arrayt:		begin aeltyp := nil; inxtyp := nil; end;
	recordt:	begin fstfld := nil; recvar := nil; end;
	devicet:	begin fstdfld := nil; devvar := nil; 
				addressed := false; devaddr := 0; end;
	tagfield:	begin fstvar := nil; tagfld := nil; tagtyp := nil; end;
	xcptnt:		begin end;
	variant:	begin varval := 0; nxtvar := nil; subvar := nil; end
	end;					{ of cases }
	end;					{ with }
end {newtyp};
procedure talloc( Var TY:stp; fm: forms; sbrng:boolean );
	 {-------	allocate & partly initialize a type record }
  begin
    newtyp(ty,fm);			{ allocate and clear type }
    with ty^ do
      begin	{ initialize fixed fields }
	form := fm;  marked := false;
	typeserial := typeserial + 1;	{global counter for types}
	tserial := typeserial
      end;
  end;	{ T_alloc }



procedure getbounds(fq: stp; var fmin, fmax: integer);
begin {getbounds}
fmin := 0; fmax := 0;	{ until shown otherwise }
if fq <> nil then with fq^ do
  if form in [scalar,booleant,chart,integert,longintt] then begin
    fmin := minvalue; fmax := maxvalue end;
end {getbounds};


{
	putbyte  --  put out byte.
		     A byte is 0..255
}
procedure putbyte(i:integer; var f:fint);
begin
  if i > 255 then  error( 400  {compiler error} )
  else begin if i < 0   then  error( 400  {compiler error} )
      else write(f,i); end; { FreePascal conversion - should write one binary byte. Needs testing. }
end;

{
	putword  --  put out word
		     A word is 0..65535
}
procedure putword(i:integer; var f:fint);
begin
  if i > 65535 then  error( 400  {compiler error} )
  else begin if i < 0   then  error( 400  {compiler error} )
    else begin
  	putbyte(i div 256, f);
  	putbyte(i mod 256, f);
	end;
      end;
end;
{
    expo - get floating point exponent from a real
    
    Not in Free Pascal
    
    Get log in base 2, round down, add 1 because
    mantissas are always 0 < m < 0.5
    ***CHECK THIS***
}
function expo(x: real) : integer;
begin    
    expo := trunc(log2(abs(x)))+1;     { calculate equivalent mantissa }
end {expo};

procedure formatflit( x: valu;  VAR m,s: integer );
	 {-----------		This procedure converts a fixed point
			literal from its internal form (real) to  an
			integer mantissa and a scale factor	      }
  begin
    if x.rval = 0.0 then
	s := -15	{give zero the maximum single length precision }
    else
	s := expo( x.prcsn );

    if (s < -127) or (s > 127) then
  	error( 400  {"Compiler error"} )
    else if abs(x.rval*power2(-s)) > 32768  then
      begin  { do not try to compute magnitude }
	m := 0;		{ use zero for default }
	error( 206 {magnitude of constant too large for precision} );
      end
    else
      m := round( x.rval * power2(-s) );
  end;
{
	verification language utility routines
}
{
	verclasschk  --  diagnose misuse of EXTRA and FREE variables

	See vmodes and the modetab table for the rules.
}
procedure verclasschk(thisclass: verclass;	{ class of identifier }
		      reqkind: refchg);		{ request, change, or both }
var isok: boolean;			{ result from table }
    errno: integer;			{ error number }
    changemsg: boolean;			{ diagnose change not allowed, vs ref }
begin

					{ get allowed state from table }
    case reqkind of			{ handle cases }
    reference: isok := vmodes.variableref[thisclass]; { reference }
    change:    isok := vmodes.variablechg[thisclass]; { change }
    referenceandchange: begin		{ both }
	isok := vmodes.variableref[thisclass] and
		vmodes.variablechg[thisclass]; { check both }
	end;
    end;
    if not isok then begin		{ if error detected }
					{ if ref allowed, cannot be ref problem}
 	changemsg := vmodes.variableref[thisclass]; { prefers ref msg }
	case thisclass of		{ generate error message }
	executablevar:  if changemsg then errno := 1033 else errno := 1030;
	extravar: if changemsg then errno := 1034 else errno := 1031;
	end;
	error(errno);			{ issue proper error message }
	end;				{ end error case }
end {verclasschk};
