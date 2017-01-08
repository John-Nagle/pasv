{	*************************************************
	*						*
	*						*
	*	ENGINE CONTROL ALGORITHM		*
	*	  PROGRAMMING LANGUAGE			*
	*						*
	*	  VERIFIER - PASS 2  			*
	*						*
	*						*
	*************************************************	}


program passtwo(output);
const 
compilerversion = 'Pascal-F version  xxx  on  4/13/1981';  {last modified by NELSON E.    }
{
	Table sizing constants
}
      maxarg=31; 			{ max args for any icode op }
      maxlexlev = 15;			{ maximum lexical nesting depth }
      maxcalldepth = 25;		{ a calls b calls c ... limit }
      maxselstack = 15;			{ a.b.c[e].f ... limit }
      maxsubstack = 100;		{ max args to routine, incl. globals }
      maxrecordtypes = 250;		{ max record types per junit }
      maxfilepath = 60;			{ max chars in file pathname }
      maxtempid = 10000;		{ max TEMPnn per junit }
      maxlabelid = 10000;		{ max labels per junit }
      maxtimestamp = 25000;		{ max time ticks per verification }
      maxpin = 1000;			{ max junits }
{
	derived sizes
}
      blockdepthmax = maxlexlev;	{ size of block stack }
      scopedepthmax = maxlexlev;	{ size of scope stack }
      oldargsmax = maxsubstack;		{ size of old args table }
      temptabmax = maxsubstack;		{ size of TEMPs usage table }
      subconstmax = maxselstack;	{ size of constant subscript table }
      recindexmax = maxrecordtypes;	{ size of record index table }
{	
	Types of program units
}
mainprogunit = 0;			{ main program }
procunit     = 1;			{ non-exported procedure/function }
exportedprocunit = 2;			{ exported procedure/function }
moduleunit   = 3;			{ module }
monitorunit  = 4;			{ monitor }
unittypemax = 4;			{ max of this type }
{
	Load types
}
internalload = 0;			{ local to this compilation }
externalload = 1;			{ non-local }
loadtypemax = 1;			{ max of this type }

{machine dependent data structure parameters:}
  bytesize = 8;		
  wordsize = 16;
  numlimit = 65536;      		{ 2 ** wordsize of machine.  - for forming complements}
  bitaddressmax = 524287;		{ maximum address in bits }
  bitsperadrunit = 8;			{ bits per addressing unit (byte) }
  rtempmax = 10;			{ max nesting depth for with statements }
{
	Misc. constants
}
    NUL = minchar;			{ ASCII }
    tempstring1 = 'TEMP';		{ base of dummy names }
    tempstring2 = '~~';			{ tail of dummy name }
    nulltid = 0;			{ null temp id }
					{ constants of type priority }
    maxpriority = 7;			{ highest priority value }
    nopriority = -1;			{ if priority irrelevant }
    backgroundpriority = 0;		{ background task }
    unknownpriority = -2;		{ no priority known yet }
					{ constants of type sharedinfo }
    unknownshared = -2;			{ no sharing info yet }
    isshared = -1;			{ definitely shared variable }
{
	Jcode formatting constants
}
    jlinelengthmax = 1000;			{ absolute maximum line length }
    jlinelengthbreak = 64;			{ beyond here break if possible}
    jlineindent = 8;				{ indent for continuations }
    jlinecomment = 40;				{ indent for comments }
