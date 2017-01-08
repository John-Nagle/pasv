				{returned by insymbol}
				{********************}
var
  sym: symtype;			{symbol type and classification}
  val: valu;			{value of constant}
  lgth: integer;		{length of string}
  string: packed array[0..strlen1] of char;	{value of string}
  id: idtype;			{last identifier}
  ch: char;			{last character}
  atendoffile: boolean;		{ true after EOF detected in source }
  lstfilen: integer;		{ serial of last file name printed }
  outputfilen: integer;		{ ditto for terminal output }
  validpathchars: set of char;	{ chars allowed in include file names }
  lastlinenumber: integer;	{ within-file number of last line read }
  lastfilenumber: integer;	{ file number of last line read }
  chavail: 0..mbuf;		{ characters available on current input line }
  symbolsprohibited: boolean;	{ true after include detected until next EOL }
  chcnt: 0..mbuf;			{character counter} {ecn}
  linenum,pagenum: integer;		{line counter}
  linenr : pack5;
  chartab:chartabtype;
  bline :  array[1..mbuf] of char; {line buffer} { MUST BEGIN AT 0 W/NEW PAS05 }
  ctimer,cdate:alfa;
  
                               {option switches:}
                               {****************}
  option:options;
 
                               {files}
                               {*****}
  src,xsrc,                         {source}
  dbg,					{ compiler debug dump}
  lst: text;                         {listing}
   symfil: text;		{symbols table for interpreter/debugger}
   fpar : text;			{parameters for pass2}
  int,                         {intermediate code}
  dat: fint;                   {intermediate data}
  errormsg: text;		{ text of error messages }
 
                               {error messages:}
                               {***************}
  errtot: integer;             {total number of errors}
  errinx: 0..7;                {number of errors in current line}
  errlist: array [1..7] of record pos: 1..mbuf;  nmr: 1..1500 end;
  errorfreecharacter: boolean;	{a switch used to suppress
					 multiple error messages at a single
					 source position.  It is set
					 by nextch and negated by
					 error.  }
 
 
 
				{type and identifier information}
				{*******************************}
 
				{pointers to builtin types:}
				{**************************}
  boolptr, charptr, intptr, realptr, xcptnptr, signalptr, textptr, nilptr: stp;
  typeserial: integer;		{a global serial number for types}
  notypeptr: stp;			{ undefined type pointer }
 
 
 
 
				{dummy identifiers for undeclareds:}
				{**********************************}
  udptrs: array[classes] of itp;
  idserial: integer;		{a global serial number for identifiers}
 
 
 
				{lex level display:}
				{******************}
				  { the display contains one entry
				    for each level of scope:  fname
				    points to the root of the iden-
				    tifier tree for that level.     }
  display: array[disprange] of record
    fname: itp;		{root of identifier tree}
    case scope: scopetype of
    modyul: (
      exportlist, importlist: chain;
      exclusion: mkinds  ) ;	{needed to determine rules to apply to variables exported from this scope}
    rcrd: (
      rtype: stp;	{pointer to type of record variable}
      daccess:  refchg;		{ allowed access modes (ref, chg, both) }
      dvclass: verclass;	{ verclass of WITH expression (not variable) }
      case occur: rctype of	{..set when scope is opened by WITH stmt}
	crec: (			{direct (constant) access}
	  dlev: lltype;
	  daddr: addrrange);
	cdev: (			{direct access to a device (ABSOLUTE)}
	  dvaddr: addrrange);
	vrec: (			{indirect (variable) access}
	  tnum: integer)
	  )
    end;
  top, disx, level: disprange;	{indices into display}

				{blockstack - indexed by mem alloc level }
  blockstack: array[disprange] of record
	blockpin: integer;	{ procedure number of this block }
	end;

  pin,			{procedure number}
  maxpin,		{highest number procedure seen so far}
  ac,			{parameter address counter}
  dc,			{fixed data address counter}
  lc,			{local variable address counter}
  tc: integer;		{temporary (with ...) variable counter}

				{** conditions returned by searchid **}
  foreign: boolean;	{true if ident is imported or exported; else false}
  prterr: boolean;	{print error if ident is undefined}
			{ ascii control character definitions }
			{*************************************}
  nul,soh,us,sp,del,ff: char;


  ismain: boolean;	{ true if this is main prog false if separate module}
 
				{** conditions returned by selector **}
  lastvclass: verclass;	{ verclass of last selected item } 
  lastaccess: refchg;	{ r/w permissions re monitor enforcement }
 
  gattr: attr;		{ attributes of current expression }
  lcp: itp;		{ points to main program ident structure }
  prioritycontext: 0 .. maxprio;	{priority of current context }
  signalcount: integer;
  signallist: array[1..maxsignal] of signalinfo;
  nl: char;		{ the new line character (LF, CR, .. whatever) }
  i: integer;
		{scratch variables to process outer level export list}
  endxlist: boolean;	{loop control variable}
  mchain,mchain1: chain;	{links for export list}
  idptr: itp;

   stmntnmr,			{ current stmt # - set in statement }
   firststmnt: integer;	{ first stmnt on a given line set by getnxtline}
  chartok: chartoktab;
  keyword: keywords;
  names:nametab;
  doarg:doargtab;
  bcode :binoptab;
  usize,ualign : sizetables;
  shfttab : litshfttab;
  fname : pack6; {primary name of source file,returned by getf}
  numline : integer; {number of entries in debug file}
  literalcase,xxfile,xfile:boolean;

  sourceinfo,objinfo,buginfo : pack80;	{file names for pass2 & listing}
{filelist : fparmlist; DEC-10 ONLY }  {holds list of files found by getdirectives}

  timezero, elapsedtime: integer;	{measure compiler's execution time}
{
	Verifier-language variables
}

vmodetab: array [vermodes] of vmodeswitches;	{table of permitted actions }

vmodestack: array [0..vmodestackmax] of vermodes;{ stack of verifier modes }

vmodestacktop: 0..vmodestackmax;		{ vmodestack }

vmodes: vmodeswitches;			{ current switches }

vmode: vermodes;			{ current vermode }

vtypeserial: 0..vtypeserialmax;		{ serial number of declared type }
