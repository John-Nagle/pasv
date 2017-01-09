{ ** add symbol table output to this version  **  }

{	*************************************************
	*						*
	*						*
	*	   ENGINE CONTROL ALGORITHM		*
	*	     PROGRAMMING LANGUAGE  		*
	*						*
	*	     COMPILER - PASS 1			*
	*						*
	*						*
	*************************************************  }

{
	Permission is hereby given to modify or use, but not for profit,
	any or all of this program provided that this copyright notice 
	is included:

	Copyright 1985

		Ford Motor Company
		The American Road
		Dearborn, Michigan  48121

	This work was supported by the Long Range Research Program of
	the Ford Motor Company, and was carried out at Ford Scientific
	Research Labs in Dearborn, Michigan and Ford Aerospace and
	Communications Corporation's Western Development Laboratories
	in Palo Alto, California.
}
program firstpass(input,output);
Uses sysutils;      { Free Pascal }


				{constants}
				{*********}
const
  alfaleng = 15; {max length of identifier}
  strlen = 80; {max length of string}
  strlen1 = 79;
  mbuf = 120; {size of  source buffer }
compilerversion= 'Pascal-F v 1.8(12/13/82)';  {last modified by NELSON,E.    }
	{ ** THIS VERSION OUTPUTS THE SYMBOL TABLE ** }
	{ ** Must use IOE4NR, not GTE4NR.REL with this }

				{addressing characteristics}
				{**************************}
  maxlevel = 15;		{maximum lex level}
  notyetimpl = 398;          {error msg number}
  maxdis = 32;	{maximum depth of display (lexlev + with)}
   maxdis1 = 31;

  nrkeywords = 62;		{ number of built-in keywords }
  kywdlen = 10;			{maximum length of a keyword}

  nrbuiltin = 19; {number of builtin procedures and functions}
  nrbuiltin1 = 18 ; {nrbuiltin-1}


{************TARGET MACHINE DEPENDENT STUFF FOLLOWS:**************}
  bitsau = 8;	{ bits in addressable unit }
  auword = 2;	{ addressable units in a 'preferred' size word }
  bitswd = 16 { bitsau*auword};	{ bits in a 'preferred' size word }
  maxlit = bitswd;		{ bits in largest literal }
  sfdmax = 5;  {used by GETF[ get_directory] }
  numlimit = 65536;	{ 2 ** word size of machine }
  maxtargint = 32767;		{ largest signed intgr in target machine}
  mintargint = -32768;	{smallest signed integer  }


			{ the constants minfix, maxfix and stdpcn are }
			{ used as defaults for error recovery	      }
maxfix = 32768.0;
minfix = -32768.0;
stdpcn = 1.0;
fixsiz=2;  	{size of fixed point variable in bytes}


maxset = 16;	{ maximum number of elements in any set }


maxprio = 15;	{maximum allowed priority level for a monitor}
maxsignal = 127;	{ max number of signals which can be declared }

			{constants which define error numbers}
			{************************************}
  fxptsyntx = 60;	    {incorrect syntax for FIXED declaration}
  fxptrange = 61;	    {invalid range for FIXED type}
  fxptprcsn = 62;	    {invalid precision for FIXED type}
{
	Constants for verifier language extensions
}
maxargs = 31;			{ Max args for any operator }
maxinseq = maxargs;		{ Max count for any SEQ operator }
illegaladdress = 999999;	{ used as address of EXTRA vars in compiler }
vmodestackmax = 25;		{ max depth for calls: f(f(f(f)))) forms    }
enforce = true;			{ if true, verifier restrictions enforced }
vtypeserialmax = 50000;		{ maximum number of declared types }
