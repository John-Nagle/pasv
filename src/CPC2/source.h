{
	Source Line Retrieval Routines
}
{
	initsourceprint  --  initialize source line printer

	Must be called before calling printsourceline.
}
procedure initsourceprint;			{ no arguments }
	external;
{
	printsourceline  --  print desired source line
}
procedure printsourceline(var f: text;		{ output file }
			  n: integer);		{ desired line }
	external;
