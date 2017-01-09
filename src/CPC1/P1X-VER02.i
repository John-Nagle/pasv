{
	VER02  --  verifier variables
}
varfile: file of varitem;		{ variables file for pass 2 }
srcfile: file of sourceline;		{ source lines for later passes }
filfile: text;				{ file of file names read }
srcitem: sourceline;			{ source line for source file }
srcserial: longint;			{ ties source lines to icode }
{
	Verifier data initialization procedures
}
procedure vinitialize;
begin
  assign(varfile,'pasf-vars');
  rewrite(varfile);			            { ***TEMP*** }
  assign(srcfile,'pasf-source');
  rewrite(srcfile);		                { ***TEMP*** }
  assign(filfile,'pasf-files');
  rewrite(filfile);		                { ***TEMP*** }
  srcserial := 0;				{ at line zero in file }
end {vinitialize};
{
	stampid  --  stamp ident with line number 
}
procedure stampid(fp: itp);			{ pointer to id to stamp }
begin
  with fp^ do begin				{ using id node }
	fileser := lastfilenumber;		{ later emitted to varfile }
	lineser := lastlinenumber;		{ later emitted to varfile }
	end;
end {stampid};
{
	writesrcfile   --   generate source-line file

	This file is used for the generation of error messages by
	later passes of the Verifier.
}
procedure writesrcfile;
begin
	srcserial := srcserial + 1;		{ increment line in file }
	srcitem.lineid.linenumber := lastlinenumber; { within-file number }
	srcitem.lineid.filenumber := lastfilenumber; { file serial }
	srcitem.linetext := bline;		{ move line to buffer }
	write(srcfile,srcitem);			{ write source line for pass 2}
end {writesrcfile};
{
	writefilfile  --  generate file of file names

	This file is used to identify the file numbers used in the source
	line file.
}
procedure writefilfile(s: pathname);		{ file path name }
begin
    writeln(filfile, s);			{ write file name to file }
end {writefilfile};
