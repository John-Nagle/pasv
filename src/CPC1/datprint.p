{
	datprint  --  print constant data file from pass one of
		      Pascal-F compiler.
}
program datprint(output);
const datname = 'pasf-data';			{ file name to read }
var i,n: integer;				{ address and data }
    dat: file of integer;			{ file to read }
begin
    n := 0;					{ clear byte counter }
    reset(dat, datname);			{ open input file }
    while not eof(dat) do begin			{ for all records }
	read(dat,n);				{ read item }
	writeln(i:8,'.',n:8);			{ address and value }
	i := i + 1;				{ count bytes }
	end;
end.

