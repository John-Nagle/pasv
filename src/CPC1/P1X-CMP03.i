{
	Dummy procedures to simulate routines in verifier 

	These routines are not called when the program is generated
	with verifier=false.
}
procedure varfilegen(dlev: disprange);		
begin
    assert(false);				{ unreachable }
end {varfilegen};
procedure paramfilegen(routine: itp; blktype: unittype);	
begin
    assert(false);				{ unreachable }
end {paramfilegen};
procedure genlineid;
begin
    assert(false);				{ unreachable }
end {genlineid};
procedure writesrcfile;
begin
    assert(false);				{ unreachable }
end {writesrcfile};
procedure vinitialize;
begin
    assert(false);				{ unreachable }
end {vinitialize};
procedure stampid(fp: itp);
begin
    assert(false);				{ unreachable }
end {stampid};
procedure writefilfile(s: pathname);
begin
    assert(false);				{ unreachable }
end {writefilfile};
procedure vdataconst(vaddr:longint; vtype:stp; vfile: longint; vline: longint); 
begin
    assert(false);				{ unreachable }
end {vdataconst};
