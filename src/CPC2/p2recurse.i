procedure WHATrecurse; const WHAT = '@(#)p2recurse.i    2.1'; begin SINK := WHAT; end;    { Version 2.1 of 10/12/82 }
{
    Recursion checking  --  check that recursive procedures terminate
}
{
    entrydepthcheck  --  check DEPTH at entry to recursive procedure

    The DEPTH clause must have a value greater than zero at entry
    to the procedure; in addition, the value must be saved for
    later use at calls to other recursive routines.
}
procedure entrydepthcheck(b: blocknodep);    { relevant block }
var i: 1..4;                    { for indentation }
begin
    with b^ do begin                { using given block }
    if blrecursive <> (bldepthexpr <> nil) then begin { if missing/extra }
        usererrorstart(blvarnode^.vardata.vrsource);{ start message }
        if blrecursive then    begin        { if missing DEPTH for recurse }
        write(output,'No DEPTH statement for recursive routine')
        end else begin            { if extra DEPTH }
        write(output,'DEPTH not needed for nonrecursive routine');
        bldepthexpr := nil;        { drop DEPTH expression }
        end;
        usererrorend;            { finish off error message }
        end;                { end missing/extra DEPTH }
    if bldepthexpr <> nil then begin    { if DEPTH present }
        assert(bldepthexpr^.code = depthop);{ must be depth operator }
        safeexpr(bldepthexpr^.arg[1]);    { must be OK to evaluate }
                        { constrain to 0..32767 }
        requirecompat(bldepthexpr,cardinalvarnode,u15,bldepthexpr^.arg[1]);
                        { now save for calls }
        assert(blockdepthtid = 0);        { only once per junit }
        blockdepthtid := nexttemp;        { assign temp number }
                        { save expr value }
        gentempasg(blockdepthtid,cardinalvarnode,bldepthexpr^.arg[1],true);
        end;                { end DEPTH present }
    end;                    { end With }
end {entrydepthcheck};
{
    calldepthcheck  --  check DEPTH relationship at call
}
procedure calldepthcheck(caller: blocknodep;    { calling routine }
             p: ptn);        { call node }
var callee: blocknodep;                { must have block }
    i: 1..4;                    { for indentation }
begin
    with p^ do begin                { using caller node }
    callee := vtype^.blockdata;        { relevant block }
    if (caller^.bldepthexpr <> nil) and
       (callee^.bldepthexpr <> nil) then begin { if recursive }
        genstring15('REQUIRE');        { REQUIRE depth relation }
        genspace;
        genstring15('(lti!');        { callee <= caller }
        genspace;
        genjexpr(callee^.bldepthexpr^.arg[1]);{ callee DEPTH }
        genspace;
                        { caller DEPTH }
        gendataid(nil,blockdepthtid,genwithoutnew,genwithoutdef);    
        genchar(')');            { close lti }
        genspace;
        genmsgstart(p^.linen);        { callee DEPTH < caller }
                        { reproduce expression }
        genmexpr1(callee^.bldepthexpr^.arg[1],relationaloperator);
        genchar(' ');
        genchar('<');
        genchar(' ');
        genstring15('"caller DEPTH"');
        for i := 1 to 4 do genchar(' ');    { 4 spaces }
        genstring15('(DEPTH');    { (DEPTH check for call of "xxx") }
        genstring15(' check for');
        genstring15(' call of "');
        genstring15(callee^.blvarnode^.vardata.itemname);
        genstring15('")');
        genmsgend;
        genline;
        end;
    end;                    { end With }
end {calldepthcheck};
