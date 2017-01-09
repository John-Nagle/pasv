procedure initprocedure;
begin

    chartok[s00].sy :=relop; chartok[s00].op :=neop;	{s00: '#'}
    chartok[s01].sy :=lparen; chartok[s01].op :=noop;	{s01: '('}
    chartok[s02].sy :=rparen; chartok[s02].op :=noop;	{s02: ')'}
    chartok[s03].sy :=mulop; chartok[s03].op :=mul;	{s03: '*'}
    chartok[s04].sy :=addop; chartok[s04].op :=plus;	{s04: '+'}
    chartok[s05].sy :=comma; chartok[s05].op :=noop;	{s05: ','}
    chartok[s06].sy :=addop; chartok[s06].op :=minus;	{s06: '-'}
    chartok[s07].sy :=mulop; chartok[s07].op :=sdiv;	{s07: '/'}
    chartok[s08].sy :=semicolon; chartok[s08].op :=noop;	{s08: ';'}
    chartok[s09].sy :=relop; chartok[s09].op :=eqop;	{s09: '='}
    chartok[s11].sy :=lbrack; chartok[s11].op :=noop;	{s11: '['}
    chartok[s12].sy :=rbrack; chartok[s12].op :=noop;	{s12: ']'}


 keyword[ 1].id.l:=3; keyword[ 1].id.s:='end       ';
 keyword[ 1].sym.sy :=endsy; keyword[ 1].sym.op:= noop;

 keyword[ 2].id.l:=5; keyword[ 2].id.s:='begin     ';
 keyword[ 2].sym.sy:=beginsy; keyword[ 2].sym.op:= noop;

 keyword[ 3].id.l:=2; keyword[ 3].id.s:='if        ';
 keyword[ 3].sym.sy:=ifsy; keyword[ 3].sym.op:= noop;

 keyword[ 4].id.l:=4; keyword[ 4].id.s:='then      ';
 keyword[ 4].sym.sy:=thensy; keyword[ 4].sym.op:= noop;

 keyword[ 5].id.l:=4; keyword[ 5].id.s:='else      ';
 keyword[ 5].sym.sy:=elsesy; keyword[ 5].sym.op:= noop;

 keyword[ 6].id.l:=3; keyword[ 6].id.s:='div       ';
 keyword[ 6].sym.sy:=mulop; keyword[ 6].sym.op:= idiv;

 keyword[ 7].id.l:=3; keyword[ 7].id.s:='mod       ';
 keyword[ 7].sym.sy:=mulop; keyword[ 7].sym.op:= imod;

 keyword[ 8].id.l:=2; keyword[ 8].id.s:='do        ';
 keyword[ 8].sym.sy:=dosy; keyword[ 8].sym.op:= noop;

 keyword[ 9].id.l:=5; keyword[ 9].id.s:='while     ';
 keyword[ 9].sym.sy:=whilesy; keyword[ 9].sym.op:= noop;

 keyword[10].id.l:=6; keyword[10].id.s:='repeat    ';
 keyword[10].sym.sy:=repeatsy; keyword[10].sym.op:= noop;

 keyword[11].id.l:=5; keyword[11].id.s:='until     ';
 keyword[11].sym.sy:=untilsy; keyword[11].sym.op:= noop;

 keyword[12].id.l:=4; keyword[12].id.s:='with      ';
 keyword[12].sym.sy:=withsy; keyword[12].sym.op:= noop;

 keyword[13].id.l:=4; keyword[13].id.s:='case      ';
 keyword[13].sym.sy:=casesy; keyword[13].sym.op:= noop;

 keyword[14].id.l:=3; keyword[14].id.s:='var       ';
 keyword[14].sym.sy:=varsy; keyword[14].sym.op:= noop;

 keyword[15].id.l:=8; keyword[15].id.s:='priority  ';
 keyword[15].sym.sy:=prioritysy; keyword[15].sym.op:= noop;

 keyword[16].id.l:=3; keyword[16].id.s:='not       ';
 keyword[16].sym.sy:=notsy; keyword[16].sym.op:= noop;

 keyword[17].id.l:=2; keyword[17].id.s:='or        ';
 keyword[17].sym.sy:=addop; keyword[17].sym.op:= orop;

 keyword[18].id.l:=3; keyword[18].id.s:='and       ';
 keyword[18].sym.sy:=mulop; keyword[18].sym.op:= andop;

 keyword[19].id.l:=2; keyword[19].id.s:='to        ';
 keyword[19].sym.sy:=tosy; keyword[19].sym.op:= noop;

 keyword[20].id.l:=2; keyword[20].id.s:='in        ';
 keyword[20].sym.sy:=relop; keyword[20].sym.op:= inop;

 keyword[21].id.l:=3; keyword[21].id.s:='for       ';
 keyword[21].sym.sy:=forsy; keyword[21].sym.op:= noop;

 keyword[22].id.l:=2; keyword[22].id.s:='of        ';
 keyword[22].sym.sy:=ofsy; keyword[22].sym.op:= noop;

 keyword[23].id.l:=5; keyword[23].id.s:='array     ';
 keyword[23].sym.sy:=arraysy; keyword[23].sym.op:= noop;

 keyword[24].id.l:=5; keyword[24].id.s:='const     ';
 keyword[24].sym.sy:=constsy; keyword[24].sym.op:= noop;

 keyword[25].id.l:=5; keyword[25].id.s:='fixed     ';
 keyword[25].sym.sy:=fixedsy; keyword[25].sym.op:=noop;

 keyword[26].id.l:=6; keyword[26].id.s:='packed    ';
 keyword[26].sym.sy:=packedsy; keyword[26].sym.op:= noop;

 keyword[27].id.l:=6; keyword[27].id.s:='record    ';
 keyword[27].sym.sy:=recordsy; keyword[27].sym.op:= noop;

 keyword[28].id.l:=3; keyword[28].id.s:='set       ';
 keyword[28].sym.sy:=setsy; keyword[28].sym.op:= noop;

 keyword[29].id.l:=4; keyword[29].id.s:='type      ';
 keyword[29].sym.sy:=typesy; keyword[29].sym.op:= noop;

 keyword[30].id.l:=6; keyword[30].id.s:='others    ';
 keyword[30].sym.sy:=othersy; keyword[30].sym.op:= noop;

 keyword[31].id.l:=6; keyword[31].id.s:='downto    ';
 keyword[31].sym.sy:=downtosy; keyword[31].sym.op:= noop;

 keyword[32].id.l:=9; keyword[32].id.s:='procedure ';
 keyword[32].sym.sy:=proceduresy; keyword[32].sym.op:= noop;

 keyword[33].id.l:=8; keyword[33].id.s:='function  ';
 keyword[33].sym.sy:=functionsy; keyword[33].sym.op:= noop;

 keyword[34].id.l:=7; keyword[34].id.s:='forward   ';
 keyword[34].sym.sy:=forwardsy; keyword[34].sym.op:= noop;

 keyword[35].id.l:=6; keyword[35].id.s:='extern    ';
 keyword[35].sym.sy:=externalsy; keyword[35].sym.op:= noop;

 keyword[36].id.l:=7; keyword[36].id.s:='program   ';
 keyword[36].sym.sy:=programsy; keyword[36].sym.op:= noop;

 keyword[37].id.l:=9; keyword[37].id.s:='precision ';
 keyword[37].sym.sy:=precisionsy; keyword[37].sym.op:=noop;

 keyword[38].id.l:=5; keyword[38].id.s:='value     ';
 keyword[38].sym.sy:=valuesy; keyword[38].sym.op:=noop;

 keyword[39].id.l:=4; keyword[39].id.s:='when      ';
 keyword[39].sym.sy:=whensy; keyword[39].sym.op:=noop;

 keyword[40].id.l:=5; keyword[40].id.s:='raise     ';
 keyword[40].sym.sy:=raisesy; keyword[40].sym.op:=noop;

 keyword[41].id.l:=7; keyword[41].id.s:='exports   ';
 keyword[41].sym.sy:=exportsy; keyword[41].sym.op:=noop;

 keyword[42].id.l:=7; keyword[42].id.s:='imports   ';
 keyword[42].sym.sy:=importsy; keyword[42].sym.op:=noop;

 keyword[43].id.l:=4; keyword[43].id.s:='init      ';
 keyword[43].sym.sy:=initsy; keyword[43].sym.op:=noop;

 keyword[44].id.l:=6; keyword[44].id.s:='module    ';
 keyword[44].sym.sy:=modulesy; keyword[44].sym.op:=noop;

 keyword[45].id.l:=7; keyword[45].id.s:='monitor   ';
 keyword[45].sym.sy:=monitorsy; keyword[45].sym.op:=noop;

 keyword[46].id.l:=6; keyword[46].id.s:='device    ';
 keyword[46].sym.sy:=devicesy;  keyword[46].sym.op:=noop;
				{ verifier reserved words follow }
 keyword[47].id.l:=4; keyword[47].id.s:='rule      ';
 keyword[47].sym.sy:=rulesy; keyword[47].sym.op:= noop;

 keyword[48].id.l:=6; keyword[48].id.s:='assert    ';
 keyword[48].sym.sy:=assertsy; keyword[48].sym.op:= noop;

 keyword[49].id.l:=5; keyword[49].id.s:='depth     ';
 keyword[49].sym.sy:=depthsy; keyword[49].sym.op:= noop;

 keyword[50].id.l:=6; keyword[50].id.s:='effect    ';
 keyword[50].sym.sy:=effectsy; keyword[50].sym.op:= noop;

 keyword[51].id.l:=5; keyword[51].id.s:='entry     ';
 keyword[51].sym.sy:=entrysy; keyword[51].sym.op:= noop;

 keyword[52].id.l:=5; keyword[52].id.s:='extra     ';
 keyword[52].sym.sy:=extrasy; keyword[52].sym.op:= noop;

 keyword[53].id.l:=6; keyword[53].id.s:='return    ';
 keyword[53].sym.sy:=returnsy; keyword[53].sym.op:= noop;

 keyword[54].id.l:=7; keyword[54].id.s:='implies   ';
 keyword[54].sym.sy:=relop;     keyword[54].sym.op:= impliesop;

 keyword[55].id.l:=9; keyword[55].id.s:='invariant ';
 keyword[55].sym.sy:=invariantsy; keyword[55].sym.op:= noop;

 keyword[56].id.l:=7; keyword[56].id.s:='measure   ';
 keyword[56].sym.sy:=measuresy; keyword[56].sym.op:= noop;

 keyword[57].id.l:=5; keyword[57].id.s:='proof     ';
 keyword[57].sym.sy:=proofsy; keyword[57].sym.op:= noop;

 keyword[58].id.l:=5; keyword[58].id.s:='state     ';
 keyword[58].sym.sy:=statesy; keyword[58].sym.op:= noop;

 keyword[59].id.l:=7; keyword[59].id.s:='summary   ';
 keyword[59].sym.sy:=summarysy; keyword[59].sym.op:= noop;

 keyword[60].id.l:=3; keyword[60].id.s:='old       ';
 keyword[60].sym.sy:=oldsy; keyword[60].sym.op:=noop;

 keyword[61].id.l:=4; keyword[61].id.s:='exit      ';
 keyword[61].sym.sy:=exitsy; keyword[61].sym.op:=noop;


     names[ 0].l := 4; names[ 0].s := 'pred           ';
     names[ 1].l := 4; names[ 1].s := 'succ           ';
     names[ 2].l := 3; names[ 2].s := 'ord            ';
     names[ 3].l := 3; names[ 3].s := 'chr            ';
     names[ 4].l := 5; names[ 4].s := 'trunc          ';
     names[ 5].l := 5; names[ 5].s := 'round          ';
     names[ 6].l := 3; names[ 6].s := 'max            ';
     names[ 7].l := 3; names[ 7].s := 'min            ';
     names[ 8].l := 4; names[ 8].s := 'ceil           ';
     names[ 9].l := 5; names[ 9].s := 'floor          ';
     names[10].l := 3; names[10].s := 'abs            ';
     names[11].l := 3; names[11].s := 'sqr            ';
     names[12].l := 4; names[12].s := 'sqrt           ';
     names[13].l := 4; names[13].s := 'wait           ';
     names[14].l := 4; names[14].s := 'send           ';
     names[15].l := 7; names[15].s := 'awaited        ';
     names[16].l := 3; names[16].s := 'odd            ';
     names[17].l := 6; names[17].s := 'enable         ';
     names[18].l := 7; names[18].s := 'defined        ';

  {usize and ualignn originally initialized in block}
 usize[scalar]:=1 ; usize[ booleant]:=1 ; usize[ chart]:=1 ; usize[ integert]:=2 ; usize[ longintt]:=4 ;
 usize[xcptnt] := 2; usize[signalt] := 2;usize[pointer]:=2 ; usize[sett]:= 0; usize[fixedt] := 2;
 ualign[scalar]:=1; ualign[booleant]:=1; ualign[chart]:=1; ualign[integert]:=2; ualign[longintt]:=2;
 ualign[pointer]:=2 ; ualign[sett]:= 0; ualign[fixedt] := 2; ualign[xcptnt]:=2; ualign[signalt] := 2;
 ualign[arrayt]:=0 ; ualign[recordt]:=0 ;; ualign[tagfield]:=0 ; ualign[variant]:=0;

		  {Binary values for intermediate code operators}
		  {*********************************************}

 bcode[scalar, mul]:=0; bcode[scalar, andop]:=0; bcode[scalar,idiv]:=0; bcode[scalar,imod]:=0;
 bcode[scalar,plus]:= 16; bcode[scalar, minus]:= 17; bcode[scalar,orop]:=0; bcode[scalar,ltop]:= 29;
 bcode[scalar,leop]:= 27; bcode[scalar,geop]:= 28; bcode[scalar,gtop]:= 26; bcode[scalar,neop]:= 25;
 bcode[scalar,eqop]:= 24; bcode[scalar,inop]:=0; bcode[scalar, maxop]:= 30; bcode[scalar, minop]:= 31;
 bcode[scalar,ceilop]:=0; bcode[scalar, floorop]:=0; bcode[scalar,noop]:=0; bcode[booleant, mul]:=0;
 bcode[booleant, andop]:=111; bcode[booleant,idiv]:=0; bcode[booleant,imod]:=0; bcode[booleant,plus]:=0;
 bcode[booleant, minus]:=0; bcode[booleant,orop]:=110; bcode[booleant,ltop]:=109; bcode[booleant,leop]:=108;
 bcode[booleant,geop]:=107; bcode[booleant,gtop]:=106; bcode[booleant,neop]:=105; bcode[booleant,eqop]:=104;
 bcode[booleant,inop]:=0; bcode[booleant, maxop]:=0; bcode[booleant, minop]:=0; bcode[booleant,ceilop]:=0;
 bcode[booleant, floorop]:=0; bcode[booleant,noop]:=0; bcode[chart, mul]:=0; bcode[chart, andop]:=0;
 bcode[chart,idiv]:=0; bcode[chart,imod]:=0; bcode[chart,plus]:= 16; bcode[chart, minus]:= 17;
 bcode[chart,orop]:=0; bcode[chart,ltop]:= 29; bcode[chart,leop]:= 27; bcode[chart,geop]:= 28;
 bcode[chart,gtop]:= 26; bcode[chart,neop]:= 25; bcode[chart,eqop]:= 24; bcode[chart,inop]:=0;
 bcode[chart, maxop]:= 30; bcode[chart, minop]:= 31; bcode[chart,ceilop]:=0; bcode[chart, floorop]:=0;
 bcode[chart,noop]:=0; bcode[integert, mul]:= 34; bcode[integert, andop]:=0; bcode[integert,idiv]:= 35;
 bcode[integert,imod]:= 36; bcode[integert,plus]:= 32; bcode[integert, minus]:= 33; bcode[integert,orop]:=0;
 bcode[integert,ltop]:= 61; bcode[integert,leop]:= 59; bcode[integert,geop]:= 60; bcode[integert,gtop]:= 58;
 bcode[integert,neop]:= 57; bcode[integert,eqop]:= 56; bcode[integert,inop]:=0; bcode[integert, maxop]:= 62;
 bcode[integert, minop]:= 63; bcode[integert,ceilop]:= 44; bcode[integert, floorop]:= 45; bcode[integert,noop]:=0;
 bcode[longintt, mul]:= 34; bcode[longintt, andop]:=0; bcode[longintt,idiv]:= 35; bcode[longintt,imod]:= 36;
 bcode[longintt,plus]:= 32; bcode[longintt, minus]:= 33; bcode[longintt,orop]:=0; bcode[longintt,ltop]:= 61;
 bcode[longintt,leop]:= 59; bcode[longintt,geop]:= 60; bcode[longintt,gtop]:= 58; bcode[longintt,neop]:= 57;
 bcode[longintt,eqop]:= 56; bcode[longintt,inop]:=0; bcode[longintt, maxop]:= 62; bcode[longintt, minop]:= 63;
 bcode[longintt,ceilop]:= 44; bcode[longintt, floorop]:= 45; bcode[longintt,noop]:=0;
bcode[fixedt, mul]:=50; bcode[fixedt, andop]:=0; bcode[fixedt, sdiv]:=51;
bcode[fixedt, idiv]:=0; bcode[fixedt,imod]:=0; bcode[fixedt, plus]:=48;
bcode[fixedt, minus]:=49; bcode[fixedt, orop]:=0; bcode[fixedt, leop]:=83;
bcode[fixedt, ltop]:=85; bcode[fixedt, geop]:=84; bcode[fixedt, gtop]:=82;
bcode[fixedt, neop]:=81; bcode[fixedt, eqop]:=80; bcode[fixedt, maxop]:=86;
bcode[fixedt, minop]:=87;
 bcode[pointer, mul]:=0; bcode[pointer, andop]:=0; bcode[pointer,idiv]:=0;
 bcode[pointer,imod]:=0; bcode[pointer,plus]:=0; bcode[pointer, minus]:=0; bcode[pointer,orop]:=0;
 bcode[pointer,ltop]:=0; bcode[pointer,leop]:=0; bcode[pointer,geop]:=0; bcode[pointer,gtop]:=0;
 bcode[pointer,neop]:= 25; bcode[pointer,eqop]:= 24; bcode[pointer,inop]:=0; bcode[pointer, maxop]:=0;
 bcode[pointer, minop]:=0; bcode[pointer,ceilop]:=0; bcode[pointer, floorop]:=0; bcode[pointer,noop]:=0;
 bcode[sett, mul]:=114; bcode[sett, andop]:=0; bcode[sett,idiv]:=0; bcode[sett,imod]:=0;
 bcode[sett,plus]:=113; bcode[sett, minus]:=115; bcode[sett,orop]:=0; bcode[sett,ltop]:=0;
 bcode[sett,leop]:=123; bcode[sett,geop]:=124; bcode[sett,gtop]:=0; bcode[sett,neop]:=121;
 bcode[sett,eqop]:=120; bcode[sett,inop]:=126; bcode[sett, maxop]:=0; bcode[sett, minop]:=0;
 bcode[sett,ceilop]:=0; bcode[sett, floorop]:=0; bcode[sett,noop]:=0; bcode[arrayt, mul]:=0;
 bcode[arrayt, andop]:=0; bcode[arrayt,idiv]:=0; bcode[arrayt,imod]:=0; bcode[arrayt,plus]:=0;
 bcode[arrayt, minus]:=0; bcode[arrayt,orop]:=0; bcode[arrayt,ltop]:=173; bcode[arrayt,leop]:=171;
 bcode[arrayt,geop]:=172; bcode[arrayt,gtop]:=170; bcode[arrayt,neop]:=169; bcode[arrayt,eqop]:=168;
 bcode[arrayt,inop]:=0; bcode[arrayt, maxop]:=0; bcode[arrayt, minop]:=0; bcode[arrayt,ceilop]:=0;
 bcode[arrayt, floorop]:=0; bcode[arrayt,noop]:=0; bcode[recordt, mul]:=0; bcode[recordt, andop]:=0;
 bcode[recordt,idiv]:=0; bcode[recordt,imod]:=0; bcode[recordt,plus]:=0; bcode[recordt, minus]:=0;
 bcode[recordt,orop]:=0; bcode[recordt,ltop]:=0; bcode[recordt,leop]:=0; bcode[recordt,geop]:=0;
 bcode[recordt,gtop]:=0; bcode[recordt,neop]:=0; bcode[recordt,eqop]:=0; bcode[recordt,inop]:=0;
 bcode[recordt, maxop]:=0; bcode[recordt, minop]:=0; bcode[recordt,ceilop]:=0; bcode[recordt, floorop]:=0;
 bcode[recordt,noop]:=0;
 bcode[tagfield, mul]:=0; bcode[tagfield, andop]:=0; bcode[tagfield,idiv]:=0; bcode[tagfield,imod]:=0;
 bcode[tagfield,plus]:=0; bcode[tagfield, minus]:=0; bcode[tagfield,orop]:=0; bcode[tagfield,ltop]:=0;
 bcode[tagfield,leop]:=0; bcode[tagfield,geop]:=0; bcode[tagfield,gtop]:=0; bcode[tagfield,neop]:=0;
 bcode[tagfield,eqop]:=0; bcode[tagfield,inop]:=0; bcode[tagfield, maxop]:=0; bcode[tagfield, minop]:=0;
 bcode[tagfield,ceilop]:=0; bcode[tagfield, floorop]:=0; bcode[tagfield,noop]:=0; bcode[variant, mul]:=0;
 bcode[variant, andop]:=0; bcode[variant,idiv]:=0; bcode[variant,imod]:=0; bcode[variant,plus]:=0;
 bcode[variant, minus]:=0; bcode[variant,orop]:=0; bcode[variant,ltop]:=0; bcode[variant,leop]:=0;
 bcode[variant,geop]:=0; bcode[variant,gtop]:=0; bcode[variant,neop]:=0; bcode[variant,eqop]:=0;
 bcode[variant,inop]:=0; bcode[variant, maxop]:=0; bcode[variant, minop]:=0;
bcode[variant,ceilop]:=0;  bcode[variant, floorop]:=0; bcode[variant,noop]:=0;
					{ verifier additions }
bcode[booleant, impliesop]:= 108;	{ implement implies as "<=" }
 bcode[arrayt,impliesop] := 0;    		{ not meaningful }
 bcode[chart,impliesop] := 0;    		{ not meaningful }
 bcode[fixedt,impliesop] := 0;    		{ not meaningful }
 bcode[integert,impliesop] := 0;    		{ not meaningful }
 bcode[longintt,impliesop] := 0;    		{ not meaningful }
 bcode[pointer,impliesop] := 0;    		{ not meaningful }
 bcode[recordt,impliesop] := 0;    		{ not meaningful }
 bcode[scalar,impliesop] := 0;    		{ not meaningful }
 bcode[sett,impliesop] := 0;    		{ not meaningful }
 bcode[tagfield,impliesop] := 0;    		{ not meaningful }
 bcode[variant,impliesop] := 0;    		{ not meaningful }
{
	Characters allowed in file names - used for include mechanism
}
validpathchars := [
	'a','b','c','d','e','f','g','h','i','j','k','l','m',
	'n','o','p','q','r','s','t','u','v','w','x','y','z',
	'A','B','C','D','E','F','G','H','I','J','K','L','M',
	'N','O','P','Q','R','S','T','U','V','W','X','Y','Z',
	'0','1','2','3','4','5','6','7','8','9',
	'.','/','-','#','+','!','$','%','~','^',','];

end {initprocedure};
{
	initvmodetab  --  initialize mode table

	The mode table determines what types of verification expressions
	are permitted in what contexts.
}
procedure initvmodetab;
const t = true; f = false;		{ to make table smaller }
{
	row  --  initialize one row
}
procedure row(rowid: vermodes;		{ row to initialize }
	  p1, p2, p3, p4, p5, p6, p7, p8, p9: boolean);
begin
    with vmodetab[rowid] do begin	{ using indicated row }
	generatecode := p1;		{ initialize fields in order }
	variableref[executablevar] := p2;
	variableref[extravar] := p3;
	variablechg[executablevar] := p4;
	variablechg[extravar] := p5;
	paramallowed := p6;
	localallowed := p7;
	globalallowed := p8;
	bodyallowed := p9;
	end;
end {row};
begin {initvmodetab} 
{
	Table of allowed actions for different modes 

bodyallowed-----------------------------------------|
globalallowed-----------------------------------|   |
localallowed--------------------------------|   |   |
paramallowed----------------------------|   |   |   |
chg[extravar]-----------------------|   |   |   |   |
chg[executablevar]--------------|   |   |   |   |   |
ref[extravar]---------------|   |   |   |   |   |   |
ref[executablevar]------|   |   |   |   |   |   |   |
generatecode--------|   |   |   |   |   |   |   |   |
		    |   |   |   |   |   |   |   |   |   }
row(codemode,	    t,  t,  f,  t,  f,  t,  t,  t,  t);
row(proofmode,	    f,  t,  t,  f,  t,  t,  t,  t,  t);
row(assertmode,	    f,  t,  t,  f,  f,  t,  t,  t,  f);
row(entrymode,	    f,  t,  t,  f,  f,  t,  f,  f,  f);
row(exitmode,	    f,  t,  t,  f,  f,  t,  f,  f,  f);
row(effectmode,	    f,  t,  t,  f,  f,  t,  f,  f,  f);
row(invariantmode,  f,  t,  t,  f,  f,  f,  t,  f,  f);
row(rulemode,	    f,  f,  f,  f,  f,  f,  f,  f,  f);
end {initvmodetab};

procedure initglobal;
var
  i:longint;
begin

			{initialize values of control characters}
  nul:=chr(0); soh:=chr(1); ff:=chr(12);  us:=chr(31); del:=chr(127);
 sp:=chr(32);

			{ next initialize table of character types }

  initprocedure;			{ do constant initializations }
  initvmodetab;				{ initialize vmode table }
  chartab[nul] :=   eos;
  for ch:=succ(nul) to pred(sp) do chartab[ch]:=ctl;
 chartab[sp]:=  oth; chartab['!']:=oth; chartab['"']:=xhex; chartab['#']:=s00;
  for ch:='$' to '%' do chartab[ch]:=oth;
 chartab['''']:=quo;
 chartab['(']:=s01; chartab[')']:=s02; chartab['*']:=s03; chartab['+']:=s04;
 chartab[',']:=s05; chartab['-']:=s06; chartab['.']:=db3; chartab['/']:=s07;
  for ch:='0' to '9' do chartab[ch]:=dig;
 chartab[':']:=db0; chartab[';']:=s08;
 chartab['<']:=db1; chartab['=']:=s09; chartab['>']:=db2; chartab['?']:=oth;
  for ch:='A' to 'Z' do chartab[ch]:=let;
 chartab['[']:=s11;
 chartab['\']:=oth; chartab[']']:=s12; chartab['_']:=let;
  for ch:='a' to 'z' do chartab[ch]:=let;
 chartab['{']:=oth;
 chartab['|']:=oth; chartab['}']:=oth; chartab['~']:=oth; chartab[del]:=ctl ;

  {doarg originally initialized in call }
   {doarg describes parameters and returned val for built in procs}
for i := 0 to nrbuiltin1 do doarg[i] := [hasarg,getarg];
doarg[18] := [hasarg];			{ defined(<expr>) special case }

  {shfftab originally initialized in block}
shfttab[0] := 256; shfttab[1] := 1; shfttab[2] := numlimit;
    end {initglobal};

procedure newid(fc: classes; fq: stp; fn: itp; var fp: itp);
  forward;	{ .. needed to enter predefined idents in inittables }


procedure talloc( var ty:stp; fm: forms; sbrng:boolean );
  forward;		{ type allocation procedure }

procedure inittables; {initialized symbol tables with standard
                      and predeclared identifiers and types}
 
 
var
  p: itp;
  i: longint;
begin {inittables}
{Initialize display}
  level := 0;  top := 0;
  with display[0] do begin
    fname := nil;  scope := blck;
    end;
  with blockstack[0] do begin			{ initialize stack numbering }
    blockpin := 0;				{ serial number of outer block }
    end;
{***integer***}
  talloc( intptr, integert, false );  {allocate type longint}
  with intptr^ do begin  			{ define type longint }
	size := 2;				{ 2 bytes }
	minvalue := -32768;         		{ minimum in 16-bit 2s comp }  
	maxvalue := +32767;			{ maximum integer }
	end;
  id.l := 7; id.s :='integer        ';
  newid(types,intptr,nil,p);
{***char***}
  talloc( charptr, chart, false );
  with charptr^ do begin			{ define type char }
	size := 1;				{ 1 byte }
	minvalue := 0;				{ minimum numeric value }
	maxvalue := 255;			{ maximum numeric value }
	end;
  id.l := 4; id.s :='char           ';
  newid(types,charptr,nil,p);
{***false,true,boolean***}
  talloc( boolptr, booleant, false );
  with boolptr^ do begin			{ define type boolean }
	size := 1;				{ 1 byte }
	minvalue := 0;				{ minimum numeric value }
	maxvalue := 1;				{ maximum numeric value }
	end;
  id.l := 5; id.s :='false          ';
  newid(konst,boolptr,nil,p);
  p^.kvalue.ival := 0;
  id.l := 4; id.s :='true           ';
  newid(konst,boolptr,p,p);
  p^.kvalue.ival := 1;
  boolptr^.maxconst := p;
  id.l := 7; id.s :='boolean        ';
  newid(types,boolptr,nil,p);
{***exception***}
  talloc( xcptnptr, xcptnt, false );
  with xcptnptr^ do  begin form := xcptnt; size := 2 end;
  id.l:= 9;  id.s:='exception      ';
  newid(types,xcptnptr,nil,p);
{***signal***}
  talloc( signalptr, signalt, false );
  with signalptr^ do  begin size:=2 end;
  id.l:=  6;  id.s:='signal         ';
  newid(types,signalptr,nil,p);
{***undefined type***}
  talloc(notypeptr,integert,false);	{ define error type as 0..1 }
  with notypeptr^ do begin size := 2; 
			   maxvalue := 1; minvalue := 0; end;
  id.l := 11; id.s := '*UNDEFINED*    ';
{***builtin procedures and functions***}
  for i := 0 to nrbuiltin-1 do begin
    id := names[i];
    newid(proc,nil,nil,p);
    with p^ do begin
      pkind := stnd; psinx := i
      end
    end;
{***enter undeclared identifiers***}
  id.l :=  3; id.s[1] := '.'; id.s[2] := 'u'; id.s[3] := 't';
  newid(types,notypeptr,nil,udptrs[types]);
  id.s[3] := 'c';
  newid(konst,notypeptr,nil,udptrs[konst]);
  id.s[3] := 'v';
  newid(vars,notypeptr,nil,udptrs[vars]);
  udptrs[vars]^.vkind := local;
  with udptrs[vars]^ do begin	{ initialize default var }
	vkind := local;
	vclass := executablevar;	{ mark as normal executable var }
	end;
  id.s[3] := 'f';
  newid(field,notypeptr,nil,udptrs[field]);
  id.s[3] := 'p';
  newid(proc,nil,nil,udptrs[proc]);
  udptrs[proc]^.paddr := 1;     {* to supress spurious error message}
  udptrs[proc]^.pkind := decl;
  udptrs[proc]^.restrictor := nil;
  id.s[3] := 'm';
  newid(modul,nil,nil,udptrs[modul]);
  udptrs[modul]^.maddr := 1;
  udptrs[modul]^.mkind := decl;
  id.s[3] := 'x';
  newid(xports,nil,nil,udptrs[xports])
end {inittables};
