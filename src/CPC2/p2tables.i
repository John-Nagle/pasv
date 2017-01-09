procedure WHATtables; const WHAT = '@(#)p2tables.i    2.6'; begin SINK := WHAT; end;    { Version 2.6 of 1/6/83 }
{
    Tables for icode to jcode translation
}
{
    initmttab  --  initialize the machine types table

    Machine types are the built-in types of the icode machine
}
procedure initmttab;
const big = 9999999;                { any big result may overflow }
{
    mt  --  initialize one row of mttab
}
procedure mt(mtype: machinetype;        { machine type of entry }
         kind: datakind;            { kind of data }
         minval: targetnumber;        { minimum allowed value }
         maxval: targetnumber;        { maximum allowed value }
         zerook: boolean;            { zero OK or out of bounds? }
         valued: boolean;            { meaningful in expressions? }
         simple: boolean);            { simple type? }
begin
    with mttab[mtype] do begin            { using table entry }
    mtkind := kind;                { data kind }
    mtmin := minval;            { low bound }
    mtmax := maxval;            { high bound }
    mtzerook := zerook;            { zero OK or out of bounds? }
    mtvalued := valued;            { value meaningful at all? }
    mtsimple := simple;            { simple type? }
    end;
end {mt};
begin {initmttab};                { begin initializing }
{
    Machine types
    These are the types of the icode machine
}
{ mtype data class      min     max     0 OK    valued  simple }
mt(b1,    booleandata,    0,    1,    true,    true,    true);
mt(i8,  numericdata,    -128,    +127,    true,    true,    true);
mt(i16, numericdata,    -32768, +32767, true,    true,    true);
mt(ibig,numericdata,    -big,    +big,    true,    true,    true);
mt(ni16,numericdata,    -32768, +32767, false,    true,    true);
mt(ni8, numericdata,    -128,    +127,    false,    true,    true);
mt(nu7, numericdata,    1,    +127,    false,    true,    true);
mt(nu8, numericdata,    1,    +255,    false,    true,    true);     
mt(nu15,numericdata,    1,    +32767, false,    true,    true);
mt(u4,  numericdata,    0,    +15,    true,    true,    true);
mt(u7,    numericdata,    0,    +127,    true,    true,    true);
mt(u8,  numericdata,    0,    +255,    true,    true,    true);
mt(u15, numericdata,    0,    +32767, true,    true,    true);
mt(u16, numericdata,    0,    +65535, true,    true,    true);
mt(ubig,numericdata,    0,    +big,    true,    true,    true);
mt(s16, setdata,    0,    +15,    true,    true,    true);
mt(f16, fixeddata,    -32768, +32767, true,    true,    true);
mt(ni16,numericdata,    -32768, +32767, false,    true,    true);
mt(sig, signaldata,    0,    0,    true,    true,    false);
mt(xxx,    numericdata,    0,    -1,    false,    false,    false);    
mt(addr,numericdata,    0,    -1,    false,    false,    false);    
mt(data,numericdata,    0,    -1,    false,    false,    false);    
mt(unp,    numericdata,    0,    -1,    false,    false,    false);    
mt(ind, numericdata,    0,    -1,    false,  false,    false);
mt(pnt,    numericdata,    0,    -1,    false,    false,    false);    

end {initmttab};
{
    initoptab  --  initialize the table of operators

    This table serves multiple purposes, including

    1.  Icode opcode to printable name for icode debugging printouts
    2.  Icode to Jcode translation for simple operators
    3.  Classification of icode operators by general class
    4.  Machine type checking for simple operators
}
procedure initoptab;
const N = -1;                { indicates variable arg count }
var i: byte;                    { for clearing optab }
{
    op -- called to initialize entries in optab
}
procedure op(code: byte; name: string6; class: iopclass; 
        jcode: string15; 
        count: longint;        { number of operands (-1 for variable)}
        result, arg1, arg2, arg3, arg4, arg5: machinetype);
begin
    with optab[code] do begin        { using indicated entry }
    opname := name;            { set name }
    opclass := class;        { class of icode operator }
    opjcode := jcode;        { equivalent jcode or '?' if special }
    opcount := count;        { count of operands }
    opresult := result;        { machine type of result }
    opargs[1] := arg1;        { machine type of arg 1 of op }
    opargs[2] := arg2;        { machine type of arg 2 of op }
    opargs[3] := arg3;        { machine type of arg 3 of op }
    opargs[4] := arg4;        { machine type of arg 4 of op }
    opargs[5] := arg5;        { machine type of arg 5 of op }
    end;
end {op};
begin {initoptab}
    for i := 0 to 255 do            { default operator entry }
op(i, 'error',        nonoi,  '-----------', 0,xxx,xxx,xxx,xxx,xxx,xxx); 

{  icode print        class    jcode           n out op1 op2 op3 op4 op5 }
op(xchop,'xch',        nonoi,    '-----------', 0,xxx,xxx,xxx,xxx,xxx,xxx);
op(delop,'del',        nonoi,    '-----------', 0,xxx,xxx,xxx,xxx,xxx,xxx);
op(fixop,'fix',        nonoi,    '-----------', 0,xxx,xxx,xxx,xxx,xxx,xxx);
op(monitop,'monit',    nonoi,    '-----------', 0,xxx,xxx,xxx,xxx,xxx,xxx);
op(identop,'ident',    nonoi,    '-----------', 0,xxx,xxx,xxx,xxx,xxx,xxx);
op(procop,'proc',    nonoi,    '-----------', 0,xxx,xxx,xxx,xxx,xxx,xxx);
op(endop,'end',        nonoi,    '-----------', 0,xxx,xxx,xxx,xxx,xxx,xxx);
op(nullop,'null',    stmti,    '-----------', 0,xxx,xxx,xxx,xxx,xxx,xxx);
op(referop,'refer',    slcti,    '-----------', 1,pnt,data,xxx,xxx,xxx,xxx);
op(stolop,'stol',    stmti,    '-----------', 2,xxx,data,i16,xxx,xxx,xxx);
op(storop,'stor',    nonoi,    '-----------', 0,xxx,xxx,xxx,xxx,xxx,xxx);
op(stofop,'stof',    stmti,    '-----------', 2,xxx,data,f16,xxx,xxx,xxx);
op(succop,'succ',    expri,    'succ!      ', 1,ibig,i16,xxx,xxx,xxx,xxx);
op(predop,'pred',    expri,    'pred!      ', 1,ibig,i16,xxx,xxx,xxx,xxx);
op(uceqop,'uceq',    expri,    'equal!     ', 2,b1,u15,u15,xxx,xxx,xxx);
op(ucneop,'ucne',    expri,    'notequal!  ', 2,b1,u15,u15,xxx,xxx,xxx);
op(ucgtop,'ucgt',    expri,    'gti!       ', 2,b1,u15,u15,xxx,xxx,xxx);
op(ucleop,'ucle',    expri,    'lei!       ', 2,b1,u15,u15,xxx,xxx,xxx);
op(ucgeop,'ucge',    expri,    'gei!       ', 2,b1,u15,u15,xxx,xxx,xxx);
op(ucltop,'uclt',    expri,    'lti!       ', 2,b1,u15,u15,xxx,xxx,xxx);
op(umaxop,'umax',    expri,    'maxi!      ', 2,u15,u15,u15,xxx,xxx,xxx);
op(uminop,'umin',    expri,    'mini!      ', 2,u15,u15,u15,xxx,xxx,xxx);
op(iaddop,'iadd',    expri,    'addi!      ', 2,ibig,i16,i16,xxx,xxx,xxx);
op(isubop,'isub',    expri,    'subi!      ', 2,ibig,i16,i16,xxx,xxx,xxx);
op(imulop,'imul',    expri,    'muli!      ', 2,ibig,i16,i16,xxx,xxx,xxx);
op(idivop,'idiv',    expri,    'divi!      ', 2,i16,i16,ni16,xxx,xxx,xxx);
op(imodop,'imod',    expri,    'mod!       ', 2,i16,i16,ni16,xxx,xxx,xxx);
op(inegop,'ineg',    expri,    'negi!      ', 1,ibig,i16,xxx,xxx,xxx,xxx);
op(iabsop,'iabs',    expri,    '?          ', 1,ibig,i16,xxx,xxx,xxx,xxx);
op(ioddop,'iodd',    expri,    'odd!       ', 1,b1,i16,xxx,xxx,xxx,xxx);
op(ceilop,'ceil',    expri,    '?          ', 1,i16,i16,xxx,xxx,xxx,xxx);
op(floorop,'floor',    expri,    '?          ', 1,i16,i16,xxx,xxx,xxx,xxx);
op(saddop,'sadd',    expri,    'addf!      ', 2,f16,f16,f16,xxx,xxx,xxx);
op(ssubop,'ssub',    expri,    'subf!      ', 2,f16,f16,f16,xxx,xxx,xxx);
op(smulop,'smul',    expri,    'mulf!      ', 2,f16,f16,f16,xxx,xxx,xxx);
op(sdivop,'sdiv',    expri,    'divf!      ', 2,f16,f16,f16,xxx,xxx,xxx);
op(resclop,'rescl',    expri,    '?          ', 2,unp,unp,unp,xxx,xxx,xxx);
op(iceqop,'iceq',    expri,    'equal!     ', 2,b1,i16,i16,xxx,xxx,xxx);
op(icneop,'icne',    expri,    'notequal!  ', 2,b1,i16,i16,xxx,xxx,xxx);
op(icgtop,'icgt',    expri,    'gti!       ', 2,b1,i16,i16,xxx,xxx,xxx);
op(icleop,'icle',    expri,    'lei!       ', 2,b1,i16,i16,xxx,xxx,xxx);
op(icgeop,'icge',    expri,    'gei!       ', 2,b1,i16,i16,xxx,xxx,xxx);
op(icltop,'iclt',    expri,    'lti!       ', 2,b1,i16,i16,xxx,xxx,xxx);
op(imaxop,'imax',    expri,    'maxi!      ', 2,i16,i16,i16,xxx,xxx,xxx);
op(iminop,'imin',    expri,    'mini!      ', 2,i16,i16,i16,xxx,xxx,xxx);
op(faddop,'fadd',    nonoi,    '-----------', 0,xxx,xxx,xxx,xxx,xxx,xxx);
op(fsubop,'fsub',    nonoi,    '-----------', 0,xxx,xxx,xxx,xxx,xxx,xxx);
op(fmulop,'fmul',    nonoi,    '-----------', 0,xxx,xxx,xxx,xxx,xxx,xxx);
op(fdivop,'fdiv',    nonoi,    '-----------', 0,xxx,xxx,xxx,xxx,xxx,xxx);
op(fnegop,'fneg',    nonoi,    '-----------', 0,xxx,xxx,xxx,xxx,xxx,xxx);
op(fabsop,'fabs',    nonoi,    '-----------', 0,xxx,xxx,xxx,xxx,xxx,xxx);
op(floatop,'float',    nonoi,    '-----------', 0,xxx,xxx,xxx,xxx,xxx,xxx);
op(truncop,'trunc',    nonoi,    '-----------', 0,xxx,xxx,xxx,xxx,xxx,xxx);
op(roundop,'round',    nonoi,    '-----------', 0,xxx,xxx,xxx,xxx,xxx,xxx);
op(fxeqop,'fxeq',    expri,    'equal!     ', 2,b1,f16,f16,xxx,xxx,xxx);
op(fxneop,'fxne',    expri,    'notequal!  ', 2,b1,f16,f16,xxx,xxx,xxx);
op(fxgtop,'fxgt',    expri,    'gtf!       ', 2,b1,f16,f16,xxx,xxx,xxx);
op(fxleop,'fxle',    expri,    'lef!       ', 2,b1,f16,f16,xxx,xxx,xxx);
op(fxgeop,'fxge',    expri,    'gef!       ', 2,b1,f16,f16,xxx,xxx,xxx);
op(fxltop,'fxlt',    expri,    'ltf!       ', 2,b1,f16,f16,xxx,xxx,xxx);
op(fxmaxop,'fxmax',    expri,    'maxf!      ', 2,f16,f16,f16,xxx,xxx,xxx);
op(fxminop,'fxmin',    expri,    'minf!      ', 2,f16,f16,f16,xxx,xxx,xxx);
op(fceqop,'fceq',    nonoi,    '-----------', 0,xxx,xxx,xxx,xxx,xxx,xxx);
op(fcneop,'fcne',    nonoi,    '-----------', 0,xxx,xxx,xxx,xxx,xxx,xxx);
op(fcgtop,'fcgt',    nonoi,    '-----------', 0,xxx,xxx,xxx,xxx,xxx,xxx);
op(fcleop,'fcle',    nonoi,    '-----------', 0,xxx,xxx,xxx,xxx,xxx,xxx);
op(fcgeop,'fcge',    nonoi,    '-----------', 0,xxx,xxx,xxx,xxx,xxx,xxx);
op(fcltop,'fclt',    nonoi,    '-----------', 0,xxx,xxx,xxx,xxx,xxx,xxx);
op(fmaxop,'fmax',    nonoi,    '-----------', 0,xxx,xxx,xxx,xxx,xxx,xxx);
op(fminop,'fmin',    nonoi,    '-----------', 0,xxx,xxx,xxx,xxx,xxx,xxx);
op(notop,'not',        expri,    'not!       ', 1,b1,b1,xxx,xxx,xxx,xxx);
op(eqvop,'eqv',        expri,    'equal!     ', 2,b1,b1,b1,xxx,xxx,xxx);
op(xorop,'xor',        expri,    'xor!       ', 2,b1,b1,b1,xxx,xxx,xxx);
op(nimpop,'nimp',    expri,    'nimp!      ', 2,b1,b1,b1,xxx,xxx,xxx);
op(rimpop,'rimp',    expri,    'rimp!      ', 2,b1,b1,b1,xxx,xxx,xxx);
op(impop,'imp',        expri,    'implies!   ', 2,b1,b1,b1,xxx,xxx,xxx);
op(nrimpop,'nrimp',    expri,    'nrimp!     ', 2,b1,b1,b1,xxx,xxx,xxx);
op(orop,'or',        expri,    'or!        ', 2,b1,b1,b1,xxx,xxx,xxx);
op(andop,'and',        expri,    'and!       ', 2,b1,b1,b1,xxx,xxx,xxx);
op(complop,'compl',    expri,    '?          ', 1,s16,s16,xxx,xxx,xxx,xxx);
op(unionop,'union',    expri,    'union!     ', 2,s16,s16,s16,xxx,xxx,xxx);
op(interop,'inter',    expri,    'intersect! ', 2,s16,s16,s16,xxx,xxx,xxx);
op(sdiffop,'sdiff',    expri,    'diff!      ', 2,s16,s16,s16,xxx,xxx,xxx);
op(sgensop,'sgens',    expri,    '?          ', 2,unp,unp,unp,xxx,xxx,xxx);
op(sadelop,'sadel',    expri,    '?          ', 2,unp,unp,unp,xxx,xxx,xxx);
op(emptyop,'empty',    expri,    'empty!     ', 0,s16,xxx,xxx,xxx,xxx,xxx);
op(sceqop,'sceq',    expri,    'equal!     ', 2,b1,s16,s16,xxx,xxx,xxx);
op(scneop,'scne',    expri,    'notequal!  ', 2,b1,s16,s16,xxx,xxx,xxx);
op(scgtop,'scgt',    nonoi,    '-----------', 0,xxx,xxx,xxx,xxx,xxx,xxx);
op(scleop,'scle',    expri,    'subset!    ', 2,b1,s16,s16,xxx,xxx,xxx);
op(scgeop,'scge',    expri,    'superset!  ', 2,b1,s16,s16,xxx,xxx,xxx);
op(scltop,'sclt',    nonoi,    '-----------', 0,xxx,xxx,xxx,xxx,xxx,xxx);
op(inop,'in',        expri,    'in!        ', 2,b1,u4,s16,xxx,xxx,xxx);
op(sanyop,'sany',    nonoi,    '-----------', 0,xxx,xxx,xxx,xxx,xxx,xxx);
op(fieldop,'field',    slcti,    '-----------', 1,data,addr,xxx,xxx,xxx,xxx);
op(ofsetop,'ofset',    slcti,    '-----------', 1,data,addr,xxx,xxx,xxx,xxx);
op(indirop,'indir',    slcti,    '-----------', 1,data,pnt,xxx,xxx,xxx,xxx);
op(indexop,'index',    slcti,    '-----------', 2,data,addr,u15,xxx,xxx,xxx);
op(movemop,'movem',    stmti,    '-----------', 2,xxx,addr,addr,xxx,xxx,xxx);
op(invokop,'invok',    nonoi,    '-----------', N,xxx,xxx,xxx,xxx,xxx,xxx);
op(rtempop,'rtemp',    slcti,  '-----------', 0,pnt,xxx,xxx,xxx,xxx,xxx);
op(dtempop,'dtemp',    stmti,  '-----------', 2,xxx,ind,xxx,xxx,xxx,xxx);
op(ifop,'if',        stmti,    '-----------', 3,xxx,b1,xxx,xxx,xxx,xxx);
op(caseop,'case',    stmti,    '-----------', N,xxx,i16,xxx,xxx,xxx,xxx);
op(entryop,'entry',    stmti,    '-----------', N,xxx,xxx,xxx,xxx,xxx,xxx);
op(loopop,'loop',    stmti,    '-----------', N,xxx,xxx,xxx,xxx,xxx,xxx);
op(exitop,'exit',    stmti,    '-----------', 2,xxx,b1,xxx,xxx,xxx,xxx);
op(forop,'for',        stmti,    '-----------', 5,xxx,data,i16,i8,i16,xxx);
op(seqop,'seq',        stmti,    '-----------', N,xxx,xxx,xxx,xxx,xxx,xxx);
op(waitop,'wait',    stmti,    '-----------', 1,xxx,ind,xxx,xxx,xxx,xxx);
op(sendop,'send',    stmti,    '-----------', 1,xxx,ind,xxx,xxx,xxx,xxx);
op(tsigop,'tsig',    expri,    '?          ', 1,b1,ind,xxx,xxx,xxx,xxx);
op(lockop,'lock',    expri,  '?          ', 1,data,data,xxx,xxx,xxx,xxx);
op(enablop,'enabl',    nonoi,  '-----------', 0,xxx,xxx,xxx,xxx,xxx,xxx);
op(litscop,'litsc',    litri,    '?          ', 0,s16,xxx,xxx,xxx,xxx,xxx);
op(literop,'liter',    litri,    '?          ', 0,i16,xxx,xxx,xxx,xxx,xxx);
op(rdataop,'rdata',    slcti,    '?          ', 0,data,xxx,xxx,xxx,xxx,xxx);
op(litdop,'litd',    litri,    '?          ', 0,i16,xxx,xxx,xxx,xxx,xxx);
op(raiseop,'raise',    stmti,    '-----------', 1,xxx,xxx,xxx,xxx,xxx,xxx);
op(vceqop,'vceq',    expri,    '...........', 2,b1,unp,unp,xxx,xxx,xxx);
op(vcneop,'vcne',    expri,    '...........', 2,b1,unp,unp,xxx,xxx,xxx);
op(vcgtop,'vcgt',    expri,    '...........', 2,b1,unp,unp,xxx,xxx,xxx);
op(vcleop,'vcle',    expri,    '...........', 2,b1,unp,unp,xxx,xxx,xxx);
op(vcgeop,'vcge',    expri,    '...........', 2,b1,unp,unp,xxx,xxx,xxx);
op(vcltop,'vclt',    expri,    '...........', 2,b1,unp,unp,xxx,xxx,xxx);
op(dvadop,'dvad',    slcti,    '-----------', 0,data,xxx,xxx,xxx,xxx,xxx);
op(varblop,'varbl',    slcti,    '-----------', 0,data,xxx,xxx,xxx,xxx,xxx);
op(paramop,'param',    slcti,    '-----------', 0,data,xxx,xxx,xxx,xxx,xxx);
op(callop,'call',    stmti,    '?          ', N,xxx,xxx,xxx,xxx,xxx,xxx);
op(icallop,'icall',    stmti,    '?          ', 0,xxx,xxx,xxx,xxx,xxx,xxx);
op(fcallop,'fcall',    expri,  '?          ', 1,data,xxx,xxx,xxx,xxx,xxx);
op(defarop,'defar',    expri,  '?          ', 3,b1,data,i16,i16,xxx,xxx);
op(vinitop,'vinit',    expri,  '?          ', 0,b1,xxx,xxx,xxx,xxx,xxx);
op(measop,'meas',    stmti,  '-----------', 1,xxx,u15,xxx,xxx,xxx,xxx);
op(depthop,'depth',    stmti,  '-----------', 1,xxx,u15,xxx,xxx,xxx,xxx);
op(defndop,'defnd',    expri,  '?          ', 1,b1,data,xxx,xxx,xxx,xxx);
op(oldop,  'old',    slcti,  '?          ', 1,data,data,xxx,xxx,xxx,xxx);
op(vdeclop,'vdecl',    decli,    '?          ', 1,xxx,b1, xxx,xxx,xxx,xxx);
op(vheadop,'vhead',    decli,    '-----------', 2,xxx,xxx,xxx,xxx,xxx,xxx);
op(asertop,'asert',    stmti,    '-----------', N,xxx,b1, b1, b1, b1, b1);
op(linenop,'linen',    nonoi,    '-----------', 0,xxx,xxx,xxx,xxx,xxx,xxx);
op(signlop,'signl',    slcti,  '-----------', 1,data,sig,xxx,xxx,xxx,xxx);
op(isgnlop,'isgnl',    decli,  '-----------', N,xxx,sig,sig,sig,sig,sig);
op(blockop,'block',    decli,  '-----------', N,xxx,xxx,xxx,xxx,xxx,xxx);
op(xhndlop,'xhndl',    decli,  '-----------', N,xxx,xxx,xxx,xxx,xxx,xxx);
end {initoptab};
{
    initmoptab  --  construct table used in printing expressions
            in user-readable form.
}
procedure initmoptab;
{
    mop  --  insert one entry in table

    Entries contain the icode opcode, the string to be printed, and
    the precedence for infix operators.  Functional operators have
    a precedence of "functional".
}
var i: byte;                    { for loop }
procedure mop(code: byte;            { icode code }
          s: string15;            { printable form }
          pre: precedence);            { precedence }
begin
    with optab[code] do begin            { using optab }
    opmcode := s;                { message form }
    opmpred := pre;                { message precedence }
    end;
end {mop};
begin {initmoptab}
    for i := 0 to 255 do            { fill with error info }
        mop(i, 'INVALIDOPCODE', functional);    { fill table }

{       opcode   name   precedence    }
    mop(succop, 'succ', functional);
    mop(predop, 'pred', functional);
    mop(uceqop, '= ', relationaloperator);
    mop(ucneop, '<>', relationaloperator);
    mop(ucgtop, '> ', relationaloperator);
    mop(ucleop, '<=', relationaloperator);
    mop(ucgeop, '>=', relationaloperator);
    mop(ucltop, '< ', relationaloperator);
    mop(umaxop, 'max', functional);
    mop(uminop, 'min', functional);
    mop(iaddop, '+ ', addingoperator);
    mop(isubop, '- ', addingoperator);
    mop(imulop, '* ', multiplyingoperator);
    mop(idivop, 'div', multiplyingoperator);
    mop(imodop, 'mod', multiplyingoperator);
    mop(inegop, '- ', negationoperator);
    mop(iabsop, 'abs', functional);
    mop(ioddop, 'odd', functional);
    mop(ceilop, 'ceil', functional);
    mop(floorop, 'floor', functional);
    mop(saddop, '+ ', addingoperator);
    mop(ssubop, '- ', addingoperator);
    mop(smulop, '* ', multiplyingoperator);
    mop(sdivop, 'div', multiplyingoperator);
    mop(resclop, 'rescale', functional);
    mop(iceqop, '= ', relationaloperator);
    mop(icneop, '<>', relationaloperator);
    mop(icgtop, '> ', relationaloperator);
    mop(icleop, '<=', relationaloperator);
    mop(icgeop, '>=', relationaloperator);
    mop(icltop, '< ', relationaloperator);
    mop(imaxop, 'max', functional);
    mop(iminop, 'min', functional);
    mop(fxeqop, '= ', relationaloperator);
    mop(fxneop, '<>', relationaloperator);
    mop(fxgtop, '> ', relationaloperator);
    mop(fxleop, '<=', relationaloperator);
    mop(fxgeop, '>=', relationaloperator);
    mop(fxltop, '< ', relationaloperator);
    mop(fxmaxop, 'max', functional);
    mop(fxminop, 'min', functional);
    mop(notop, 'not', negationoperator);
    mop(eqvop, '= ', relationaloperator);
    mop(xorop, 'xor', addingoperator);
    mop(nimpop, 'notimplies', functional);
    mop(rimpop, 'reverseimplies', functional);
    mop(impop, 'implies', relationaloperator);
    mop(nrimpop, 'notrevimplies', functional);
    mop(orop, 'or', addingoperator);
    mop(andop, 'and', multiplyingoperator);
    mop(complop, 'complement', functional);
    mop(unionop, 'union', functional);
    mop(interop, 'intersection', functional);
    mop(sdiffop, 'diff', functional);
    mop(sgensop, 'setgenerator', functional);
    mop(sadelop, 'setdeletor', functional);
    mop(emptyop, 'empty', functional);
    mop(sceqop, '= ', relationaloperator);
    mop(scneop, '<>', relationaloperator);
    mop(scleop, 'subset', functional);
    mop(scgeop, 'superset', functional);
    mop(inop, 'in', relationaloperator);
    mop(tsigop, '? ', functional);
    mop(lockop, '? ', functional);
    mop(vceqop, '= ', relationaloperator);
    mop(vcneop, '<>', relationaloperator);
    mop(vcgtop, '> ', relationaloperator);
    mop(vcleop, '< ', relationaloperator);
    mop(vcgeop, '>=', relationaloperator);
    mop(vcltop, '<=', relationaloperator);
    mop(vinitop, 'defined', functional);
    mop(defndop, 'defined', functional);
    mop(defarop, 'defined', functional);
end {initmoptab};
{
    inittables  --  initialize static tables
}
procedure inittables;
begin
    initmttab;                { mttab array }
    initoptab;                { optab array }
    initmoptab;                { remainder of moptab array }
end { inittables };
