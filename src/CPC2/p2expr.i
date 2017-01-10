procedure WHATexpr; const WHAT = '@(#)p2expr.i    2.8'; begin SINK := WHAT; end;    { Version 2.8 of 1/14/83 }
{
    expression generation - Jcode and messages
}
{
    genliteral  --  generate literal constant   

    FIXED-POINT NOT IMPLEMENTED
}
procedure genliteral(p: ptn);            { literal to generate }
begin
    with p^ do begin                { using given node }
    if not (code in [literop]) then verybadnode(p,134); { not a literal }
    case code of                { handle different codes }
    literop: begin                { constant }
    case mttab[mtype].mtkind of        { kinds of constants }
        booleandata: begin            { boolean data }
            case disp of            { boolean cases }
                0: genstring15('(false!)');    { 0 is false }
                1: genstring15('(true!)');    { 1 is true }
                end;
            end;                { end booleandata }
        numericdata: begin            { numeric data }
            genintconst(disp);        { (consti! n) }
            end;
        setdata: begin            { set constant }
        unimplemented(linen);        { ***UNIMPLEMENTED*** }
        end;    
        fixeddata: begin            { fixed point constant }
        unimplemented(linen);        { ***UNIMPLEMENTED*** }
        end;
        end;                { end literal classes }
    end;                    { end literop }
    end;                    { of cases }
    end;                    { of WITH }
end {genliteral};
{
    genjexpr  --  generate jcode expression
}
procedure genjexpr(p: ptn);          { FORWARD resolution }
var iclass: iopclass;                { icode op class }
    i: 0..maxarg;                { for arg loop }
{
    genspecialjexpr  --  generate non one-to-one jcode ops
}
procedure genspecialjexpr(p: ptn);
begin
    with p^ do begin                { using given node }
        if code in [defndop,defarop,vinitop] then begin    { if known }
        case code of             { fan out }
        defndop: begin            { defined(<selector>) }
        if arg[1]^.vtype^.down = nil then begin { if leaf of type }
            gendefselector(arg[1]);    { gen selector as defined }
        end else begin            { if structured object }
            genstring15('(alltrue!');    { (alltrue! object) }
            genspace;
            gendefselector(arg[1]);    { gen selector as defined }
            genchar(')');        { finish alltrue }
            end;
        end;
        defarop: begin            { defined(<array> <lo> <hi>) }
        genstring15('(arraytrue!');    { (arraytrue! args ) }
        genspace;
        gendefselector(arg[1]);        { selector as def }
        genspace;
        genjexpr(arg[2]);        { low bound }
        genspace;
        genjexpr(arg[3]);        { high bound }
        genchar(')');            { close arraytrue }
        end;
        vinitop: begin            { defined(<modulename>) }
        gendataref(p^.linen,p^.vtype,false,genwithdef);
        end;
        end;                { end of cases }
    end else begin                { if unknown op }
            genstring15('<unimplemented>');    { ***TEMP*** }
        unimplemented(p^.linen);        { ***UNIMPLEMENTED*** }
        end;
    end;                    { end With }
end {genspecialjexpr};    
{
    genfunctioncall  --  generate function call 

    Uninterpreted function calls are generated here, unless the
    result of the function has already been computed and is associated
    with the icode node.  All real functions (other than ones intended
    for rule builder use only) are evaluated in safeexpr and the value
     associated with the icode node.
}
procedure genfunctioncall(p: ptn);
var i: 0..maxarg;                { for arg loop }
    callee: blocknodep;                { called block }
begin
    assert(p^.code = fcallop);            { must be fcall node }
    if p^.ndfntemp.ftstamp = clockserial then begin { if has tagged temp }
                        { (TEMPnn) }
    gendataid(nil,p^.ndfntemp.fttemp,genwithoutnew, genwithoutdef);
    end else begin                { no tagged temp }
    with p^.arg[1]^ do begin        { using subnode }
        assert(code = callop);        { must be call }
        callee := vtype^.blockdata;        { get called block }
        genchar('(');            { uninterpreted fn call }
        genname(vtype);            { name of function }
        for i := 1 to nrarg do begin    { for each arg }
        genspace;            { space between args }   
        genjexpr(arg[i]);        { generate arg value }
        end;
        if not (callee^.blfnkind in 
        [rulefunction, purefunction])
                        { ***TEMP*** SAFE fn not OK yet}
        then begin
        usererrorstart(p^.linen);    { uninterpreted fn is impure}
        write(output,'Cannot call ');
        diagblockname(callee);        { name of routine }
        write(output,' which ');
        diagblockkind(callee);        { kind of routine }
        write(output,' here.');    
        usererrorend;            { finish message }
        end;                { end impure fn in assert }
        end;                { of With }
        genchar(')');            { finish uninterpreted fn }
    end;                    { no tagged temp }
end {genfunctioncall};
begin { genjexpr }
    with p^ do begin                { using given node }
    if code = fcallop then begin        { if call }
        genfunctioncall(p);            { generate function call }
        assert(arg[1]^.code = callop);    { must be call below }
        prioritycheck(p,lastblockp^.blpriority,arg[1]^.vtype^.blockdata);
    end else if code = lockop then begin    { if lock for call }
        if arg[1]^.code <> fcallop then     { must be fncall below }
        verybadnode(p,363);         { lock of non fcall }
        genfunctioncall(arg[1]);        { gen fn call }
        assert(arg[1]^.arg[1]^.code = callop); { must be call below }
        prioritycheck(p,disp,arg[1]^.arg[1]^.vtype^.blockdata);
    end else begin                { non-call }
        iclass := optab[code].opclass;    { get class of opcode }
        if not (iclass in [litri, expri,slcti]) then { if bad node in expr }
            badnode(p,104)            { wrong class of node in expr }
        else case iclass of            { kinds of nodes }
        litri: begin            { literal }
            genliteral(p);        { generates literal }
            end;
            slcti: begin            { selector }
            genjselector(p);        { generate selector expr }
            end;
            expri: begin            { expression }
            with optab[code] do begin    { using optab }
                assert(opjcode[1] <> '-');    { table entry unusable }
                if opjcode[1] = '?' then begin { special operators }
                genspecialjexpr(p);    { handle special cases }
                end else begin        { if general case }
                genchar('(');    { open expression }
                genstring15(opjcode);    { emit operator }
                for i := 1 to nrarg do begin { for args }
                    genspace;    { space before arg }
                        { gen subexpression }
                    genjexpr(arg[i]);  
                    end;        { end arg loop }
                genchar(')');    { finish expression }
                end;        { end general case }
                end;            { end WITH optab }
            end;            { end expri case }
            end;                { end cases }
        end;                { end WITH }
    end;                    { end non-special }
end {genjexpr};
{
    genmliteral  --  generate literal constant in printable form

    FIXED-POINT NOT IMPLEMENTED
}
procedure genmliteral(p: ptn);            { literal to generate }
var val: longint;                { working set value }
    needcomma: boolean;                { comma needed }
    i: 0..15;                    { for loop }
begin
    with p^ do begin                { using given node }
    if not (code in [literop]) then verybadnode(p,130); { not a literal }
    case code of                { handle different codes }
    literop: begin                { constant }
    case mttab[mtype].mtkind of        { kinds of constants }
        booleandata: begin            { boolean data }
            case disp of            { boolean cases }
                0: genstring15('false');    { 0 is false }
                1: genstring15('true');    { 1 is true }
                end;
            end;                { end booleandata }
        numericdata: begin            { numeric data }
            geninteger(disp);        { n }
            end;
        setdata: begin            { set data }
        { NOTE: all sets are assumed to start at 0 }
        genchar('[');            { begin set constant }
        assert(mtype = s16);        { must be 16 bit set }
        val := disp;            { get value of constant }
        needcomma := false;        { no comma before first elt }
        for i := 0 to 15 do begin    { for possible values }
            if (val mod 2) <> 0 then begin { if this bit set }
            if needcomma then genchar(','); { separator }
            needcomma := true;    { cause comma next time }
            geninteger(i);        { generate this const item }
            end;
            val := val div 2;    { strip off bit }
            end;
        genchar(']');            { end set constant }
        end;
        end;                { end literal classes }
    end;                    { end literop }
    end;                    { of cases }
    end;                    { of WITH }
end {genmliteral};
{
    genmselector  --  generate selector as message

    Generates "a[i].b", etc. for messages
}
procedure genmselector(p: ptn);            { expression }
var stk: selstack;                { working selector stack }
    i: 1..maxselstack;                { position in stack }
    lowbound: targetinteger;            { lower bound of array }
begin
    buildselstack(p, stk);            { build selector stack }
    for i := stk.top downto 1 do begin        { starting from variable }
    with stk.tab[i] do             { for each selector part }
    case stkind of                { fan out on operator }
    variablesel: begin            { variable }
        genstring15(stvar^.vardata.itemname); { name of variable }
        if stold then            { if .old }
        genstring15('.old');        { so note }
        end;

    arraysel: begin                { array reference }
        genchar('[');            { begin ref }
                        { machine arrays start at 0 }
        lowbound := stvar^.up^.vardata.minvalue; { array low bound }
        if lowbound <> 0 then begin        { if not start from 0 }
        genmexpr1(stsub,addingoperator);{ main part of subscript }
        genchar(' ');
        if lowbound > 0 then         { if positive bias }
            genchar('+')        { undo subtract }
        else
            genchar('-');        { undo add }
        genchar(' ');
        geninteger(abs(lowbound));    { bias }
        end else begin            { if start from 0 }
            genmexpr(stsub);        { subscript expression }
        end;
        genchar(']');            { end ref }
        end;

    recordsel: begin            { record reference }
        genchar('.');            { proper delimiter }
        genstring15(stvar^.vardata.itemname); { field name }
        end;
    end;                    { of cases and with }
    end;                    { of for }
end {genmselector};
{
    genmfunctioncall  --  generate function call in message
}
procedure genmfunctioncall(p: ptn);
var i: cardinal;                { for loop }
begin
    with p^ do begin                { using given node }
    assert(code = fcallop);            { must be fcall }
    p := p^.arg[1];                { get call node }
    end;
    with p^ do begin                { using given node }
    assert(code = callop);            { must be call node }
    genstring15(vtype^.vardata.itemname);    { name of function }
    genchar('(');                { begin arg list }
    for i := 1 to nrarg do begin        { for all args }
        if i > 1 then genchar(',');        { arg separator }
        genmexpr(arg[i]);            { actual arg }
        end;
    genchar(')');                { end arg list }
    end;
end {genmfunctioncall};
{
    genmvinit  --  special case for vinit operator
}
procedure genmvinit(p: ptn);
begin
    genstring15('defined(');            { defined(<modulename>) }
    genstring15(p^.vtype^.vardata.itemname);    { name of module }
    genchar(')');                { finish }
end {genmvinit};
{
    genmexpr1  --  internal recursive generator
}
procedure genmexpr1(p: ptn;             { node }
            callerpred: precedence);    { precedence of caller }
                                        { FORWARD resolution }
var iclass: iopclass;                { icode op class }
    i: cardinal;                { for arg loop }
begin { genmexpr1 }
    with p^ do begin                { using given node }
    if code = fcallop then begin        { if call }
        genmfunctioncall(p);        { generate function call }
    end else if code = vinitop then begin    { if initialized-module test }
        genmvinit(p);            { generate said test }
    end else begin                { non-call }
        iclass := optab[code].opclass;    { get class of opcode }
        if not (iclass in [litri, expri,slcti]) then { if bad node in expr }
            badnode(p,135)            { wrong class of node in expr }
        else case iclass of            { kinds of nodes }
        litri: begin            { literal }
            genmliteral(p);        { generates literal }
            end;
            slcti: begin            { selector }
            genmselector(p);        { generate selector expr }
            end;
            expri: begin            { expression }
            with optab[code] do begin    { using optab }
            if opmpred = functional then begin { if function-like }
                genstring15(opmcode); { function name }
                genchar('(');    { begin arg list }
                for i := 1 to nrarg do begin { for each arg }
                if i > 1 then     { if not first arg }
                    genchar(','); { separate }
                genmexpr(arg[i]); { subexpression }
                end;
                genchar(')');    { close function call }
            end else begin        { if infix operator }
                if callerpred <= opmpred then begin { if pred drop }
                genchar('(');    { enclose in parens }
                end;
                assert(nrarg in [1,2]); { 1 or 2 args only }
                case nrarg of    { number of args }
                1: begin        { one arg }
                genstring15(opmcode); { operator }
                genchar(' ');
                genmexpr1(arg[1], opmpred); { operand }
                end;
                2: begin        { binary operator }
                genmexpr1(arg[1], opmpred); { left operand }
                genchar(' ');
                genstring15(opmcode); { operator }
                genchar(' ');
                genmexpr1(arg[2], opmpred); { right operand }
                end;
                end;        { end of cases }
                if callerpred <= opmpred then begin { if pred drop }
                genchar(')');    { enclose in parens }
                end;
                end;        { end infix operator }
                end;            { end WITH optab }
            end;            { end expri case }
            end;                { end cases }
        end;                { end WITH }
    end;                    { end non-special }
end {genmexpr1};
{
    genmexpr  --  generate message expression

    Used to generate expressions in REQUIRE statement messages
}
procedure genmexpr(p: ptn);                { FORWARD resolution }
begin {genmexpr}
    genmexpr1(p, functional);            { start generation }
end {genmexpr};
