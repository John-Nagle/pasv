procedure WHATbound; const WHAT = '@(#)p2bound.i    2.3'; begin SINK := WHAT; end;    { Version 2.3 of 1/14/83 }
{
    bounds checking
}
procedure functcall(p: ptn);    forward;    { forward def }
{
    gendevnew  --  temporary handling for devices

    For each reference to a device, we generate a NEW which causes
    no information to be available other than that implied by the
    type of the device variable.  This is a temporary implementation
    to be used until the implementation of EXTRA MONITORs.
}
procedure gendevnew(p: ptn);            { device variable to do }
var basev: varnodep;                { entire device var }
begin
    with p^ do begin                { using given node }
    assert(code = dvadop);            { must be a device }
    basev := basevariable(vtype);        { get entire device var }
    genstring15('NEW');            { NEW (device) (true) }
    genspace;
                        { (device) }
    gendataid(basev,nulltid, genwithoutnew, genwithoutdef);
    genspace;
    genstring15('(true!)');            { only available info }
    if comments then begin            { commentary }
        gencomment(p^.linen);        { line of dev ref }
        genstring15('Device read');
        end;
    genline;                { finish NEW }
    end;                    { With }
end {gendevnew};
{
    getnumericbounds  --  get the bounds of a mtype/vtype pair

    Bounds come from three sources:

    1.  The machine type (m) - from machine restrictions   
    2.  The variable type (v) - from source declaration.
    3.  The value (val) - only used for constants
    The mtype must be numeric, as must the vtype, if it exists.
}
procedure getnumericbounds(p: ptn;        { node for debug location }
               val: ptn;        { relevant icode node if any }
               v: varnodep;        { relevant varnode if any }
               m: machinetype;    { relevant machine type }
               var lo, hi: longint); { bounds returned }
begin
    with mttab[m] do begin            { using mtype }
    if mtkind <> numericdata then badnode(p,132); { mtype not numeric }
    lo := mtmin;                { get low bound }
    hi := mtmax;                { get high bound }
    end;
    if v <> nil then with v^ do begin        { using given node }
    if vardata.form <> numericdata then badnode(p,133); { vtype not number }
    lo := max(lo, vardata.minvalue);    { get most restrictive value }
    hi := min(hi, vardata.maxvalue);    { ditto }
    end;
    if val <> nil then                 { if value node }
    if val^.code = literop then begin    { if literal constant }
        lo := val^.disp;            { use constant value }
        hi := lo;                { both bounds are same }
        end;
end {getnumericbounds};    
{
    gendefined  --  generate DEFINED require for selector expression

    For complex structures, the predicate "alltrue" is generated,
    which tests a complex object for definedness.  Code in the
    theorem prover understands the meaning of alltrue! as a generic.
}
procedure gendefined(p: ptn);            { expression }
const def0impl = true;                { gen defined of base too }
begin
    assert(sbtop = 0);                { no substitutes in effect }
    genstring15('REQUIRE');            { REQUIRE (defined! <expr>) }
    genspace;
    genalldef(p);                { gen alldefined if needed }
    genspace;
    genmsgstart(p^.linen);            { begin message }
    genchar('"');                { enclose in quotes }
    genmexpr(p);                { <expression> is defined }
    genchar('"');                { enclose in quotes }
    genchar(' ');
    genstring15('is defined');            { message }
    genmsgend;                    { end message }
    genline;                    { end REQUIRE }
end {gendefined};
{
    editbound  --  edit forms of n..m
}
procedure editbound(v: varnodep);        { edit bounds of object }
begin
    with v^ do begin                { using varnode }
    geninteger(vardata.minvalue);        { minimum value }
    genstring15('..');            { .. }
    geninteger(vardata.maxvalue);        { maximum value }
    end;
end {editbound};
{
    explainbound  --  generate explaination for operator constraint

    Used to explain implicit REQUIREs
}
procedure explainbound(p: ptn);            { relevant operator }
const spaces = 4;                { spaces before msg }
var i: 1..spaces;
    boundobj: varnodep;                { object causing bounds }
begin
    for i := 1 to spaces do genchar(' ');    { space before explaination }
    genchar('(');                { enclose in parentheses }
    with p^ do begin                { using given node }
    if code in [indexop, stolop, movemop, stofop] then begin
        if code = indexop then begin     { if index }
            genstring15('subscript');
        boundobj := findarray(p);    { get bounds from array }
        boundobj := boundobj^.up;    { bounds of subscript not elt }
        end else begin            { if store-type operator }
        genstring15('range');        { if variable }
        boundobj := arg[1]^.vtype;    { get lhs variable }
        end;
        genstring15(' check for "');    { common message }
        genmexpr(arg[1]);            { lhs }
        genchar('"');
        genchar(' ');
        editbound(boundobj);        { bounds of object }
    end else if code = callop then begin    { if call }
        genstring15('input to "');    
        genstring15(vtype^.vardata.itemname); { callee }
        genchar('"');
    end else if code = forop then begin    { if FOR loop }
        genstring15('FOR loop count');
    end else if code = depthop then begin    { if DEPTH check }
        genstring15('DEPTH check');
    end else if code = measop then begin    { if MEASURE check }
        genstring15('MEASURE');
    end else begin                { operator overflow }
        genstring15('machine limit');
        genstring15(' for "');
        genstring15(optab[code].opmcode);    { printable form }
        genchar('"');            { finish }
        end;
    genchar(')');                { finish explaination }
    end;                    { end With }
end {explainbound};
{
    genboundrequire  --  generate require for bound test
}
procedure genboundrequire(p: ptn;        { expression }
              bound: longint;    { bound }
              op: byte;        { icode operator for compare }
              explainop: ptn);    { operator for explaination }
begin
    assert(op in [iceqop, icleop, icgeop, icneop, icltop, icgtop]);
    genstring15('REQUIRE');            { REQUIRE (op expr bound) }
    genspace;
    genchar('(');
    genstring15(optab[op].opjcode);        { compare operator }
    genspace;
    genjexpr(p);                { expression }
    genspace;
    genstring15('(consti!');            { longint constant }
    genspace;
    geninteger(bound);                { bound }
    genchar(')');                { close consti }
    genchar(')');                { close operator }
    genspace;
    genmsgstart(p^.linen);            { begin message }
    genmexpr1(p, relationaloperator);        { edit with right precedence }
    genchar(' ');
    genstring15(optab[op].opmcode);        { compare operator (infix) }
    genchar(' ');                { space before bound }
    geninteger(bound);                { longint }
    explainbound(explainop);            { explain source of bound }
    genmsgend;                    { end message }
    genline;                    { end REQUIRE }
end {genboundrequire};
{
    requirecompat  --  require that arg fit in given type
}
procedure requirecompat(p: ptn;            { node for debug location }
            destv: varnodep;    { destination vtype }
            destm: machinetype;    { destination mtype }
            source: ptn);        { source expression }
var errorpossible: boolean;            { true if any err possible }
    srcmin, srcmax, dstmin, dstmax: longint;     { bounds }
begin
    assert(source <> nil);            { source must exist }
    assert(p <> nil);                { diag node must exist }
    with source^ do begin            { using source node }
    errorpossible := destm <> mtype;    { do mtypes differ? }
    if destv <> nil then begin        { if destination has vtype }
        assert(destv^.varmtype = destm);    { vtype/mtype incompatible }
        if destv <> vtype then errorpossible := true; { possible error }
        end;
    if errorpossible then begin        { if serious test needed }
        if not mttab[destm].mtsimple then begin   { if not simple type }    
        if destv = nil then badnode(p,128); { non-simple no vtype }
        checkcompat(p,destv,vtype,false);    { check assignable }    
        end else begin            { if simple type }
        case mttab[destm].mtkind of    { fan out on kind }
        booleandata: begin        { booleans }
            if mtype <> b1 then     { if source not boolean }
            badnode(p,131);        { Boolean := non-boolean }
            end;
        fixeddata: begin        { fixed point }
            unimplemented(p^.linen);    { ***UNIMPLEMENTED*** }
            end;
        numericdata: begin        { if numeric dest }
                        { get destination bounds }
            getnumericbounds(p,nil, destv,destm,dstmin,dstmax);
                        { get source bounds }
            getnumericbounds(source,source,source^.vtype,source^.mtype,
            srcmin, srcmax);    
            if dstmin > srcmin then     { if underflow possible }
            genboundrequire(source,dstmin,icgeop,p); { gen REQUIRE }
            if dstmax < srcmax then     { if overflow possible }
            genboundrequire(source,dstmax,icleop,p); { gen REQUIRE }
            if not mttab[destm].mtzerook then { if zero disallowed }
            if (dstmax >= 0) and (dstmin <= 0) then { if 0 in range}
                genboundrequire(source,0,icneop, p); { gen require }
            end;            { end numeric data }
        setdata: begin            { set }
            unimplemented(p^.linen);    { ***UNIMPLEMENTED*** }
            end;
        end;                { end cases on kind }
        end;                { end simple type }
        end;                { end error possible }
    end;                    { end With }
end {requirecompat};
{
    Subscript checking

    At the icode level, all arrays start from zero.  Thus, the lower
    bound of an index is always zero, and the high bound is the
    high subscript limit minus the low subscript limit.
}
procedure subscriptcheck(p: ptn);        { index operator }
const lolim = 0;                { low index limit }
var losub, hisub, hilim: longint;        { bounds }
    arrayid: varnodep;                 { array item }
    subscript: ptn;                { working pointers }
begin
    with p^ do begin                { using given node }
    assert(code = indexop);            { only for index operator }
    arrayid := findarray(p);        { find relevant array elt }
    arrayid := arrayid^.up;            { get to array itself }
    subscript := arg[2];            { subscript }
    end;
    with arrayid^ do begin            { using type entry for array }
    assert(vardata.form = arraydata);    { must be array }
    hilim := vardata.maxvalue - vardata.minvalue; { high subscript bound}    
    end;
    if subscript^.mtype = b1 then begin        { if boolean subscript }
    losub := 0;                { special case for boolean }
    hisub := 1;                { treat as 0..1 }
    end else begin                { otherwise must be numeric }
        getnumericbounds(subscript, subscript, subscript^.vtype, 
    subscript^.mtype,
    losub, hisub);                { get bounds of subscript }
    end;
    if losub < lolim then begin            { if underflow possible }
    genboundrequire(subscript, lolim, icgeop, p);{ check for it }
    end;
    if hisub > hilim then begin            { if overflow possible }
    genboundrequire(subscript, hilim, icleop, p);{ check for it }
    end;
end {subscriptcheck};
{
    safeactuals  --  handle calls to user-defined functions
}
procedure safeactuals(p: ptn);            { call node }
var i: 1..maxarg;                { for loop }
    q: varnodep;                { argument being considered }
    parg: ptn;                    { pointer to arg being worked }
begin
    with p^ do begin                { using given node }
    assert(code = callop);            { must be a call }
    q := firstformal(vtype);        { get first formal arg }
    for i := 1 to nrarg do begin        { for all args given }
        if q = nil then badnode(p,46)     { too many args in call }
        else begin                { for this arg }
        parg := arg[i];            { get arg entry }
        case q^.vardata.by of        { kind of argument }
        byactualvalue: begin        { by value }
            safeexpr(parg);        { insure valid value expr }
                        { may catch user errors }
            requirecompat(p, q, q^.varmtype, parg); { check fits }
            end;
        byreference: begin        { by reference }
            if parg^.code <> referop then begin 
            badnode(p,53);        { ref arg not passed by ref }
            end else begin        { proper ref arg }
            if q^.vardata.form <> pointerdata then begin
                badnode(p,50);    { not pointer at ref arg }
                end;
            assert(q^.down <> nil);    { must be at bottom }
                        { must be valid selector } 
            safeselector(parg^.arg[1]); { valid selector }
                        { strictly a bad icode check }
            checkcompat(p,q^.down,parg^.arg[1]^.vtype,true);
            end;            { end proper ref arg }
            end;            { end by reference }
        end;                { end cases }    
        q := q^.right;            { advance to next arg }
        end;
        end;                { end arg loop }
    if q <> nil then badnode(p,47);        { too few args in call }
    end;                    { of with }
end {safeactuals};
{
    safeexpr  --  insure that valued expression is completely valid

    All operators are checked for machine overflow.
    Selector expressions are validated.
    Function calls are evaluated here.  This seems misplaced but
    we need to generate the jcode for all function calls before the
    expression containing the function is used.  TEMP names are
    associated with the function in the code tree during this process.

        A weak type check is performed on the machine types of
        expression parts.  For each operator, the optab table
        gives the type of the result and the type of the operands.
        When, in the tree, an operand type does not match the
        type of the operator below, a possible error exists.
        If the datakinds of the types do not match, a definite
        error has been detected.  If the datakinds match,
        but the types do not, it may be necessary to generate
        REQUIRE operators.  
}
procedure safeexpr(p: ptn);                { FORWARD resolution }
var i: 1..maxarg;                { arg loop }
    j: 1..5;                    { position in optab args }
    sideeffectinthisexpr: boolean;        { side effect found }
begin {safeexpr}
    assert(p <> nil);                { node must exist }
    sideeffectinthisexpr := false;        { no side effect seen yet }
    with p^ do begin                { using given node }
    with optab[code] do begin        { using relevant table }
        assert(opclass <> nonoi);        { checked in augment1 }
        case opclass of            { fan out on class of op }
        stmti: begin            { statement operator }
        badnode(p,306);            { statement inside expr }
        end;
        slcti: begin            { selector operator }
        if code = dvadop then begin    { if this is a device }
            gendevnew(p);        { has new value on each read }
            end;
        safeselector(p);        { selector must be OK }
        if not alwaysdefined(vtype)then    { if not inherently defined }
            gendefined(p);        { REQUIRE object be defined }
        end;
        litri, expri: begin            { general case }
            if code in             { special cases } 
        [lockop,fcallop]
        then begin
            case code of        { fan out }
            lockop: begin        { lock }
                safeexpr(arg[1]);    { insure safe }
                end;
            fcallop: begin        { function call }
            functcall(p);        { actually generate call here }
            with p^.arg[1]^.vtype^.blockdata^ do begin { callee }
                        { if call to general function}
                if blfnkind = generalroutine then 
                    sideeffectinthisexpr := true;{note side effects}
                        { if callee has output VAR }
                if blhasoutputvararg then
                    sideeffectinthisexpr := true;{note side effect }
                end;        { end callee with }
            end;
            end;            { end special cases }
            end else begin            { general case }
        assert(opclass in [litri, expri, slcti]); { must be expression }
        {
            For built-in operators with over five arguments,
            the last two entries in oparg are used over
            and over.  This mechanism applies to
                loop
                case
                seq
        }
            for i := 1 to nrarg do begin { using arg list }
                if i <= 5 then j := i     { if 1..5, i else 4 or 5 }
                  else j := (i mod 2) + 4;
                safeexpr(arg[i]);        { validate operand }
                        { check domain of operator }
                requirecompat(p, nil, opargs[j], arg[i]);
                end;
            end;            { end general case }
        end;                { end litri, expri }
        end;                { end cases }
        end;                { end WITH }
    {
        Preliminary, dumb, side effect detector

        Functions with side effects can only be called in
        expressions with no operators outside the function call.
    }
        if sideeffectinthisstmt then begin    { if side effect already }
        diag(linen,'Cannot use function with side effects in expression');
        sideeffectinthisstmt := false;    { avoid multiple diags }
        end;
        
    end;                    { end WITH }
                        { note side effects if any }
    sideeffectinthisstmt := sideeffectinthisstmt and sideeffectinthisexpr;
end {safeexpr};
{
    safeselector  --  is evaluation of selector expression permissible?
              Generates requires for subscripts.
              Does not insure that expression itself has a
              defined value.
}
procedure safeselector(p: ptn);                { FORWARD resolution }
begin
    with p^ do begin                { using given node }
    if optab[code].opclass <> slcti then    { if not selector op }
        badnode(p, 164);             { non-selector in selector expr}
    if nrarg > 0 then safeselector(arg[1]);    { insure next selector OK }
    if code = indexop then begin        { only subscripts need check }
        safeexpr(arg[2]);            { defined check for sub expr }
        subscriptcheck(p);            { gen subscript check }
        end;                { end is index op }
    end;                    { end with }
end {safeselector};
