procedure WHATselect; const WHAT = '@(#)p2select.i    2.2'; begin SINK := WHAT; end;    { Version 2.2 of 12/13/82 }
{
    Selector expresssion processing

    A selector expression in Pascal-F is a variable-valued expression
    suitable for appearance on the left side of an assignment statement.
    Only selector expressions may be passed as VAR arguments to routines.

    While selector expressions may be quite complex, the most common
    selector expression is a simple variable reference.
}
{
    forward references
}
procedure genjexpr(p: ptn);             { mutual recursion ahead }
           forward;
procedure genmexpr(p: ptn);
           forward;
{
    findarray  --  find array which index operator refers to

    An index operator normally has a vtype which references an
    array element.  However, should the array be composed of
    records of one element, the result of the index operator may
    be a record field deeper in the structure.  Thus, whenever
    the array element varnode associated with an index operator is
    needed, this routine must be called to return the proper varnode.
}
function findarray(p: ptn)            { index operator }
        : varnodep;            { returned array varnode }
var quit: boolean;                { for loop exit }
    v: varnodep;                { working varnode }
    upform : datakind;                { kind of v^.up }
begin
    with p^ do begin                { using given node }
    assert(code = indexop);            { apply only to index operators}
    v := vtype;                { starting vtype }
    end;
    assert(v <> nil);                { must exist }
    quit := false;                { start loop }
    repeat                     { must search for array }
    if v^.up = nil then             { if no up node }
        verybadnode(p, 177);        { array search failed }
    upform := v^.up^.vardata.form;        { get form of parent }
    if upform = arraydata then begin    { if array found }
        findarray := v;            { success }
        quit := true;            { force exit }
    end else begin                { if no find }
        if upform <> recorddata then     { must be part of record }
        verybadnode(p, 178);        { bad form in array search }
        v := v^.up;                { continue search }
        end;
    until quit;                { until told to stop }
end {findarray};
{
    buildselstack  --  build stack representing selector expression

    This routine builds an array representing the components of a 
    selector expression.  Each entry in the array will correspond
    to exactly one jcode operation.  The primary necessity for this
    routine comes from the fact that field and ofset operators in icode
    may refer to more than one level of record extraction, and varbl,
    param, rddata, and dvad operators may refer to a subpart of a
    variable.  Such icode generates good machine instructions but
    is difficult to decompile.
}
procedure buildselstack(p: ptn;            { selector expression }
            var stk: selstack);    { returned selector stack }
var more: boolean;                { more expression to do }
    recelt: boolean;                { still inside record elt }
    w: ptn;                    { working point in icode }
    vwork: varnodep;                { working point in var structur}
    isold: boolean;                { if var.old }
{
    push  --  push item onto sel stack
}
procedure push(sel: selkind;            { kind of selection }
           v: varnodep;            { var info at this level }
           subscr: ptn;            { subscript expression or nil }
           oldvar: boolean);        { if var.old }
begin
    if stk.top >= maxselstack then verybadnode(p, 120); { selector too complex }
    stk.top := stk.top + 1;            { push stack } 
    with stk.tab[stk.top] do begin        { using stack element }
    stkind := sel;                { set sel }
    stvar := v;                { set varnode }
    stsub := subscr;            { set expr for index }
    stold := oldvar;            { set oldness mode }
    end;
end {push};
{
    pushimplicits  --  push implicit fields

    An icode operator containing a size may perform any number of
    field extractions in one operation.  This routine makes these
    field extractions implicit.
}
procedure pushimplicits(vhave, vneed: varnodep);{ bounds of implicit fields }
begin
    assert(vneed <> nil);            { must exist }
    assert(vhave <> nil);            { must exist }
    while vhave <> vneed do begin        { until match achieved }
    assert(vhave^.up^.vardata.form = recorddata); { must be field }
    push(recordsel, vhave, nil, false);    { insert needed dummy field }
    vhave := vhave^.up;            { continue up }
    assert(vhave <> nil);            { must not hit top of def }
    end;                    { implicit field loop }
end {pushimplicits};
begin {buildselstack}
    stk.sesubstitute := true;            { normal mode is substitute }
    stk.sellinen := p^.linen;            { line number for messages }
    w := p;                    { working position in icode }
    isold := false;                { not seen as old }
    stk.sedef := genwithoutdef;            { not defined mode }
    stk.top := 0;                { no elts on stack yet }
    more := true;                { at least one to do }
    repeat                    { for entire expression }
    with w^ do                 { using current icode node }
    case code of                { fan out on icode operator }
    ofsetop, fieldop: begin            { record field }    
        push(recordsel, vtype, nil, false);    { push field reference }
        pushimplicits(vtype^.up, arg[1]^.vtype);{ note implicit fields }
        end;
    indexop: begin                { array subscript }
        vwork := findarray(w);        { find array element item }
        pushimplicits(vtype, vwork);    { note implicit fields }
        push(arraysel, vwork, arg[2], false);{ push array reference }
        pushimplicits(vwork^.up, arg[1]^.vtype);{ note implicit fields }
        end;
    referop: begin                { address take }
        pushimplicits(vtype, arg[1]^.vtype);{ note implicit fields } 
        end;
    indirop: begin                { indirect }
        {
        Indir appears in two contexts:
        1)  Reference to formal VAR param, in which case vwork will
            refer to the pointer passed into the routine
        2)  Reference to WITH argument, in which case vwork will
            be the result of the refer operator below, but no
            pointer varnode will exist.
        This dichotomy requires special cases in everything dealing
        with indir operators.
        }
        vwork := arg[1]^.vtype;        { result of next operator }
        if arg[1]^.code = paramop then     { if indir of param }
        if vwork^.vardata.by = byreference then begin { if VAR param }
            assert(vwork^.vardata.form = pointerdata); { have pointer }
            vwork := vwork^.down;    { get useful object }
            end;
        pushimplicits(vtype, vwork);    { note implicit fields }
        end;
    oldop: begin                { marked as old }
        isold := true;            { so note }
        if vtype <> arg[1]^.vtype then begin { if type change }
        verybadnode(w, 122);        { type change across old op }
        end;
        end;
    paramop, dvadop, rdataop, varblop: begin { data object }
        vwork := vtype;            { get type of self }
        if code = paramop then        { if parameter }
        if vwork^.vardata.by = byreference then begin { if VAR param }
            assert(vwork^.vardata.form = pointerdata); { have pointer }
            vwork := vwork^.down;    { get referenced object }
            end;
        repeat                { get down to base variable } 
        recelt := false;        { assume none }
        if vwork^.up <> nil then    { check for have is rec elt }
            if vwork^.up^.vardata.form = recorddata then begin
            push(recordsel, vwork, nil, false); { push rec elt }
            vwork := vwork^.up;    { go to record level }
            recelt := true;        { must do again }
            end;
        until not recelt;        { stop when not at rec elt }
        push(variablesel, vwork, nil, isold);    { final variable }
        if vwork <> basevariable(vwork) then { if not base variable }
        verybadnode(p, 121);        { cannot find base var }
        more := false;            { stop expr analysis }
        end;                { data object }
        end;                { of cases }
    if more then                 { if descend necessary }
        w := w^.arg[1];            { down one level }
    assert(w <> nil);            { bad tree }
    until not more;                { end of main loop }
end {buildselstack};
{
    setsubstitute  --  cause reference to variable to emit 
               temporary value
}
procedure setsubstitute(varkey: varnodep;    { formal to be marked }
            oldkey: boolean;    { true if x.old }
            subid: tempid;        { which temporary id }
            subnew: gennewmode);    { new! to be put out? }
begin
    if sbtop >= maxsubstack then begin        { if subtab full }
    internalerror(124);            { subtab overflow }
    end else begin                { will fit }
    sbtop := sbtop + 1;            { get next avail entry }
    with sbtab[sbtop] do begin        { using new entry }
        sbvarkey := varkey;            { set key }
        sboldkey := oldkey;            { set key }
        sbtempid := subid;            { which TEMP to gen if nonzero }
        sbnew := subnew;            { enclose in new! if true }
        end;
    end;
end {setsubstitute};
{
    substitute  --  takes variable, returns substitution if indicated
}
procedure substitute(varkey: varnodep;        { input variable }
             oldkey: boolean;        { if var.old in source }
             var subid: tempid;        { returned temp id or 0 }
             var subnew: gennewmode);    { returns true if new! req }
var i: longint;                    { for loops }
begin
    subid := 0;                    { assume no find }
    subnew := genwithoutnew;            { these are defaults }
    i := 1;                    { begin search loop }
    while i <= sbtop do begin            { search loop }
    with sbtab[i] do begin            { using current entry }
        if sbvarkey = varkey then         { if v part of key matches }
        if sboldkey = oldkey then begin    { if find }
            subid := sbtempid;        { return temp number }
            subnew := sbnew;        { return new! state }
            i := sbtop;            { force loop exit }
            end;            { end find }
        end;                { end With }
    i := i + 1;                { on to next one }
    end;                    { end while loop }
end {substitute};
{
    clearallsubstitutes  --  clear all substitutions 
}
procedure clearallsubstitutes;
begin
    sbtop := 0;                    { very simple }
end {clearallsubstitutes};
{
    nexttemp  --  returns next available temp id
}
function nexttemp: tempid;
begin
    tempserial := tempserial + 1;        { get next available number }
    nexttemp := tempserial;            { return it }
end {tempid};
{
    gendataref  --  generate reference to data object, using
            applicable substitutions
}
procedure gendataref(line: lineinfo;        { line number }
             v: varnodep;        { data item }
             oldref: boolean;        { ref as x.old }
             defref: gendefmode);    { ref as defined }
var sid: tempid;                { working TEMP$ number }
    snew: gennewmode;                { true if new! }
begin
    substitute(v,oldref,sid,snew);        { check substitutions }
    if oldref then                 { if .old ref }   
    if sid = 0 then begin            { if old ref with no subst }
        usererrorstart(line);        { start message }
        write(output, 'Variable "');
        writestring15(output, v^.vardata.itemname);
        write(output, '" does not have a ".old" value.');
        usererrorend;            { finish message }
        end;                { end bad .old ref }
    gendataid(v,sid,snew,defref);        { generate as indicated }
end {gendataref};
{
    subscript  --  generate expression for array subscript value
}
procedure subscript(v: varnodep;        { item being subscripted }
            p: ptn);            { subscript expression }
var lowbound: longint;                { low bound of array }
begin
    assert(v^.vardata.form = arraydata);    { must subscript array only }
    lowbound :=  v^.vardata.minvalue;        { initial subscript bias }
    with p^ do begin                { using expr }
    if lowbound = 0 then begin        { if array starts at 0 }
        genjexpr(p);            { index expression }
    end else begin                { if not }
        genstring15('(addi!');        { (addi! index minvalue) }
        genspace;
        genjexpr(p);            { index expression }
        genspace;
        genintconst(lowbound);        { minvalue }
        genchar(')');            { end addi }    
        end;
    end;                    { With }
end {subscript};
{
    substackselect  --  generate selection expression for substack

    Used for both select and store operations
}
procedure substackselect(var stk: selstack;    { working stack (unmodified) }
             lowlim: longint);    { limit for stack scan }
var i: 1..maxselstack;                { stack position }
begin
    for i := lowlim to stk.top do begin        { forward pass }
    with stk.tab[i] do begin        { using this selector }
        case stkind of            { fan out on selector type }
        variablesel: begin            { variable }
                        { generate data reference }
        if stk.sesubstitute then begin    { if substituting }
            gendataref(stk.sellinen, stvar, stold, stk.sedef);
        end else begin            {if TEMP substitutes suppressed}
                    { used only for proc actual->formal}
            gendataid(stvar, nulltid, genwithoutnew, stk.sedef);
            end;
        end;
        arraysel: begin            { array reference }
        genstring15('(selecta!');    { (selecta ... }
        genspace;
        end;
        recordsel: begin            { record reference }
        genstring15('(selectr!');    { (selectr ... }
        genspace;
        end;
        end;                { of cases }
        end;                { of with }
    end;                    { downward for }
    for i := stk.top downto lowlim do begin    { reverse pass }
    with stk.tab[i] do begin        { using this selector }
        case stkind of             { fan out on selector type }
        variablesel: begin             { variable }
        end;                { no action }
        arraysel: begin            { array reference }
        genspace;
        subscript(stvar^.up, stsub);    { generate subscript expr }
        genchar(')');            { close selecta }
        end;
        recordsel: begin            { record reference }
        genspace;
        gentypeid(stvar^.up);        { type of parent }
        genchar('$');            { combine field and record id }
        genfieldid(stvar);        { field id }
        genchar(')');            { close selectr! }
        end;
        end;                { of cases }
        end;                { of With }
    end;                    { reverse pass }
end {substackselect};
{
    genselcommon  --  generate selector, with or without defined
}
procedure genselcommon(p: ptn;            { selector expression }
               dodef: gendefmode);    { if defined mode }
var stk: selstack;                { selector stack }
begin
    buildselstack(p, stk);            { build selector stack }
    stk.sedef := dodef;                { set defined mode if asked }
    substackselect(stk,1);            { select for entire stack }
end {genselcommon};
{
    genjselector  --  generate selector expression
}
procedure genjselector(p: ptn);            { selector expression }
begin
    genselcommon(p, genwithoutdef);        { generate non-defined form }
end {genjselector};
{
    gendefselector  --  generate defined selector
}
procedure gendefselector(p: ptn);        { selector expression }
begin
    genselcommon(p, genwithdef);        { generate defined form }
end {gendefselector};
{
    genrepcommon  --  generate store expression     

    Used for assignment operators and routine parameter
    substutitions.
    "lhs" is a selector expression.  "rhs" is a valued expression.
    The jcode expression emitted refers to the base variable of the
    lhs expression with rhs substituted for the the selected element.
Pascal:        a.b[i] := j;

Jcode:        NEW (a) 
                (equal! (new! a)
            (storer! (a) T b
                 (storea! (selectr! (a) T b) (i) (j))))

    functions: 1 = gen normal store expression, new value is rhs
           2 = gen defined store expression, new value is true
           3 = gen normal store expression, new value is tempid
}
procedure genrepcommon( fn: longint;        { function }
            lhs: ptn;        { left hand side of := }
                rhs: ptn;        { right hand side }
            tnum: tempid);        { temp id if any }
var stk: selstack;                { working selection stack }
    i: 1..maxselstack;                { position in selstack }
begin
    buildselstack(lhs, stk);            { build selector stack }
    if fn = 2 then begin            { if defined! mode }
    stk.sedef := genwithdef;        { mark stack as defined expr }
    end;
    for i := stk.top downto 1 do begin        { inward loop }
    with stk.tab[i] do begin        { using this entry }
        case stkind of            { fan out on selector kind }
        recordsel: begin            { record subscript }
        if comments then gencontinuation; { improve readability }
        genstring15('(storer!');
        genspace;
        substackselect(stk,i+1);    { select substack }
        genspace;
        gentypeid(stvar^.up);        { name of type of record }
        genspace;
        genstring15(stvar^.vardata.itemname); { field id }
        end;
        arraysel: begin            { array subscript }
        if comments then gencontinuation; { improve readability }
        genstring15('(storea!');    { (storea! (a) (sub) (newval)) }
        genspace;
        substackselect(stk,i+1);    { select substack }
        genspace;
        subscript(stvar^.up, stsub);    { subscript expression }
        end;
        variablesel: begin            { variable  }
        assert(i = stk.top);        { must be at top of stack }
        end;
        end;                { of cases }
        genspace;
        end;                { with }
    end;                    { inward loop }  
    case fn of                    { fan out on function }
    1: begin                    { normal mode }
        genjexpr(rhs);                { now put in right hand side }
    end;
    2: begin                    { defined mode }
    genstring15('(true!)');            { just force true into def } 
    end;
    3: begin                    { temp id mode }
    assert(tnum > 0);            { must have valid temp num }
    genchar('(');                { begin name }
    gentempid(tnum);            { TEMP$nn }
    genchar(')');                { end name }
    end;
    end;                    { of cases }
    for i := 1 to stk.top - 1 do begin        { finish up with right parens }
    genchar(')');                { close expression }
    end;                    { inward loop }  
end {genrepcommon};
{
    genreplace  --  generate store expression in which new value
            is an icode expression
}
procedure genreplace(lhs: ptn;            { left hand side selector }
             rhs: ptn);            { right hand side expression }
begin
    genrepcommon(1, lhs, rhs, 0);        { call common routine }
end;
{
    genreplacedef  --  generate store expression in which everything
               is "defined" and new value is "true".
}
procedure genreplacedef(p: ptn);        { selector expression }
begin
    genrepcommon(2, p, nil, 0);            { call common routine }
end {genreplacedef};
{
    genreplacetemp  --  generate store expression in which
                new value is temp number
}
procedure genreplacetemp(lhs: ptn;        { selector for lhs }
             rhstemp: tempid);    { temp number for rhs }
begin
    genrepcommon(3, lhs, nil, rhstemp);        { call common routine }
end {genreplacetemp};
