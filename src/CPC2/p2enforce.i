procedure WHATenforce; const WHAT = '@(#)p2enforce.i    2.2'; begin SINK := WHAT; end;    { Version 2.2 of 12/19/82 }
{
    Enforcement of Static Rules
}
{
    freeze/thaw processing
}
{
    freeze  --  freeze of single variable
}
procedure freeze(v: varnodep);            { variable to freeze }
begin
    assert(v <> nil);                { must exist }
    with v^ do begin                { using variable node }
    freezecount := freezecount + 1;        { increment freeze count }
    end;
end {freeze};
{
    thaw  --  thaw of single variable
}
procedure thaw(v: varnodep);            { variable to thaw }
begin
    assert(v <> nil);
    with v^ do begin                { using variable node }
      assert(freezecount > 0);        { must be frozen }
    freezecount := freezecount - 1;        { unfreeze }
    end;
end {thaw};
{
    freezeorthaw  -  freeze or thaw variables within expression

    If valwanted is true, every variable in the expression mentioned
    must be frozen.
    If valwanted is false, the expression is being used in a selector
    sense and only subscript expressions within the selector need be
    frozen.
}
procedure freezeorthaw(p: ptn;            { expr to freeze }
               valwanted: boolean;    { true if freeze entire value }
               isthaw: boolean);    { true=freeze, false=thaw }
var i: 0..maxarg;                { for arg loop }
begin
    with p^ do begin                { using code node }
    if code in 
        [indexop, callop,paramop,varblop] 
    then begin                { special cases }
    case code of                { special case handling }
    indexop: begin                { index }
        if valwanted then            { only freeze array if asked }
            freezeorthaw(arg[1],true,isthaw);{ freeze array as specified }
        freezeorthaw(arg[2],true,isthaw);    { freeze subscript as value }
        end;

    fcallop, callop: begin            { call not allowed in WITH }
        if isthaw then begin        { if thaw call }
        assert(usererrors > 0);        { was diagnosed at freeze }    
        end else begin            { if freeze call }
            usererrorstart(p^.linen);    { start error message }
            write(output,'Function call prohibited in WITH argument.');
            usererrorend;
        end;
        end;

    paramop, varblop: begin            { data object }
        if valwanted then begin        { if value wanted }
            if isthaw then thaw(basevariable(vtype))
                else freeze(basevariable(vtype));    { freeze it }
        end;                { end value wanted }
        end;
    end;                    { of special cases }
    end else begin                { general case }
        for i := 1 to nrarg do begin    { for all args given }
        freezeorthaw(arg[i],valwanted,isthaw); { recurse downward }
        end;
        end;                { end general case }
    end;                    { end with }
end {freezeorthaw};
{
    freezeselector  --  freeze a selector expression
}
procedure freezeselector(p: ptn);        { expression }
begin
    freezeorthaw(p,false,false);        { use common routine }
end {freezeselector};
{
    thawselector -- thaw a selector expression
}
procedure thawselector(p: ptn);            { expression }
begin
    freezeorthaw(p,false,true);            { use common routine }
end {thawselector};
{
    checknotfrozen  --  diagnose if variable is frozen
}
procedure checknotfrozen(p: ptn;        { location for diagnosis }
             v: varnodep);        { variable to check }
begin
    v := basevariable(v);            { get down to base level }
    if v^.freezecount <> 0 then begin        { if frozen, trouble }
    usererrorstart(p^.linen);        { begin error message }
    write(output,'Changing "'); 
    writestring15(output,v^.vardata.itemname); { name of variable }
    write(output,'" here interferes with WITH or FOR above.');
    usererrorend;                { finish error message }
    end;
end {checknotfrozen};
{
    Type compatability checking
}
{
    checkcompat  --  check two variable descriptions for compatability

    The definition of compatiblity is as follows:

    strong = true:
       Full structural equivalence - subtrees must match exactly 
          except for names.

    strong = false:
       If the types are simple, only assignment compatability
       is required, for v1 := v2; i.e. the forms of the types
       must be the same.  Overflow is possible.
}
procedure checkcompat(p: ptn;            { for messages }
              v1,v2: varnodep;        { nodes to check }
              strong: boolean);        { strong check desired }
var errnum: integer;                { error code found }
{
    inner check - recursive 
}
procedure checkcompat1(v1,v2: varnodep;        { nodes to check }
            strong: boolean);    { strong check }
var v1down, v2down: varnodep;            { child pointers }
begin
    if v1 = nil then begin
    errnum := 70;                { badnode:  first var nil }
    end else if v2 = nil then begin 
    errnum := 71;                { badnode:  second var nil }
    end else begin                { non-nil, do compare }
                                 { compare one line item }
                        { check form, size, scale }
    if v1^.vardata.form <> v2^.vardata.form then 
        errnum := 75;             { badnode: type clash (form) }
    if v1^.vardata.size <> v2^.vardata.size then 
        errnum := 76;             { badnode: type clash (size) }
    if v1^.vardata.scale <> v2^.vardata.scale then 
        errnum := 77;             { badnode: type clash (scale) }
    if strong then begin            { if must match exactly }
        if v1^.vardata.minvalue <> v2^.vardata.minvalue then 
        errnum := 78;        { badnode: type clash (min value) }
        if v1^.vardata.maxvalue <> v2^.vardata.maxvalue then 
        errnum := 79;        { badnode: type clash (max value) }
    end else begin                { if assignment compat only }
                        { no bounds check }
        end;
                        { check children }
    v1down := v1^.down;            { v1 child }
    v2down := v2^.down;            { v2 child }
    if (v1down = nil) <> (v2down = nil) then begin { if one has child }
        errnum := 72;            { badnode:  only one has child }
    end else if v1down <> nil then    { if both have children }
        while v1down <> nil do begin    { for all children }
        if v2down = nil then begin
            errnum := 73;    { badnode: more children of arg #1 }
        end else begin
            checkcompat1(v1down, v2down, true); { recurse }
            v1down := v1down^.right;    { advance to right }
            v2down := v2down^.right;    { advance to right }
            end;
        end;                { end child loop }
    if v2down <> nil then             { if not at end of v2 list }
        errnum := 74;    { badnode: more children of arg #2 }
    end;                    { end no nil problems }
end {checkcompat1};
begin {checkcompat}
    errnum := 0;                { no error so far }
    checkcompat1(v1, v2, strong);        { examine recursively }
    if errnum <> 0 then begin            { if error found }
    badnode(p,errnum);            { variable error value }
    writeln('First variable type:');    { print problem }
    printvar(output,v1);            { print variable }
    writeln('Second variable type:');    { print variable }
    printvar(output,v2);
    end;
end {checkcompat};
{
    assignable  --  is assignment legal in static sense?

    This is just a backup check on the compiler.
}
procedure assignable(dest: varnodep;        { vtype of destination }
             source: ptn);        { rhs expression }
begin
    with dest^ do begin                { using destination }
    if mttab[source^.mtype].mtsimple     { if simple source }
    then begin                { if simple form }
        if vardata.form <> mttab[source^.mtype].mtkind  then begin
        badnode(source, 170);        { form clash for assignment }
        end else begin            { if forms match }
        case vardata.form of         { fan out on form }
        booleandata: begin end;        { no problem }
        fixeddata: begin        { fixed point }
            unimplemented(source^.linen); { ***UNIMPLEMENTED*** }
            end;
        numericdata: begin        { numeric }
                        { no checking }
            end;            
        setdata: begin            { set }
            unimplemented(source^.linen); { ***UNIMPLEMENTED*** }
            end;
        end;                { end cases }
        end;                { end forms match }
    end else begin                { if non-simple }
        checkcompat(source, dest, source^.vtype, false);{ check compatible }
        end;                { end non-simple }
    end;                { with }
end {assignable};
{
    validateblockname  --  enforce naming conventions for blocks

    No block may have the name of the main program.
    This allows us to use the names of blocks at the outermost level
    without decoration.  This, in turn, allows us to use the names
    of rule functions in exactly the form given by the user.
    Such functions are known by name in the Rule Builder, and are the
    only objects so known other than built-in functions.
}
procedure validateblockname(where: lineinfo;    { location for debug }
             b: blocknodep);    { block node being tested }
var blk: blocknodep;                { working block }
begin
    blk := b;                    { find main program }
    while blk^.blouterblock <> nil do blk := blk^.blouterblock; { find main }
    if blk <> b then                { if this is not main }
    if blk^.blvarnode^.vardata.itemname = b^.blvarnode^.vardata.itemname
    then begin                { if duplicate }
    diag(where,'The name of the main program cannot be reused here.');
    end;
end {validateblockname};
{
    prioritycheck  --  check for violation of priority rule at call

    Rules:

       1.  Call down is prohibited.
       2.  Calls across priority levels must use lock correctly.
       3.  Exception:  For no-priority callee, above does not matter.
}
procedure prioritycheck(p: ptn;            { node for debug }
            callpri: integer;    { priority of caller after lock}
            callee: blocknodep);    { called node }
var calleepri: priority;            { called priority }
begin
    calleepri := callee^.blpriority;        { priority of callee }
    if calleepri <> nopriority then begin    { if we care about pri }
    if calleepri <> callpri then        { if priorities disagree }
        badnode(p,364);        { lock priority wrong or no lock }
    if lastblockp^.blpriority > calleepri then { if call down }
        badnode(p,365);            { call down in priority }
    end;
end {prioritycheck};
