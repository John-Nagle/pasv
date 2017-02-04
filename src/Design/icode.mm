.nr Hc 2
.nr Hs 9
.nr Hb 9
.nr Ej 2
.rm ul
.rm cu
.TL
Pascal-F Verifier Internal Design Document
.AF "FACC / Palo Alto"
``Icode'' Interface Control Document
.AU "John Nagle" JBN "" "Software Technology"
.PF "|Draft 1.9|``Icode'' -- Interface Control Document|4/28/82|"
.MT 4
.SA 1
.H 1 "Introduction"
``Icode'' is the name given to the intermediate language produced by the
first pass of the Pascal-F compiler.  As the first pass of the compiler is
used both as a component of the compiler and of the Pascal-F Verifier,
it is necessary to clearly establish the 
syntax and semantics of ``icode''.
.H 2 "The Icode Operators"
.VL 22 2
.LI "xch\ (1)"
Working operator.  Causes the top two operands on the stack to be exchanged.
.LI "del\ (2)"
Working operator.  Causes the top operand on the stack to be deleted.
.LI "fix\ (3)"
Working operator.
.LI "monit\ (4)"   
Working operator.  Identifies the beginning of a monitor or module.
.LI "ident\ (5)"   
Working operator.  Marks an identifier as an object in the icode stream.
.LI "proc\ (6)"    
Working operator.  Identifies the beginning of a procedure or function.
.LI "end\ (7)"
Working operator.  Identifies the end of a monitor, module, procedure, function,
or main program.
.LI "null\ (8)"
Statement operator, 0-argument.
Represents the null statement.
.LI "refer\ (9)"
Selector operator, 1-argument, result type pointer, argument type: data.
Used when passing data items by address as VAR arguments to procedures.
.LI "stol\ (10)"
Statement operator, 2-argument, argument types: data, [-32768..32767].
Store operator; stores second argument into first.
.LI "stor\ (11)"
Unused.
.LI "stof\ (12)"
Statement operator, 2-argument, argument types: data, fixed point.
Store operator for fixed point operands.
.LI "succ\ (16)"
Expression operator, 1-argument, result type oversize integer, argument type: [-32768..32767].
Successor operator.
.LI "pred\ (17)"
Expression operator, 1-argument, result type oversize integer, argument type: [-32768..32767].
Predecessor operator.
.LI "uceq\ (24)"
Expression operator, 2-argument, result type Boolean, argument types: [0..32767], [0..32767].
Unsigned compare equal.
.LI "ucne\ (25)"
Expression operator, 2-argument, result type Boolean, argument types: [0..32767], [0..32767].
Unsigned compare not equal.
.LI "ucgt\ (26)"
Expression operator, 2-argument, result type Boolean, argument types: [0..32767], [0..32767].
Unsigned compare greater.
.LI "ucle\ (27)"
Expression operator, 2-argument, result type Boolean, argument types: [0..32767], [0..32767].
Unsigned compare less than or equal.
.LI "ucge\ (28)"
Expression operator, 2-argument, result type Boolean, argument types: [0..32767], [0..32767].
Unsigned compare greater than or equal.
.LI "uclt\ (29)"
Expression operator, 2-argument, result type Boolean, argument types: [0..32767], [0..32767].
Unsigned compare less than.
.LI "umax\ (30)"
Expression operator, 2-argument, result type [0..32767], argument types: [0..32767], [0..32767].
Unsigned maximum function.
.LI "umin\ (31)"
Expression operator, 2-argument, result type [0..32767], argument types: [0..32767], [0..32767].
Unsigned minimum function.
.LI "iadd\ (32)"
Expression operator, 2-argument, result type oversize integer, argument types: [-32768..32767], [-32768..32767].
Integer add.
.LI "isub\ (33)"
Expression operator, 2-argument, result type oversize integer, argument types: [-32768..32767], [-32768..32767].
Integer subtract.
.LI "imul\ (34)"
Expression operator, 2-argument, result type oversize integer, argument types: [-32768..32767], [-32768..32767].
Integer multiply.
.LI "idiv\ (35)"
Expression operator, 2-argument, result type [-32768..32767], argument types: [-32768..32767], [-32768..-1, 1..32767].
Integer divide.
.LI "imod\ (36)"
Expression operator, 2-argument, result type [-32768..32767], argument types: [-32768..32767], [-32768..-1, 1..32767].
Integer remainder.
.LI "ineg\ (40)"
Expression operator, 1-argument, result type oversize integer, argument type: [-32768..32767].
Integer negation.
.LI "iabs\ (41)"
Expression operator, 1-argument, result type oversize integer, argument type: [-32768..32767].
Integer absolute value.
.LI "iodd\ (42)"
Expression operator, 1-argument, result type Boolean, argument type: [-32768..32767].
Integer test for odd.
.LI "ceil\ (44)"
Expression operator, 1-argument, result type [-32768..32767], argument type: [-32768..32767].
Integer ceiling.
.LI "floor\ (45)"
Expression operator, 1-argument, result type [-32768..32767], argument type: [-32768..32767].
Integer floor.
.LI "sadd\ (48)"
Expression operator, 2-argument, result type fixed point, argument types: fixed point, fixed point.
Fixed point add.
.LI "ssub\ (49)"
Expression operator, 2-argument, result type fixed point, argument types: fixed point, fixed point.
Fixed point subtract.
.LI "smul\ (50)"
Expression operator, 2-argument, result type fixed point, argument types: fixed point, fixed point.
Fixed point multiply.
.LI "sdiv\ (51)"
Expression operator, 2-argument, result type fixed point, argument types: fixed point, fixed point.
Fixed point divide.
.LI "rescl\ (53)"
Expression operator, 2-argument, result type Unimplemented, argument types: Unimplemented, Unimplemented.
Fixed point rescale.
.LI "iceq\ (56)"
Expression operator, 2-argument, result type Boolean, argument types: [-32768..32767], [-32768..32767].
Signed integer compare equal.
.LI "icne\ (57)"
Expression operator, 2-argument, result type Boolean, argument types: [-32768..32767], [-32768..32767].
Signed integer compare not equal.
.LI "icgt\ (58)"
Expression operator, 2-argument, result type Boolean, argument types: [-32768..32767], [-32768..32767].
Signed integer compare greater than.
.LI "icle\ (59)"
Expression operator, 2-argument, result type Boolean, argument types: [-32768..32767], [-32768..32767].
Signed integer less than or equal to.
.LI "icge\ (60)"
Expression operator, 2-argument, result type Boolean, argument types: [-32768..32767], [-32768..32767].
Signed integer compare greater than or equal to.
.LI "iclt\ (61)"
Expression operator, 2-argument, result type Boolean, argument types: [-32768..32767], [-32768..32767].
Signed integer compare less than.
.LI "imax\ (62)"
Expression operator, 2-argument, result type [-32768..32767], argument types: [-32768..32767], [-32768..32767].
Signed integer maximum.
.LI "imin\ (63)"
Expression operator, 2-argument, result type [-32768..32767], argument types: [-32768..32767], [-32768..32767].
Signed integer minimum.
.LI "fadd\ (64)"
Unused.
.LI "fsub\ (65)"
Unused.
.LI "fmul\ (66)"
Unused.
.LI "fdiv\ (67)"
Unused.
.LI "fneg\ (72)"
Unused.
.LI "fabs\ (73)"
Unused.
.LI "float\ (74)"
Unused.
.LI "trunc\ (75)"
Unused.
.LI "round\ (76)"
Unused.
.LI "fxeq\ (80)"
Expression operator, 2-argument, result type Boolean, argument types: fixed point, fixed point.
Fixed point compare equal.
.LI "fxne\ (81)"
Expression operator, 2-argument, result type Boolean, argument types: fixed point, fixed point.
Fixed point compare not equal.
.LI "fxgt\ (82)"
Expression operator, 2-argument, result type Boolean, argument types: fixed point, fixed point.
Fixed point greater than.
.LI "fxle\ (83)"
Expression operator, 2-argument, result type Boolean, argument types: fixed point, fixed point.
Fixed point less than or equal to.
.LI "fxge\ (84)"
Expression operator, 2-argument, result type Boolean, argument types: fixed point, fixed point.
Fixed point greater than or equal to.
.LI "fxlt\ (85)"
Expression operator, 2-argument, result type Boolean, argument types: fixed point, fixed point.
Fixed point less than.
.LI "fxmax\ (86)"
Expression operator, 2-argument, result type fixed point, argument types: fixed point, fixed point.
Fixed point maximum.
.LI "fxmin\ (87)"
Expression operator, 2-argument, result type fixed point, argument types: fixed point, fixed point.
Fixed point minimum.
.LI "fceq\ (88)"
Unused.
.LI "fcne\ (89)"
Unused.
.LI "fcgt\ (90)"
Unused.
.LI "fcle\ (91)"
Unused.
.LI "fcge\ (92)"
Unused.
.LI "fclt\ (93)"
Unused.
.LI "fmax\ (94)"
Unused.
.LI "fmin\ (95)"
Unused.
.LI "not\ (96)"
Expression operator, 1-argument, result type Boolean, argument type: Boolean.
Boolean negation.
.LI "eqv\ (104)"
Expression operator, 2-argument, result type Boolean, argument types: Boolean, Boolean.
Boolean equivalence.
.LI "xor\ (105)"
Expression operator, 2-argument, result type Boolean, argument types: Boolean, Boolean.
Boolean exclusive or.
.LI "nimp\ (106)"
Expression operator, 2-argument, result type Boolean, argument types: Boolean, Boolean.
Boolean negative implication.
.LI "rimp\ (107)"
Expression operator, 2-argument, result type Boolean, argument types: Boolean, Boolean.
Boolean reverse implication.
.LI "imp\ (108)"
Expression operator, 2-argument, result type Boolean, argument types: Boolean, Boolean.
Boolean implication.
.LI "nrimp\ (109)"
Expression operator, 2-argument, result type Boolean, argument types: Boolean, Boolean.
Boolean negative reverse implication.
.LI "or\ (110)"
Expression operator, 2-argument, result type Boolean, argument types: Boolean, Boolean.
Boolean or.
.LI "and\ (111)"
Expression operator, 2-argument, result type Boolean, argument types: Boolean, Boolean.
Boolean and.
.LI "compl\ (112)"
Expression operator, 1-argument, result type set, argument type: set.
Set complement.
.LI "union\ (113)"
Expression operator, 2-argument, result type set, argument types: set, set.
Set union.
.LI "inter\ (114)"
Expression operator, 2-argument, result type set, argument types: set, set.
Set intersection.
.LI "sdiff\ (115)"
Expression operator, 2-argument, result type set, argument types: set, set.
Set difference.
.LI "sgens\ (117)"
Expression operator, 2-argument, result type Unimplemented, argument types: Unimplemented, Unimplemented.
.LI "sadel\ (118)"
Expression operator, 2-argument, result type Unimplemented, argument types: Unimplemented, Unimplemented.
.LI "empty\ (119)"
Expression operator, 0-argument, result type set.
Empty set constant.
.LI "sceq\ (120)"
Expression operator, 2-argument, result type Boolean, argument types: set, set.
Set compare equal.
.LI "scne\ (121)"
Expression operator, 2-argument, result type Boolean, argument types: set, set.
Set compare unequal.
.LI "scgt\ (122)"
Unused.
.LI "scle\ (123)"
Expression operator, 2-argument, result type Boolean, argument types: set, set.
Improper subset test.
.LI "scge\ (124)"
Expression operator, 2-argument, result type Boolean, argument types: set, set.
Improper superset test.
.LI "sclt\ (125)"
Unused.
.LI "in\ (126)"
Expression operator, 2-argument, result type Boolean, argument types: [0..15], set.
Set membership test.
.LI "sany\ (127)"
Unused.
.LI "field\ (131)"
Selector operator, 1-argument, result type data, argument type: addr.
Specifies a subfield of a word by bit offset and length in bits.
.LI "ofset\ (132)"
Selector operator, 1-argument, result type data, argument type: addr.
Specifies a subfield of a record by byte offset and length in bytes.
.LI "indir\ (133)"
Selector operator, 1-argument, result type data, argument type: pointer.
Indirect address operator; used in referencing VAR formal arguments to routines.
.LI "index\ (134)"
Selector operator, 2-argument, result type data, argument types: addr, [0..32767].
Array subscript operator.
.LI "movem\ (135)"
Statement operator, 2-argument, argument types: addr, addr.
Data move operator; used for record and array assignments.
.LI "invok\ (138)"
Unused.
.LI "rtemp\ (140)"
Selector operator, 0-argument, result type pointer.
Used to refer to temporary variables created with the ``dtemp'' operator.
.LI "dtemp\ (141)"
Statement operator, 2-argument, argument types: ind, statement.
Declares a temporary variable for the duration of one statement; used in the implementation of WITH statements.
.LI "if\ (144)"
Statement operator, 3-argument, argument types: Boolean, statement, statement.
IF statement operator.
.LI "case\ (145)"
Statement operator, N-argument, argument types: [-32768..32767], [-32768..32767], statement, [-32768..32767].
CASE statement operator.
.LI "entry\ (146)"
Unused.
.LI "loop\ (147)"
Statement operator, N-argument, argument types: statement.
Loop operator; used for LOOP, WHILE, and REPEAT statements.
.LI "exit\ (148)"
Statement operator, 2-argument, argument types: Boolean, statement.  
Exit from loop operator; used within loop constructs.
.LI "for\ (149)"
Statement operator, 5-argument, argument types: data, [-32768..32767], i8, [-32768..32767], statement.
FOR statement operator.
.LI "seq\ (152)"
Statement operator, N-argument, argument types: statement.
Represents a sequence of statements.
.LI "wait\ (154)"
Statement operator, 1-argument, argument type: signal.
WAIT statement operator.
.LI "send\ (155)"
SEND statement operator.
Statement operator, 1-argument, argument type: signal.
.LI "tsig\ (156)"
Expression operator, 1-argument, argument type: signal.
.LI "lock\ (157)"
Expression operator, 1-argument, result type data, argument type: data.
.LI "enabl\ (158)"
Unused.
.LI "litsc\ (160)"
Literal operator, 0-argument, result type set.
Set constant operator.
.LI "liter\ (162)"
Literal operator, 0-argument, result type [-32768..32767].
Numeric literal operator.
.LI "rdata\ (163)"
Selector operator, 0-argument, result type data.
Used to specify addresses in VALUE data space.
.LI "litd\ (164)"
Literal operator, 0-argument, result type [-32768..32767].
.LI "raise\ (165)"
Statement operator, 1-argument.
.LI "vceq\ (168)"
Expression operator, 2-argument, result type Boolean, argument types: Unimplemented, Unimplemented.
.LI "vcne\ (169)"
Expression operator, 2-argument, result type Boolean, argument types: Unimplemented, Unimplemented.
.LI "vcgt\ (170)"
Expression operator, 2-argument, result type Boolean, argument types: Unimplemented, Unimplemented.
.LI "vcle\ (171)"
Expression operator, 2-argument, result type Boolean, argument types: Unimplemented, Unimplemented.
.LI "vcge\ (172)"
Expression operator, 2-argument, result type Boolean, argument types: Unimplemented, Unimplemented.
.LI "vclt\ (173)"
Expression operator, 2-argument, result type Boolean, argument types: Unimplemented, Unimplemented.
.LI "dvad\ (174)"
Selector operator, 0-argument, result type data.
Used to specify addresses in device space.
.LI "varbl\ (176)"
Selector operator, 0-argument, result type data.
Used to specify addresses in absolute data space or stack-relative data space.
.LI "param\ (192)"
Selector operator, 0-argument, result type data.
Used to specify addresses in routine parameter space.
.LI "call\ (208)"
Statement operator, N-argument, argument types: statement.
Used to call procedures and functions.
.LI "icall\ (224)"
Expression operator, 0-argument.
Used to call monitors and modules.
.LI "defnd\ (249)"
Expression operator, 1-argument, result type Boolean, argument type: data.
Verifier only.  Boolean test for definedness.
.LI "old\ (250)"
Selector operator, 1-argument, result type data, argument type: data.
Verifier only.  Used only in EXIT declarations to refer to the value of a
variable at entry to the routine.
.LI "vdecl\ (252)"
Declaration operator, 1-argument, argument type: Boolean.
Verifier only.  Used only in the specification part of the icode for a routine; indicates each declaration assertion.
.LI "vhead\ (253)"
Declaration operator, 2-argument.
Verifier only.  This is always the top operator in the tree for the verifier.
The first argument is a sequence of verifier assertions, and the second
are the program statements.
.LI "asert\ (254)"
Statement operator, N-argument, argument type: Boolean.
Verifier only.  Used to represent statement-type assertions (STATE,
ASSERT, and SUMMARY) in icode.
.LI "linen\ (255)"
Working operator.  Verifier only.  Used to express source line number 
information in icode.
