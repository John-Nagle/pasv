/* Yacc grammar for Pascal-F 
   Version 1.11  (last modified 5/1/81) */

%token ALL
%token AND
%token ARRAY
%token ASSERT
%token BECOMES
%token BEGIN
%token CASE
%token CONST
%token DEPTH
%token DEVICE
%token DIV
%token DO
%token DOTDOT
%token DOWNTO
%token EFFECT
%token ELSE
%token END
%token ENTRY
%token EQUAL
%token EXIT
%token EXPORTS
%token EXTRA
%token FIXED
%token FOR
%token FREE
%token FUNCTION
%token GREATER.THAN
%token GREATER.THAN.OR.EQUAL
%token IDENTIFIER
%token IF
%token IMPLIES
%token IMPORTS
%token IN
%token INIT
%token INVARIANT
%token LABEL
%token LESS.THAN
%token LESS.THAN.OR.EQUAL
%token MEASURE
%token MOD
%token MODULE
%token MONITOR
%token NOT
%token NOT.EQUAL
%token OF
%token OR
%token OTHERS
%token PACKED
%token PRECISION
%token PRIORITY
%token PROCEDURE
%token PROGRAM
%token PROOF
%token RAISE
%token RECORD
%token REPEAT
%token SEND
%token SET
%token SIGNAL
%token SOME
%token STATE
%token STRING
%token SUMMARY
%token THEN
%token TO
%token TYPE
%token UNSIGNED.NUMBER
%token UNTIL
%token VALUE
%token VAR
%token WAIT
%token WHEN
%token WHILE
%token WITH
%%
program :
    program.heading outer.block '.' ;

program.heading:
    PROGRAM IDENTIFIER ';' ;

outer.block:
    label.declaration.part constant.definition.part
    type.definition.part value.declaration.part
    variable.declaration.part global.routine.declarations statement.part ;

label.declaration.part:
    empty | LABEL label.list ';' ;

label.list:
    label | label.list ',' label ;

entry.declaration.part:
    empty | ENTRY expression.sequence ';' ;

exit.declaration.part:
    empty | EXIT expression.sequence ';' ;

effect.declaration.part:
    empty | EFFECT expression.sequence ';' ;

invariant.declaration.part:
    empty | INVARIANT expression.sequence ';' ;

expression.sequence:
    expression | expression.sequence ';' expression ;

depth.declaration.part:
    empty | DEPTH expression ';' ;

constant.definition.part:
    empty | CONST constant.definition.sequence ';' ;

constant.definition.sequence:
    constant.definition | constant.definition.sequence ';' constant.definition ;

type.definition.part:
    empty | TYPE type.definition.sequence ';' ;

type.definition.sequence:
    type.definition | type.definition.sequence ';' type.definition ;

variable.declaration.part:
    empty | VAR variable.declaration.sequence ';' ;

variable.declaration.sequence:
    variable.declaration | variable.declaration.sequence ';' variable.declaration ;

value.declaration.part:
    empty | VALUE value.declaration.sequence ';' ;

value.declaration.sequence:
    value.declaration | value.declaration.sequence ';' value.declaration ;

global.routine.declarations:
    empty | global.routine.declarations global.routine ';' ;

routine.declaration.part:
    empty | routine.declaration.part routine.declaration ';' ;

statement.part:
    BEGIN statement.sequence optional.exception.handler.sequence END ;
 
optional.exception.handler.sequence:
    empty | ';' exception.handler optional.exception.handler.sequence ;

global.routine:
    monitor.declaration | routine.declaration ;

routine.declaration:
    module.declaration | function.declaration | procedure.declaration ;

module.declaration:
    module.heading block ;

module.heading:
    MODULE IDENTIFIER ';' export.list import.list ;

export.list:
    EXPORTS identifier.list ';' | empty ;

import.list:
    IMPORTS identifier.list ';' | empty ;

monitor.declaration:
    monitor.heading block ;

monitor.heading:
    MONITOR IDENTIFIER PRIORITY priority.level ';' export.list import.list ;

priority.level:
    constant ;

procedure.declaration:
    procedure.heading block ;

procedure.heading:
    possibly.extra PROCEDURE IDENTIFIER parameters ';' ;

function.declaration:
    function.heading block ;

function.heading:
    possibly.extra FUNCTION IDENTIFIER parameters ':' IDENTIFIER ';' ;

parameters:
    '(' formal.parameter.section.sequence ')' | empty ;

formal.parameter.section.sequence:
    formal.parameter.section | formal.parameter.section.sequence ';' formal.parameter.section ;

formal.parameter.section:
    parameter.group | VAR parameter.group ;

parameter.group:
    identifier.list ':' possibly.extra IDENTIFIER ;

block:
    label.declaration.part entry.declaration.part exit.declaration.part
    effect.declaration.part invariant.declaration.part
    depth.declaration.part constant.definition.part type.definition.part
    value.declaration.part variable.declaration.part
    routine.declaration.part statement.part ;

value.declaration:
    IDENTIFIER '=' IDENTIFIER '(' value.list ')' ;

value.list:
    value | value.list ',' value ;

value:
    constant | '(' value.list ')' ;

compound.statement:
    BEGIN statement.sequence END ;

statement.sequence:
    statement | statement.sequence ';' statement ;

statement:
    unlabelled.statement | label ':' unlabelled.statement |
    PROOF unlabelled.statement | verification.statement ;

verification.statement:
    assert.statement | summary.statement ;

unlabelled.statement:
    simple.statement | structured.statement ;

simple.statement:
    assignment.statement | procedure.statement | init.statement |
    send.statement | wait.statement | raise.statement | empty.statement ;

structured.statement:
    compound.statement | conditional.statement | repetitive.statement |
    with.statement ;

exception.handler:
    WHEN exception.spec DO statement ;

exception.spec:
    identifier.list | OTHERS ;

constant.definition:
    IDENTIFIER '=' constant ;

constant:
    unsigned.constant | sign unsigned.constant ;

unsigned.constant:
    UNSIGNED.NUMBER | STRING | IDENTIFIER ;

sign:
    '+' | '-' ;

type.definition:
    IDENTIFIER '=' type ;

type:
    simple.type | structured.type ;

simple.type:
    ordinal.type | fixed.point.type | signal.type ;

ordinal.type:
    enumerated.type | subrange.type | IDENTIFIER | ;

enumerated.type:
    '(' identifier.list ')' ;

subrange.type:
    constant DOTDOT constant ;

fixed.point.type:
    FIXED constant DOTDOT constant PRECISION constant ;

signal.type:
    SIGNAL hardware.mapping ;

structured.type:
    unpacked.structured.type | PACKED unpacked.structured.type |
    DEVICE hardware.mapping field.list END ;

hardware.mapping:
    '[' address ']' | empty ;

address:
    expression ;

unpacked.structured.type:
    array.type | record.type | set.type ;

array.type:
    ARRAY '[' index.type.list ']' OF component.type ;

index.type.list:
    index.type | index.type.list ',' index.type ;

index.type:
    ordinal.type ;

component.type:
    type ;

record.type:
    RECORD field.list END ;

field.list:
    fixed.part | fixed.part ';' variant.part | variant.part ;

fixed.part:
    record.section.list ;

record.section.list:
    record.section | record.section.list ',' record.section ;

record.section:
    identifier.list ':' possibly.extra type | empty ;

variant.part:
    CASE IDENTIFIER ':' possibly.extra IDENTIFIER OF variant.sequence ;

variant.sequence:
    variant | variant.sequence ';' variant ;

variant:
    case.label.list ':' '(' field.list ')' | empty ;

set.type:
    SET OF base.type ;

base.type:
    simple.type ;

variable.declaration:
    identifier.list ':' possibly.extra.or.free type ;

identifier.list:
    IDENTIFIER | identifier.list ',' IDENTIFIER ;

possibly.extra.or.free:
    FREE | possibly.extra ;

possibly.extra:
    empty | EXTRA ;

assignment.statement:
    variable BECOMES expression ;

variable:
    IDENTIFIER optional.component.part.series ;

optional.component.part.series:
    empty | optional.component.part.series component.part ;

component.part:
    '[' expression.list ']' | '.' IDENTIFIER ;

expression:
    qualifier relation ;

qualifier:
    empty | IF expression THEN expression ELSE |
    FOR IDENTIFIER ':' type quantifier ;

quantifier:
    SOME | ALL ;

relation:
    simple.expression relation.part ;

relation.part:
    empty | relational.operator relation ;

relational.operator:
    EQUAL | NOT.EQUAL | LESS.THAN | LESS.THAN.OR.EQUAL |
    GREATER.THAN.OR.EQUAL | GREATER.THAN | IN | IMPLIES ;

simple.expression:
    unsigned.simple.expression | sign unsigned.simple.expression ;

unsigned.simple.expression:
    term addend ;

addend :
    empty | adding.operator unsigned.simple.expression ;

adding.operator:
    '+' | '-' | OR ;

term:
    factor multiplier.part ;

multiplier.part:
    empty | multiplying.operator term ;

multiplying.operator:
    '*' | '/' | DIV | MOD | AND ;

factor:
    variable | STRING | UNSIGNED.NUMBER | '(' expression ')' |
    IDENTIFIER actual.parameter.part | set | NOT factor ;

set:
    '[' optional.element.list ']' ;

optional.element.list:
    element.list | empty ;

element.list:
    element | element.list ',' element ;

element:
    expression range.part ;

range.part:
    empty | DOTDOT expression ;

procedure.statement:
    IDENTIFIER actual.parameter.part | IDENTIFIER ;

actual.parameter.part:
    '(' actual.parameter.list ')' ;

actual.parameter.list:
    actual.parameter | actual.parameter.list ',' actual.parameter ;

actual.parameter:
    expression ;

label:
    constant ;

empty.statement:
    empty ;

conditional.statement:
    if.statement | case.statement ;

if.statement:
    IF expression THEN statement else.part ;

else.part:
    empty | ELSE statement ;

case.statement:
    CASE expression OF case.element.sequence END ;

case.element.sequence:
    case.element | case.element.sequence ';' case.element ;

case.element:
    case.label.list ':' statement | empty ;

case.label.list:
    case.label | case.label.list ',' case.label ;

case.label:
    constant ;

repetitive.statement:
    while.statement | repeat.statement | for.statement ;

while.statement:
    WHILE expression DO loop.body ;

repeat.statement:
    REPEAT loop.body UNTIL expression ;

for.statement:
    FOR IDENTIFIER BECOMES for.list DO loop.body ;

for.list:
    initial.value direction final.value ;

direction:
    TO | DOWNTO ;

initial.value:
    expression ;

final.value:
    expression ;

loop.body:
    BEGIN loop.body.head state.statement optional.measure.statement
    loop.body.tail END ;

loop.body.head:
    empty | loop.body.head statement ';' ;

loop.body.tail:
    empty | loop.body.tail ';' statement ;

optional.measure.statement:
    empty | measure.statement ;

with.statement:
    WITH identifier.list DO statement ;

init.statement:
    INIT IDENTIFIER ;

raise.statement:
    RAISE IDENTIFIER ;

send.statement:
    SEND IDENTIFIER ;

wait.statement:
    WAIT IDENTIFIER ;

assert.statement:
    ASSERT '(' expression.list ')' ;

summary.statement:
    SUMMARY '(' expression.list ')' ;

state.statement:
    STATE '(' expression.list ')' ;

expression.list:
    expression | expression.list ',' expression ;

measure.statement:
    MEASURE '(' expression ')' ;

empty: ;
