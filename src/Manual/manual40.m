.H 1 "The Syntax of Pascal-F"
This chapter describes the syntax of Pascal-F,
including the verification statements.
Lines marked with asterisks indicate productions altered
to include the additional syntax necessary for verification.
The syntax here is a superset of that given in the Pascal-F
Language Reference Manual, and is given in the notation used in that
manual.
.H 2 "Production Rules"
.SP 2
.nf
<program> ::= <program heading> <outer block> .

<program heading> ::= PROGRAM <identifier> ;

<outer block> ::= <label declaration part>
                  <constant definition part>
                  <type definition part>
                  <value declaration part>
                  <variable declaration part>
                  <global routine declarations>
                  <statement part>

<label declaration part> ::=
                  <empty> | LABEL <label> {, <label>} ;

.mc *
<entry declaration part> ::= <empty> |
                ENTRY <expression> {; <expression>} ;

<exit declaration part> ::= <empty> |
                EXIT <expression> {; <expression>} ;

<effect declaration part> ::= <empty> |
                EFFECT <expression> {; <expression>} ;

<invariant declaration part> ::= <empty> |
                INVARIANT <expression> {; <expression>} ;

<depth declaration part> ::=
                <empty> | DEPTH <expression> ;

.mc
<constant definition part> ::= <empty> | CONST
                <constant definition> {; <constant definition>} ;

<type definition part> ::= <empty> |
                TYPE <type definition> {; <type definition>} ;

<variable declaration part> ::= <empty> |
                VAR <variable declaration>
                {; <variable declaration>} ;

<value declaration part> ::= <empty> |
                VALUE <value declaration>
                {; <value declaration> } ;

<global routine declarations> ::= { <global routine> ; }

<routine declaration part> ::= { <routine declaration> ;}

<statement part> ::= BEGIN <statement> {; <statement>}
                        {; <exception handler>}  END

<global routine> ::= <monitor declaration> |
                     <routine declaration>

<routine declaration> ::= <module declaration> |
                          <function declaration> |
                          <procedure declaration>

<module declaration> ::=
                <module heading> <block>

<module heading> ::=
                MODULE  <identifier> ;
                  <export list>  <import list>

<export list> ::= EXPORTS  <identifier> {, <identifier>} ;
                   |  <empty>

<import list> ::= IMPORTS  <identifier> {, <identifier>}  ;
                   |  <empty>

<monitor declaration> ::=
                <monitor heading> <block>

<monitor heading> ::= MONITOR <identifier> PRIORITY
                <priority level> ; <export list>  <import list>

<priority level> ::= <constant>

<procedure declaration> ::= <procedure heading> <block>

.mc *
<procedure heading> ::= <possibly extra> PROCEDURE
                <identifier> <parameters> ;
.mc

<function declaration> ::= <function heading> <block>

.mc *
<function heading> ::= <possibly extra> FUNCTION <identifier>
                <parameters> : <result type> ;
.mc

<parameters> ::= ( <formal parameter section>
                {; <formal parameter section>} ) |  <empty>

<formal parameter section> ::= <parameter group> |
                VAR <parameter group>

.mc *
<parameter group> ::= <identifier> {, <identifier>} :
                  <possibly extra> <type identifier>
.mc

<block> ::= <label declaration part>
.mc *
            <entry declaration part>
            <exit declaration part>
            <effect declaration part>
            <invariant declaration part>
            <depth declaration part>
.mc
            <constant definition part>
            <type definition part>
            <value declaration part>
            <variable declaration part>
            <routine declaration part>
            <statement part>

<value declaration> ::= <identifier> =
                <type identifier> ( <value list> )

<value list> ::= <value> { , <value> }

<value> ::= <constant> | ( <value list> )

<compound statement> ::= BEGIN <statement> {; <statement>} END

.mc *
<statement> ::= <unlabeled statement> |
                <label> : <unlabeled statement> |
                PROOF <unlabeled statement> |
                <verification statement>
.mc

.mc *
<verification statement> ::= <assert statement> |
                             <summary statement>
.mc

<unlabeled statement> ::= <simple statement> |
                <structured statement>

<simple statement> ::= <assignment statement> |
                       <procedure statement> |
                       <init statement> |
                       <send statement> |
                       <wait statement> |
                       <raise statement> |
                       <empty statement>

<structured statement> ::= <compound statement> |
                           <conditional statement> |
                           <repetitive statement> |
                           <with statement>

<exception handler> ::=
          WHEN <exception spec> DO <statement>

<exception spec> ::= <exception name> {, <exception name>} |
                     OTHERS

<exception name> ::= <identifier>

<constant definition> ::= <identifier> = <constant>

<constant> ::= <unsigned constant> | <sign> <unsigned constant>

<type definition> ::= <identifier> = <type>

<type> ::= <simple type> | <structured type>
        
<simple type> ::= <ordinal type> |
                  <fixed point type> |
                  <signal type>

<ordinal type> ::= <enumerated type> |
                   <subrange type> |
                   <type identifier> |

<enumerated type> ::= ( <identifier> {, <identifier>} )

<subrange type> ::= <constant> .. <constant>

<type identifier> ::= <identifier>

<fixed point type> ::= FIXED <constant> .. <constant>
                PRECISION <constant>

<signal type> ::= SIGNAL <hardware mapping>

<structured type> ::= <unpacked structured type> |
                      PACKED <unpacked structured type> |
                      DEVICE <hardware mapping>  <field list> END

<hardware mapping> ::= [ <address> ]  |  <empty>

<address> ::= <expression>

<unpacked structured type> ::= <array type> |
                               <record type> |
                               <set type>

<array type> ::= ARRAY [ <index type> {, <index type>} ] OF
                  <component type>

<index type> ::= <ordinal type>

<component type> ::= <type>


<record type> ::= RECORD <field list> END

<field list> ::= <fixed part> |
                 <fixed part> ; <variant part> |
                 <variant part>

<fixed part> ::= <record section> {, <record section>}

.mc *
<record section> ::= <field identifier> {, <field identifier>}
                        : <possibly extra> <type>   |  <empty>
.mc

.mc *
<variant part> ::= CASE <tag field> <possibly extra>
                   <type identifier> OF <variant> {; <variant>}

<tag field> ::= <identifier> :
.mc

<variant> ::= <case label list> : ( <field list> ) | <empty>

<set type> ::= SET OF <base type>

<base type> ::= <simple type>

.mc *
<variable declaration> ::= <identifier> {, <identifier>} :
                       <possibly extra> <type>
.mc

.mc *
<possibly extra> ::= <empty> | EXTRA

.mc
<result type> ::= <type identifier>

<assignment statement> ::= <variable> := <expression> |
         <function identifier> := <expression>

<variable> ::= <entire variable> {<component part>}

<component part> ::= [ <expression> {, <expression>} ] |
        . <field identifier>

<entire variable> ::= <variable identifier> | <field identifier>

<variable identifier> ::= <identifier>

<field identifier> ::= <identifier>

.mc *
<expression> ::= <relation>
.mc

.mc *
<relation> ::= <simple expression> <relation part>

<relation part> ::= <empty> | <relational operator> <relation>

<relational operator> ::= = | <> | '< | '<= | >= |
                > | IN | IMPLIES

.mc
<simple expression> ::= <unsigned simple expression> |
         <sign> <unsigned simple expression>

<sign> ::= + | -

<unsigned simple expression> ::= <term> <addend>

<addend> ::=  <empty> |
              <adding operator> <unsigned simple expression>

<adding operator> ::= + | - | OR

<term> ::= <factor> <multiplier part>

<multiplier part> ::= <empty> | <multiplying operator> <term>

<multiplying operator> ::= * | / | DIV | MOD | AND

<factor> ::= <variable> | <unsigned constant> | ( <expression> ) |
                 <function designator> | <set> | NOT <factor>

<unsigned constant> ::= <unsigned number> | <string> |
                 <constant identifier>

<constant identifier> ::= <identifier>

<function designator> ::= <function identifier>
                <actual parameter part>

<function identifier> ::= <identifier>

<set> ::= [ <element list> ]

<element list> ::= <element> {, <element>} | <empty>

<element> ::= <expression> <range part>

<range part> ::= <empty> | .. <expression>

<procedure statement> ::= <procedure identifier>
                <actual parameter part>

<procedure identifier> ::= <identifier>

<actual parameter part> ::= <empty> |  ( <actual parameter>
         {, <actual parameter>} )

<actual parameter> ::= <expression>

<label> ::= <constant>

<empty statement> ::= <empty>

<conditional statement> ::= <if statement> | <case statement>

<if statement> ::= IF <expression> THEN <statement> <else part>

<else part> ::= <empty> | ELSE <statement>

<case statement> ::= CASE <expression> OF <case list element>
         {; <case list element>} END

<case list element> ::= <case label list> : <statement> |
         <empty>

<case label list> ::= <case label> {, <case label> }

<case label> ::= <constant>

<repetitive statement> ::= <while statement> |
                           <repeat statement> |
                           <for statement>

.mc *
<while statement> ::= WHILE <expression> DO <loop body>
.mc

.mc *
<repeat statement> ::= REPEAT <loop body>
.mc
         UNTIL <expression>

<for statement> ::= FOR <control variable> := <for list> DO
.mc *
         <loop body>
.mc

<for list> ::= <initial value> <direction> <final value>

<direction> ::= TO | DOWNTO

<control variable> ::= <identifier>

<initial value> ::= <expression>

<final value> ::= <expression>

.mc *
<loop body> ::= BEGIN {<statement> ;} <state statement>
                <optional measure statement> {; <statement>} END
.mc

.mc *
<optional measure statement> ::= ; <measure statement> | <empty>
.mc

<with statement> ::= WITH <record variable list> DO <statement>

<record variable list> ::= <record variable> {, <record variable>}

<record variable> ::= <identifier>

<init statement> ::= INIT <monitor identifier>

<monitor identifier> ::= <identifier>

<raise statement> ::= RAISE <exception name>

<send statement> ::= SEND  <signal name>

<wait statement> ::= WAIT <signal name>
.mc *

<assert statement> ::= ASSERT ( <expression> {, <expression>} )

<summary statement> ::= SUMMARY ( <expression> {, <expression>} )

<state statement> ::= STATE ( <expression> {, <expression>} )

<measure statement> ::= MEASURE ( <expression> )
.mc

<signal name> ::= <identifier>
.fi
