/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
grammar CucarachaGrammar;
import CucarachaLexer;

@header {
    package org.antlr.samples.antlr4example;
}


/* OBSERVATION: some clauses were expanded in order to have smaller ASTs.
                Those rules were annotated with a comment: "EXP: ...".
 */

/***************/
/* PROGRAMS    */
/***************/
gobstones : (progdef)?    # Program
          | progdef defs  # ProgramDefs
          | defs progdef  # DefsProgram
          ;

progdef : PROGRAM LBRACE cmds (cmdreturn)? RBRACE     #NProgram  // EXP: nprogdef, nprogBody
        | INTERACTIVE PROGRAM LBRACE keyassocs RBRACE #IProgram  // EXP: iprogdef, iprogBody
        ;

/***************/
/* DEFINITIONS */
/***************/
defs :  (def)+ ;
def  : PROCEDURE UPPERID varTuple
         LBRACE cmds RBRACE                 #ProcDef     // EXP: procDef, procName, params, procBody
     | FUNCTION  LOWERID varTuple
         LBRACE cmds cmdreturn RBRACE       #FuncDef     // EXP: funcDef, funcName, params, funcBody
     ;

/***************/
/* RETURNS     */
/***************/
cmdreturn : RETURN LPAREN gexp (COMMA gexp)* RPAREN (SEMICOLON)? ; // EXP: gexpTuple1, gexps

/***************/
/* COMMANDS    */
/***************/
cmdblock   : LBRACE cmds RBRACE ;
cmds : (( cmdassign                         // Assignments
        | procCall                          // Procedure calls
        | cmdif     | cmdswitch             // Alternative commands
        | cmdrepeat | cmdwhile | cmdforeach // Repetition commands
        | cmdblock                          // Blocks
        ) SEMICOLON?
       )*
       ;                                    // EXP: cmd, simpcmd, compcmd

cmdassign : LOWERID   ASSIGN gexp                # SimpleAssign // EXP: varName
          | varTuple1 ASSIGN LOWERID gexpTuple   # MultiAssign  // EXP: funcCall, funcName, args
            // NOT EXPANDED to easily identify the function call (LOWERID)
          ;

// NOT EXPANDED to avoid labelling cmd.
procCall : (UPPERID | predefProc) gexpTuple ; // EXP: procName, args
                      // SOON OUTDATED!

// REMOVE THIS PRODUCTION TOGETHER WITH CORRESPONDING TOKENS WHEN ADDING BUILT-IN PROCS
predefProc : PONER | SACAR | MOVER | IRALBORDE | VACIARTABLERO ; // EXP: tableroProcs

// COMPOUND COMMANDS
cmdif      : IF LPAREN gexp RPAREN (THEN)? cmdblock (ELSE cmdblock)? ;
cmdswitch  : SWITCH LPAREN gexp RPAREN TO branches ;
cmdrepeat  : REPEAT LPAREN gexp RPAREN cmdblock;
cmdwhile   : WHILE LPAREN gexp RPAREN cmdblock ;
cmdforeach : FOREACH LOWERID IN sequence cmdblock ; // EXP: varName

sequence : LBRACK (seqenum | seqrange) RBRACK ; // EXP: seqdef
seqenum  : gexp (COMMA gexp)* ;
seqrange : gexp (COMMA gexp)? UPTO gexp ;

branches      : (branch)* defaultbranch ;
defaultbranch : WILD                     INTO cmdblock ;
branch        : literal (COMMA literal)* INTO cmdblock ; // EXP: lits

/***************/
/* EXPRESSIONS */
/***************/
// More similar to the original. Apt for ANTLR v3.0
//gexp : bexp ;
//bexp  : bterm | bterm OROP bexp ; // OR has more precedence than AND, right assoc
//bterm : bfact | bfact ANDOP bterm ; // AND has more precedence than NOT, right assoc
//bfact : (NOT)* batom ;         // NOT has more precedence than ROPs
//batom : nexp (ROP nexp)? ;     // Not assoc (because of nexp)

//nexp  : (nterm aop)* nterm ;   // NOP has more precedence than MOP, left assoc
//nterm : (nfact MOP)* nfact ;   // MOP has more precedence than DOP, left assoc
//nfact : npow (DOP npow)? ;     // DOP has more precedence than EOP, and it's not assoc (because of npow)

aop : POP | NOP ;   // It's needed because token NOP is used both as unary and binary operator

// Redesigned to use precedence and left recursion (ANTLR v4.0)
gexp   : nexp                          # BAtom        // EXP: bexp, bterm, bfact, batom
       | <assoc=right> gexp OROP gexp  # LogicalOr    // OR has more precedence than AND, right assoc
       | <assoc=right> gexp ANDOP gexp # LogicalAnd   // AND has more precedence than NOT, right assoc
       | NOT gexp                      # LogicalNot   // NOT has more precedence than ROPs
       | nexp ROP nexp                 # RelationalOp // Not assoc (because of nexp)
       ;
nexp   : natom                   # NAtom     // EXP: nterm, nfact,
       | nexp aop nexp           # AddOp     // NOP has more precedence than MOP, left assoc
       | nexp MOP nexp           # MultOp    // MOP has more precedence than DOP, left assoc
       | npow (DOP npow)?        # DivEOp    // DOP has more precedence than EOP, and it's not assoc (because of npow)
       ;
npow   : natom (EOP natom)* ;                //  Rigth assoc
natom  : NOP natom               # ANeg
       | LOWERID                 # AVarName  // EXP: varName
       | literal                 # ALiteral
       | funcCall                # AFuncCall
       | LPAREN gexp RPAREN      # AParents
       ;

funcCall : (LOWERID | primFunc) gexpTuple;              // EXP: funcName, args
                      // SOON OUTDATED!!
// REMOVE THIS PRODUCTION TOGETHER WITH CORRESPONDING TOKENS WHEN ADDING BUILT-IN FUNCS
primFunc : NROBOLITAS | HAYBOLITAS | PUEDEMOVER
         | SIGUIENTE  | PREVIO     | OPUESTO
         | MINBOOL    | MAXBOOL
         | MINDIR     | MAXDIR
         | MINCOLOR   | MAXCOLOR
         ;

literal : NUM                             // EXP: literN, literB, literC, literD
        | FALSE | TRUE
        | AZUL  | NEGRO | ROJO | VERDE
        | NORTE | ESTE  | SUR  | OESTE
        ;

/**************************/
/* Interactive programs   */
/**************************/
keyassocs       : (keyassoc)* defaultkeyassoc? ;
keyassoc        : KEYDEF INTO cmdblock ;
defaultkeyassoc : WILD INTO cmdblock;

/******************/
/* Auxiliary defs */
/******************/
varTuple1  : LPAREN LOWERID  (COMMA LOWERID)*   RPAREN ;  // EXP: varNames, varName
varTuple   : LPAREN (LOWERID (COMMA LOWERID)*)? RPAREN ;  // EXP: varNames, varName
gexpTuple  : LPAREN (gexp    (COMMA gexp)*)?    RPAREN ;  // EXP: gexps
