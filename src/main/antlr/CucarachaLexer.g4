/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
lexer grammar CucarachaLexer;

// Comments will not be part of the AST. That can be a nuisance to print the program back...
WS : ([ \t]|NEWLINE|Comment)+ -> skip ; //  Consider using channels!!!!!
fragment NEWLINE :'\r'? '\n' ; // The optional \r is to care for MS coding of newlines.

fragment Comment : LineComm
                 | ParComm
                 ;
fragment LineComm : '--' .*? '\n'  // The ? after .* makes it non-greedy (TDA4R,Ch.5,p.76)
                  | '//' .*? '\n'
                  | '#'  .*? '\n'
                  ;
fragment ParComm  : '{-' .*? '-}'
                  | '/*' .*? '*/'
                  | '\'\'\'' .*? '\'\'\''
                  ;

// SYMBOLS

LPAREN    : '('  ;
RPAREN    : ')'  ;
COMMA     : ','  ;
LBRACK    : '['  ;
RBRACK    : ']'  ;
SEMICOLON : ';'  ;
LBRACE    : '{'  ;
RBRACE    : '}'  ;
ASSIGN    : ':=' ;
COLON     : ':'  ;
HASH      : '#'  ;
LE        : '<=' ;
GE        : '>=' ;
LT        : '<'  ;
GT        : '>'  ;
EQ        : '==' ;
NE        : '!=' ;
PLUS      : '+'  ;
MINUS     : '-'  ;
TIMES     : '*'  ;
EOP       : '^';

// Reserved

BOOL     : 'Bool'   ;
INT      : 'Int'    ;
VEC      : 'Vec'    ;
TRUE     : 'True'   ;
FALSE    : 'False'  ;
AND      : 'and'    ;
ELSE     : 'else'   ;
FUN      : 'fun'    ;
IF       : 'if'     ;
NOT      : 'not'    ;
OR       : 'or'     ;
RETURN   : 'return' ;
WHILE    : 'while'  ;

// Identifiers
// Having them here guarantees that no reserved word can be confused with an identifier
LOWERID : LowChar (Char)* ;  // EXP: LowName. Reserved ids were first, so they didn't match here
UPPERID : UppChar (Char)* ;  // EXP: UppName. Reserved ids were first, so they didn't match here
fragment Char : LowChar | UppChar | Digit | IdSymbol ; // EXP: BasicChar
fragment LowChar  : [a-z] ;
fragment UppChar  : [A-Z] ;
fragment IdSymbol : [\'_] ;

// Numbers
NUM : Digit | NonZeroDigit (Digit)+ ;   // EXP: Digits
fragment Digit : [0] | NonZeroDigit ;
fragment NonZeroDigit : [1-9] ;

