lexer grammar CucarachaLexer;

// Comments will not be part of the AST. That can be a nuisance to print the program back...
WS : ([ \t]|NEWLINE|Comment)+ -> skip ;
fragment NEWLINE :'\r'? '\n' ; // The optional \r is to care for MS coding of newlines.
fragment Comment : '/*' .*? '*/' ; // The ? after .* makes it non-greedy (TDA4R,Ch.5,p.76)


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


ID : [_a-zA-Z][_a-zA-Z0-9]*;
NUM : [0-9]+;