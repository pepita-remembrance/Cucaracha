grammar CucarachaGrammar;
import CucarachaLexer;

@header {
    package ar.edu.unq.parse.tp1;
}

program : (function)* (EOF)?;
function : FUN ID params (COLON type)? block;

params: LPAREN (param (COMMA param)*)? RPAREN;
param: ID COLON type;

block : LBRACE instructions RBRACE;
instructions :  ( instr_assign
                | instr_vecassign
                | instr_if
                | instr_while
                | instr_return
                | instr_call
                )* ;

instr_assign:    ID ASSIGN expr;
instr_vecassign: ID LBRACK expr RBRACK ASSIGN expr;
instr_if:        IF expr block (ELSE block)?;
instr_while:     WHILE expr block;
instr_return:    RETURN expr;
instr_call:      ID LPAREN expr_list RPAREN;
expr_list:       (expr (COMMA expr)*)?;

expr: expr AND expr_logic
        | expr_logic;

expr_logic: expr_logic OR expr_logic_atomic
                | expr_logic_atomic;

expr_logic_atomic: NOT expr_logic_atomic
                    | expr_rel;

expr_rel: expr_math_term LE expr_math_term
            | expr_math_term GE expr_math_term
            | expr_math_term LT expr_math_term
            | expr_math_term GT expr_math_term
            | expr_math_term EQ expr_math_term
            | expr_math_term NE expr_math_term
            | expr_math_term;

expr_math_term: expr_math_term PLUS expr_math_mul
                | expr_math_term MINUS expr_math_mul
                | expr_math_mul;

expr_math_mul: expr_math_mul TIMES expr_atom
                | expr_atom;

expr_atom: expr_variable
            | expr_literal_num
            | expr_literal_bool
            | expr_vec_cons
            | expr_vec_len
            | expr_vec_deref
            | instr_call
            | LPAREN expr RPAREN;

expr_variable:      ID;
expr_literal_num:   NUM;
expr_literal_bool:  TRUE | FALSE;
expr_vec_cons:      LBRACK expr_list RBRACK;
expr_vec_len:       HASH ID;
expr_vec_deref:     ID LBRACK expr RBRACK;
type:               INT | BOOL | VEC;
