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

//expr: ( expr_variable
//      | expr_literal_num
//      | expr_literal_bool
//      | expr_vec_cons
//      | expr_vec_len
//      | exp_vec_deref
//      | instr_call
//      | NOT expr
//) (bin_op expr)?;

expr: unary_expr | expr bin_op expr;

unary_expr: NOT expr
            | LPAREN expr RPAREN
            | expr_variable
            | expr_literal_num
            | expr_literal_bool
            | expr_vec_cons
            | expr_vec_len
            | exp_vec_deref
            | instr_call;

expr_variable:      ID;
expr_literal_num:   NUM;
expr_literal_bool:  TRUE | FALSE;
expr_vec_cons:      LBRACK expr_list RBRACK;
expr_vec_len:       HASH ID;
exp_vec_deref:      ID LBRACK expr RBRACK;
bin_op :            AND | OR | PLUS | MINUS | TIMES | LE | GE | LT | GT | EQ | NE ;
type:               INT | BOOL | VEC;
