grammar CucarachaGrammar;
import CucarachaLexer;

@header {
    package ar.edu.unq.parse.tp1;
}

program : (function)* (EOF)?;
function : FUN ID params (COLON type)? block;

params: LPAREN (param (COMMA param)*)? RPAREN;
param: ID COLON type;

block : LBRACE ( instr_assign
                | instr_vecassign
                | instr_if
                | instr_while
                | instr_return
                | instr_call
                )* RBRACE ;

instr_assign:    ID ASSIGN expr;
instr_vecassign: ID LBRACK expr RBRACK ASSIGN expr;
instr_if:        IF expr block (ELSE block)?;
instr_while:     WHILE expr block;
instr_return:    RETURN expr;
instr_call:      ID LPAREN expr_list RPAREN;
expr_list:       (expr (COMMA expr)*)?;

expr: expr AND expr_logic_atomic
        | expr OR expr_logic_atomic
        | expr_logic_atomic;

expr_logic_atomic: NOT expr_logic_atomic
                    | expr_rel;

expr_rel: expr_math_term op=LE expr_math_term
            | expr_math_term op=GE expr_math_term
            | expr_math_term op=LT expr_math_term
            | expr_math_term op=GT expr_math_term
            | expr_math_term op=EQ expr_math_term
            | expr_math_term op=NE expr_math_term
            | expr_math_term;

expr_math_term: expr_math_term op=PLUS expr_math_mul
                | expr_math_term op=MINUS expr_math_mul
                | expr_math_mul;

expr_math_mul: expr_math_mul TIMES expr_atom
                | expr_atom;

expr_atom: expr_variable
            | expr_literal_num
            | expr_literal_bool
            | expr_vec_cons
            | expr_vec_len
            | expr_vec_deref
            | expr_call
            | LPAREN expr RPAREN;

expr_variable:      ID;
expr_literal_num:   NUM;
expr_literal_bool:  TRUE | FALSE;
expr_vec_cons:      LBRACK expr_list RBRACK;
expr_vec_len:       HASH ID;
expr_vec_deref:     ID LBRACK expr RBRACK;
expr_call:      ID LPAREN expr_list RPAREN;
type:               INT | BOOL | VEC;
