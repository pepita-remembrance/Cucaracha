package ar.edu.unq.parse.tp1.ast

import ar.edu.unq.parse.tp1.CucarachaGrammarParser._
import ar.edu.unq.parse.tp1.{CucarachaGrammarBaseVisitor, CucarachaGrammarParser}
import org.antlr.v4.runtime.tree.ParseTree

import scala.collection.JavaConversions._

object ASTifier extends CucarachaGrammarBaseVisitor[ASTTree] {

  override def visitProgram(ctx: ProgramContext): Program = {
    Program(functions = ctx.function.map(visitFunction))
  }

  override def visitFunction(ctx: FunctionContext): CucaFunction = {
    CucaFunction(
      id = ctx.ID().getText,
      params = ctx.params.param.map(visitParam),
      body = ctx.block().children.collect(visitInstruction),
      returnType = visitType(ctx.`type`())
    )
  }

  override def visitParam(ctx: ParamContext): Parameter = {
    Parameter(
      id = ctx.ID().getText,
      paramType = visitType(ctx.`type`())
    )
  }

  override def visitType(ctx: TypeContext): CucaTypes.Type = {
    if (ctx == null) CucaTypes.CucaUnit
    else ctx.getText match {
      case "Int" => CucaTypes.CucaInt
      case "Bool" => CucaTypes.CucaBool
      case "Vec" => CucaTypes.CucaVec
    }
  }

  def visitInstruction: PartialFunction[ParseTree, Instruction] = {
    import CucarachaGrammarParser._
    {
      case i: Instr_assignContext => visitInstr_assign(i)
      case i: Instr_vecassignContext => visitInstr_vecassign(i)
      case i: Instr_ifContext => visitInstr_if(i)
      case i: Instr_whileContext => visitInstr_while(i)
      case i: Instr_returnContext => visitInstr_return(i)
      case i: Instr_callContext => visitInstr_call(i)
    }
  }

  override def visitInstr_assign(ctx: Instr_assignContext): StmtAssign = {
    StmtAssign(
      id = ctx.ID().getText,
      value = visitExpr(ctx.expr())
    )
  }

  override def visitInstr_vecassign(ctx: Instr_vecassignContext): StmtVecAssign = {
    StmtVecAssign(
      id = ctx.ID().getText,
      position = visitExpr(ctx.expr(0)),
      value = visitExpr(ctx.expr(1))
    )
  }

  override def visitInstr_if(ctx: Instr_ifContext): StatementIf = {
    ctx.block(1) match {
      case null => StmtIf(
        condition = visitExpr(ctx.expr()),
        branchTrue = ctx.block(0).children.collect(visitInstruction))
      case block => StmtIfElse(
        condition = visitExpr(ctx.expr()),
        branchTrue = ctx.block(0).children.collect(visitInstruction),
        branchFalse = block.children.collect(visitInstruction))
    }
  }

  override def visitInstr_while(ctx: Instr_whileContext): StmtWhile = {
    StmtWhile(
      condition = visitExpr(ctx.expr()),
      body = ctx.block().children.collect(visitInstruction)
    )
  }

  override def visitInstr_return(ctx: Instr_returnContext): StmtReturn = {
    StmtReturn(visitExpr(ctx.expr()))
  }

  override def visitInstr_call(ctx: Instr_callContext): StmtCall = {
    StmtCall(
      id = ctx.ID().getText,
      params = ctx.expr_list().expr().map(visitExpr)
    )
  }

  override def visitExpr(ctx: ExprContext): Expression = {
    if (ctx.AND() != null)
      ExprAnd(visitExpr(ctx.expr()), visitExpr_logic_atomic(ctx.expr_logic_atomic()))
    else if (ctx.OR() != null)
      ExprOr(visitExpr(ctx.expr()), visitExpr_logic_atomic(ctx.expr_logic_atomic()))
    else
      visitExpr_logic_atomic(ctx.expr_logic_atomic())
  }

  override def visitExpr_logic_atomic(ctx: Expr_logic_atomicContext): Expression = {
    if (ctx.NOT() != null)
      ExprNot(visitExpr_logic_atomic(ctx.expr_logic_atomic()))
    else
      visitExpr_rel(ctx.expr_rel())
  }

  override def visitExpr_rel(ctx: Expr_relContext): Expression = {
    if (ctx.op == null) return visitExpr_math_term(ctx.expr_math_term(0))
    (ctx.op.getText match {
      case "<=" => ExprLe
      case ">=" => ExprGe
      case "<" => ExprLt
      case ">" => ExprGt
      case "==" => ExprEq
      case "!=" => ExprNe
    }) (visitExpr_math_term(ctx.expr_math_term(0)), visitExpr_math_term(ctx.expr_math_term(1)))
  }

  override def visitExpr_math_term(ctx: Expr_math_termContext): Expression = {
    if (ctx.op == null) return visitExpr_math_mul(ctx.expr_math_mul())
    (ctx.op.getText match {
      case "+" => ExprAdd
      case "-" => ExprSub
    }) (visitExpr_math_term(ctx.expr_math_term()), visitExpr_math_mul(ctx.expr_math_mul()))
  }

  override def visitExpr_math_mul(ctx: Expr_math_mulContext): Expression = {
    if (ctx.TIMES() != null)
      ExprMul(visitExpr_math_mul(ctx.expr_math_mul()), visitExpr_atom(ctx.expr_atom()))
    else
      visitExpr_atom(ctx.expr_atom())
  }

  override def visitExpr_atom(ctx: Expr_atomContext): Expression = {
    if (ctx.LPAREN() != null) return visitExpr(ctx.expr())
    ctx.children.head match {
      case c: Expr_variableContext => visitExpr_variable(c)
      case c: Expr_literal_numContext => visitExpr_literal_num(c)
      case c: Expr_literal_boolContext => visitExpr_literal_bool(c)
      case c: Expr_vec_consContext => visitExpr_vec_cons(c)
      case c: Expr_vec_lenContext => visitExpr_vec_len(c)
      case c: Expr_vec_derefContext => visitExpr_vec_deref(c)
      case c: Expr_callContext => visitExpr_call(c)
    }
  }

  override def visitExpr_variable(ctx: Expr_variableContext): ExprVar =
    ExprVar(ctx.ID().getText)

  override def visitExpr_literal_num(ctx: Expr_literal_numContext): ExprConstNum =
    ExprConstNum(ctx.NUM().getText.toInt)

  override def visitExpr_literal_bool(ctx: Expr_literal_boolContext): ExprConstBool =
    ExprConstBool(ctx.TRUE() != null)

  override def visitExpr_vec_cons(ctx: Expr_vec_consContext): ExprVecMake =
    ExprVecMake(ctx.expr_list().expr().map(visitExpr))

  override def visitExpr_vec_len(ctx: Expr_vec_lenContext): ExprVecLength =
    ExprVecLength(ctx.ID().getText)

  override def visitExpr_vec_deref(ctx: Expr_vec_derefContext): ExprVecDeref =
    ExprVecDeref(ctx.ID().getText, visitExpr(ctx.expr()))

  override def visitExpr_call(ctx: Expr_callContext): ExprCall =
    ExprCall(
      id = ctx.ID().getText,
      params = ctx.expr_list().expr().map(visitExpr)
    )
}
