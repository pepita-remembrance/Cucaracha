package ar.edu.unq.parse.tp1.ast

import ar.edu.unq.parse.tp1.CucarachaGrammarParser._
import ar.edu.unq.parse.tp1.{CucarachaGrammarBaseVisitor, CucarachaGrammarParser}
import org.antlr.v4.runtime.tree.ParseTree

import scala.collection.JavaConversions._

object ASTifier extends CucarachaGrammarBaseVisitor[ASTNode] {

  override def visitProgram(ctx: ProgramContext): Program = {
    val functions = ctx.function.map(visitFunction)
    Program(functions)
  }

  override def visitFunction(ctx: FunctionContext): CucaFunction = {
    val id = ctx.ID().getText
    val params = ctx.params.param.map(visitParam)
    val body = ctx.block().instructions().children.collect(visitInstruction)
    val returnType = visitType(ctx.`type`())
    CucaFunction(id, params, body, returnType)
  }

  override def visitParam(ctx: ParamContext): Param = {
    val id = ctx.ID().getText
    val cucaType = visitType(ctx.`type`())
    Param(id, cucaType)
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

  override def visitInstr_assign(ctx: Instr_assignContext): Assign = {
    val id = ctx.ID().getText
    val expression = visitExpr(ctx.expr())
    Assign(id, expression)
  }

  override def visitInstr_vecassign(ctx: Instr_vecassignContext): VectorAssign = {
    val id = ctx.ID().getText
    val position = visitExpr(ctx.expr(0))
    val value = visitExpr(ctx.expr(1))
    VectorAssign(id, position, value)
  }

  override def visitInstr_if(ctx: Instr_ifContext): IfThenElse = {
    val condition = visitExpr(ctx.expr())
    val branchTrue = ctx.block(0).instructions().children.collect(visitInstruction)
    val falseBlock = ctx.block(1)
    val branchFalse = if (falseBlock == null) List.empty[Instruction]
    else {
      falseBlock.instructions().children.collect(visitInstruction)
    }

    IfThenElse(condition, branchTrue, branchFalse)
  }

  override def visitInstr_while(ctx: Instr_whileContext): While = {
    val condition = visitExpr(ctx.expr())
    val body = ctx.block().instructions().children.collect(visitInstruction)
    While(condition, body)
  }

  override def visitInstr_return(ctx: Instr_returnContext): Return = {
    val value = visitExpr(ctx.expr())
    Return(value)
  }

  override def visitInstr_call(ctx: Instr_callContext): Call = {
    val id = ctx.ID().getText
    val params = ctx.expr_list().expr().map(visitExpr)
    Call(id, params)
  }

  override def visitExpr(ctx: ExprContext): Expression = {
    if (ctx.AND() != null)
      LogicAnd(visitExpr(ctx.expr()), visitExpr_logic(ctx.expr_logic()))
    else
      visitExpr_logic(ctx.expr_logic())
  }

  override def visitExpr_logic(ctx: Expr_logicContext): Expression = {
    if (ctx.OR() != null)
      LogicOr(visitExpr_logic(ctx.expr_logic()), visitExpr_logic_atomic(ctx.expr_logic_atomic()))
    else
      visitExpr_logic_atomic(ctx.expr_logic_atomic())
  }

  override def visitExpr_logic_atomic(ctx: Expr_logic_atomicContext): Expression = {
    if (ctx.NOT() != null)
      LogicNot(visitExpr_logic_atomic(ctx.expr_logic_atomic()))
    else
      visitExpr_rel(ctx.expr_rel())
  }

  override def visitExpr_rel(ctx: Expr_relContext): Expression = {
    if (ctx.LE() != null)
      RelLE(visitExpr_math_term(ctx.expr_math_term(0)), visitExpr_math_term(ctx.expr_math_term(1)))
    else if (ctx.GE() != null)
      RelGE(visitExpr_math_term(ctx.expr_math_term(0)), visitExpr_math_term(ctx.expr_math_term(1)))
    else if (ctx.LT() != null)
      RelLT(visitExpr_math_term(ctx.expr_math_term(0)), visitExpr_math_term(ctx.expr_math_term(1)))
    else if (ctx.GT() != null)
      RelGT(visitExpr_math_term(ctx.expr_math_term(0)), visitExpr_math_term(ctx.expr_math_term(1)))
    else if (ctx.EQ() != null)
      RelEQ(visitExpr_math_term(ctx.expr_math_term(0)), visitExpr_math_term(ctx.expr_math_term(1)))
    else if (ctx.NE() != null)
      RelNE(visitExpr_math_term(ctx.expr_math_term(0)), visitExpr_math_term(ctx.expr_math_term(1)))
    else
      visitExpr_math_term(ctx.expr_math_term(0))
  }

  override def visitExpr_math_term(ctx: Expr_math_termContext): Expression = {
    if (ctx.PLUS() != null)
      MathPlus(visitExpr_math_term(ctx.expr_math_term()), visitExpr_math_mul(ctx.expr_math_mul()))
    else if (ctx.MINUS() != null)
      MathMinus(visitExpr_math_term(ctx.expr_math_term()), visitExpr_math_mul(ctx.expr_math_mul()))
    else
      visitExpr_math_mul(ctx.expr_math_mul())
  }

  override def visitExpr_math_mul(ctx: Expr_math_mulContext): Expression = {
    if (ctx.TIMES() != null)
      MathTimes(visitExpr_math_mul(ctx.expr_math_mul()), visitExpr_atom(ctx.expr_atom()))
    else
      visitExpr_atom(ctx.expr_atom())
  }

  override def visitExpr_atom(ctx: Expr_atomContext): Expression = {
    if (ctx.LPAREN() != null)
      visitExpr(ctx.expr())
    else ctx.children.head match {
      case c: Expr_variableContext => visitExpr_variable(c)
      case c: Expr_literal_numContext => visitExpr_literal_num(c)
      case c: Expr_literal_boolContext => visitExpr_literal_bool(c)
      case c: Expr_vec_consContext => visitExpr_vec_cons(c)
      case c: Expr_vec_lenContext => visitExpr_vec_len(c)
      case c: Expr_vec_derefContext => visitExpr_vec_deref(c)
    }
  }

  override def visitExpr_variable(ctx: Expr_variableContext): Variable =
    Variable(ctx.ID().getText)

  override def visitExpr_literal_num(ctx: Expr_literal_numContext): ConstantInt =
    ConstantInt(ctx.NUM().getText.toInt)

  override def visitExpr_literal_bool(ctx: Expr_literal_boolContext): ConstantBool =
    if (ctx.TRUE() != null) ConstantBool(true)
    else ConstantBool(false)

  override def visitExpr_vec_cons(ctx: Expr_vec_consContext): Vector =
    Vector(ctx.expr_list().expr().map(visitExpr))

  override def visitExpr_vec_len(ctx: Expr_vec_lenContext): VectorLength =
    VectorLength(ctx.ID().getText)

  override def visitExpr_vec_deref(ctx: Expr_vec_derefContext): VectorDeref =
    VectorDeref(ctx.ID().getText, visitExpr(ctx.expr()))
}
