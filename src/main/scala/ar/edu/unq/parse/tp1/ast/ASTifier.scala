package ar.edu.unq.parse.tp1.ast

import ar.edu.unq.parse.tp1.CucarachaGrammarParser._
import ar.edu.unq.parse.tp1.{CucarachaGrammarBaseVisitor, CucarachaGrammarParser}
import org.antlr.v4.runtime.tree.ParseTree

import scala.collection.JavaConversions._

class ASTifier extends CucarachaGrammarBaseVisitor[ASTNode] {
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

  override def visitExpr(ctx: ExprContext): Expression = ???

  override def visitExpr_variable(ctx: Expr_variableContext): Variable = ???

  override def visitExpr_literal_num(ctx: Expr_literal_numContext): ConstantInt = ???

  override def visitExpr_literal_bool(ctx: Expr_literal_boolContext): ConstantBool = ???

  override def visitExpr_vec_cons(ctx: Expr_vec_consContext): Vector = ???

  override def visitExpr_vec_len(ctx: Expr_vec_lenContext): VectorLength = ???

  override def visitExp_vec_deref(ctx: Exp_vec_derefContext): VectorDeref = ???
}
