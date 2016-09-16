package ar.edu.unq.parse.tp1.ast

import ar.edu.unq.parse.tp1.ast.CucaTypes.Type

class IndentableStringBuilder(indentStep: String) {
  val builder = new StringBuilder
  private var indentLevel = 0

  def appendln(newLine: String): Unit = builder.++=(indentStep * indentLevel).++=(newLine).++=("\n")

  def indent() = indentLevel += 1

  def dedent() = indentLevel = Math.min(indentLevel - 1, 0)

  override def toString = builder.toString

}

trait ASTTree {

  def key: String = getClass.getSimpleName.stripPrefix("Cuca")

  def serialize: String = {
    val builder = new IndentableStringBuilder("  ")
    serialize(builder)
    builder.toString
  }

  def serialize(builder: IndentableStringBuilder) = {
    builder.appendln("(" + key)
    builder.indent()
    serializeContents(builder)
    builder.dedent()
    builder.appendln(")")
  }

  def wrapInBlock(builder: IndentableStringBuilder, items: Seq[ASTTree]) = {
    builder.appendln("(Block")
    builder.indent()
    items.foreach(_.serialize(builder))
    builder.dedent()
    builder.appendln(")")
  }

  def serializeContents(builder: IndentableStringBuilder): Unit

}

case class Program(functions: Seq[CucaFunction]) extends ASTTree {
  def serializeContents(builder: IndentableStringBuilder): Unit =
    functions.foreach(_.serialize(builder))
}

case class CucaFunction(id: String, params: Seq[Parameter], body: Seq[Instruction], returnType: Type) extends ASTTree {
  def serializeContents(builder: IndentableStringBuilder): Unit = {
    builder.appendln(id)
    builder.appendln(returnType.key)
    params.foreach(_.serialize(builder))
    wrapInBlock(builder, body)
  }
}

case class Parameter(id: String, paramType: Type) extends ASTTree {
  def serializeContents(builder: IndentableStringBuilder): Unit = {
    builder.appendln(id)
    builder.appendln(paramType.key)
  }
}

trait Instruction extends ASTTree

case class StmtAssign(id: String, value: Expression) extends Instruction {
  def serializeContents(builder: IndentableStringBuilder): Unit = {
    builder.appendln(id)
    value.serialize(builder)
  }
}

case class StmtVecAssign(id: String, position: Expression, value: Expression) extends Instruction {
  def serializeContents(builder: IndentableStringBuilder): Unit = {
    builder.appendln(id)
    position.serialize(builder)
    value.serialize(builder)
  }
}

case class StmtIfElse(condition: Expression, branchTrue: Seq[Instruction], branchFalse: Seq[Instruction]) extends Instruction {
  def serializeContents(builder: IndentableStringBuilder): Unit = {
    condition.serialize(builder)
    wrapInBlock(builder, branchTrue)
    wrapInBlock(builder, branchFalse)
  }
}

case class StmtWhile(condition: Expression, body: Seq[Instruction]) extends Instruction {
  def serializeContents(builder: IndentableStringBuilder): Unit = {
    condition.serialize(builder)
    wrapInBlock(builder, body)
  }
}

case class StmtReturn(value: Expression) extends Instruction {
  def serializeContents(builder: IndentableStringBuilder): Unit = value.serialize(builder)
}

case class StmtCall(id: String, params: Seq[Expression]) extends Instruction with Expression {
  def serializeContents(builder: IndentableStringBuilder): Unit = {
    builder.appendln(id)
    params.foreach(_.serialize(builder))
  }
}

trait Expression extends ASTTree

case class ExprVar(id: String) extends Expression {
  def serializeContents(builder: IndentableStringBuilder): Unit = builder.appendln(id)
}

case class ExprConstNum(value: Int) extends Expression {
  def serializeContents(builder: IndentableStringBuilder): Unit = builder.appendln(value.toString)
}

case class ExprConstBool(value: Boolean) extends Expression {
  def serializeContents(builder: IndentableStringBuilder): Unit = builder.appendln(value.toString)
}

case class ExprVecMake(values: Seq[Expression]) extends Expression {
  def serializeContents(builder: IndentableStringBuilder): Unit = values.foreach(_.serialize(builder))
}

case class ExprVecLength(id: String) extends Expression {
  def serializeContents(builder: IndentableStringBuilder): Unit = builder.appendln(id)
}

case class ExprVecDeref(id: String, position: Expression) extends Expression {
  def serializeContents(builder: IndentableStringBuilder): Unit = {
    builder.appendln(id)
    position.serialize(builder)
  }
}

case class ExprNot(expr: Expression) extends Expression {
  def serializeContents(builder: IndentableStringBuilder): Unit = expr.serialize(builder)
}

class BinaryExpression(expr1: Expression, expr2: Expression) extends Expression {
  def serializeContents(builder: IndentableStringBuilder): Unit = {
    expr1.serialize(builder)
    expr2.serialize(builder)
  }
}

case class ExprAnd(expr1: Expression, expr2: Expression) extends BinaryExpression(expr1, expr2)

case class ExprOr(expr1: Expression, expr2: Expression) extends BinaryExpression(expr1, expr2)

case class ExprAdd(expr1: Expression, expr2: Expression) extends BinaryExpression(expr1, expr2)

case class ExprSub(expr1: Expression, expr2: Expression) extends BinaryExpression(expr1, expr2)

case class ExprMul(expr1: Expression, expr2: Expression) extends BinaryExpression(expr1, expr2)

case class ExprLe(expr1: Expression, expr2: Expression) extends BinaryExpression(expr1, expr2)

case class ExprGe(expr1: Expression, expr2: Expression) extends BinaryExpression(expr1, expr2)

case class ExprLt(expr1: Expression, expr2: Expression) extends BinaryExpression(expr1, expr2)

case class ExprGt(expr1: Expression, expr2: Expression) extends BinaryExpression(expr1, expr2)

case class ExprEq(expr1: Expression, expr2: Expression) extends BinaryExpression(expr1, expr2)

case class ExprNe(expr1: Expression, expr2: Expression) extends BinaryExpression(expr1, expr2)

object CucaTypes {

  sealed trait Type extends ASTTree {
    def serializeContents(builder: IndentableStringBuilder): Unit = {}
  }

  case object CucaUnit extends Type

  case object CucaInt extends Type

  case object CucaBool extends Type

  case object CucaVec extends Type

}
