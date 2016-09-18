package ar.edu.unq.parse.tp1.ast

import ar.edu.unq.parse.tp1.ast.CucaTypes._
import ar.edu.unq.parse.tp1.semantics.{DefaultSemanticChecker, SemanticChecker, TypeException}

class IndentableStringBuilder(indentStep: String) {
  val builder = new StringBuilder
  private var indentLevel = 0

  def appendln(newLine: String): Unit = builder.++=(indentStep * indentLevel).++=(newLine).++=("\n")

  def indent() = indentLevel += 1

  def dedent() = indentLevel = Math.max(indentLevel - 1, 0)

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

  def semanticCheck(checker: SemanticChecker = DefaultSemanticChecker): Unit = {
    checker.checkProgram(this)
  }
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

trait Instruction extends ASTTree {
  def checkType(): Type = throw new NotImplementedError
}

case class StmtAssign(id: String, value: Expression) extends Instruction {
  def serializeContents(builder: IndentableStringBuilder): Unit = {
    builder.appendln(id)
    value.serialize(builder)
  }

  override def checkType(): Type = value.infer

}

case class StmtVecAssign(id: String, position: Expression, value: Expression) extends Instruction {
  def serializeContents(builder: IndentableStringBuilder): Unit = {
    builder.appendln(id)
    position.serialize(builder)
    value.serialize(builder)
  }

  override def checkType(): Type = value <===> position <===> CucaInt
}

trait StatementIf extends Instruction {
  def condition: Expression

  def branchTrue: Seq[Instruction]

  def serializeContents(builder: IndentableStringBuilder): Unit = {
    condition.serialize(builder)
    wrapInBlock(builder, branchTrue)
  }

  override def checkType(): Type = condition <===> CucaBool
}

case class StmtIf(condition: Expression, branchTrue: Seq[Instruction]) extends StatementIf

case class StmtIfElse(condition: Expression, branchTrue: Seq[Instruction], branchFalse: Seq[Instruction]) extends StatementIf {
  override def serializeContents(builder: IndentableStringBuilder): Unit = {
    super.serializeContents(builder)
    wrapInBlock(builder, branchFalse)
  }
}

case class StmtWhile(condition: Expression, body: Seq[Instruction]) extends Instruction {
  def serializeContents(builder: IndentableStringBuilder): Unit = {
    condition.serialize(builder)
    wrapInBlock(builder, body)
  }

  override def checkType(): Type = condition <===> CucaBool
}

case class StmtReturn(value: Expression) extends Instruction {
  def serializeContents(builder: IndentableStringBuilder): Unit = value.serialize(builder)

  override def checkType(): Type = throw new NotImplementedError
}

case class StmtCall(id: String, params: Seq[Expression]) extends Instruction {
  def serializeContents(builder: IndentableStringBuilder): Unit = {
    builder.appendln(id)
    params.foreach(_.serialize(builder))
  }

  override def checkType(): Type = throw new NotImplementedError
}

trait Expression extends ASTTree {
  def infer: Type = throw new NotImplementedError

  def checkType: Type = throw new NotImplementedError
}

case class ExprVar(id: String) extends Expression {
  def serializeContents(builder: IndentableStringBuilder): Unit = builder.appendln(id)

  override def checkType: Type = throw new NotImplementedError
}

case class ExprConstNum(value: Int) extends Expression {
  def serializeContents(builder: IndentableStringBuilder): Unit = builder.appendln(value.toString)

  override def infer: Type = CucaInt
}

case class ExprConstBool(value: Boolean) extends Expression {
  def serializeContents(builder: IndentableStringBuilder): Unit = builder.appendln(value.toString.capitalize)

  override def infer: Type = CucaBool
}

case class ExprVecMake(values: Seq[Expression]) extends Expression {
  def serializeContents(builder: IndentableStringBuilder): Unit = values.foreach(_.serialize(builder))

  override def infer: Type = {
    values.foreach(CucaInt <===> _)
    CucaVec
  }
}

case class ExprVecLength(id: String) extends Expression {
  def serializeContents(builder: IndentableStringBuilder): Unit = builder.appendln(id)

  override def infer: Type = CucaInt
}

case class ExprVecDeref(id: String, position: Expression) extends Expression {
  def serializeContents(builder: IndentableStringBuilder): Unit = {
    builder.appendln(id)
    position.serialize(builder)
  }

  override def infer: Type = CucaInt
}

case class ExprCall(id: String, params: Seq[Expression]) extends Expression {
  def serializeContents(builder: IndentableStringBuilder): Unit = {
    builder.appendln(id)
    params.foreach(_.serialize(builder))
  }

  override def checkType: Type = throw new NotImplementedError
}

case class ExprNot(expr: Expression) extends Expression {
  def serializeContents(builder: IndentableStringBuilder): Unit = expr.serialize(builder)
}

object CucaTypes {

  sealed trait Type extends ASTTree {
    override def key: String = super.key.stripSuffix("$")

    def serializeContents(builder: IndentableStringBuilder): Unit = {}

    def <===>(other: Type): Type = {
      if (this != other) throw TypeException(s"Type $key does not match ${other.key}")
      this
    }
  }

  case object CucaUnit extends Type

  case object CucaInt extends Type

  case object CucaBool extends Type

  case object CucaVec extends Type

  implicit def exprToType(expr: Expression): Type = expr.infer
}