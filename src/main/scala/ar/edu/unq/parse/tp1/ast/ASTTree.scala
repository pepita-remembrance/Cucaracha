package ar.edu.unq.parse.tp1.ast

import ar.edu.unq.parse.tp1.ast.CucaTypes._
import ar.edu.unq.parse.tp1.ast.expressions.Expression
import ar.edu.unq.parse.tp1.ast.statements.Statement
import ar.edu.unq.parse.tp1.semantics.{Context, DefaultSemantics, SemanticChecker, TypeException}

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

  def semanticCheck(checker: SemanticChecker = DefaultSemantics): Unit = {
    checker.checkProgram(this)
  }
}

case class CucaFunction(id: String, params: Seq[Parameter], body: Seq[Statement], returnType: Type) extends ASTTree {
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

  implicit def exprToType(expr: Expression)(implicit programContext: Context[CucaFunction], localContext: Context[Type]): Type = expr.infer
}