package ar.edu.unq.parse.tp1.ast

import ar.edu.unq.parse.tp1.{Context, IndentableStringBuilder, PredefinedFunctions}
import ar.edu.unq.parse.tp1.ast.CucaTypes._
import ar.edu.unq.parse.tp1.ast.expressions.Expression
import ar.edu.unq.parse.tp1.ast.statements.{Statement, StmtReturn}
import ar.edu.unq.parse.tp1.semantics.{DefaultSemantics, SemanticChecker, TypeException}

import scala.collection.mutable

trait ASTTree {

  def key: String = getClass.getSimpleName.stripPrefix("Cuca")

  def serialize: String = {
    val builder = new IndentableStringBuilder("  ")
    serialize(builder)
    builder.toString
  }

  def serialize(builder: IndentableStringBuilder) = {
    builder.appendln("(" + key)
      .indent()
    serializeContents(builder)
    builder.dedent()
      .appendln(")")
  }

  def wrapInBlock(builder: IndentableStringBuilder, items: Seq[ASTTree]) = {
    builder.appendln("(Block")
      .indent()
    items.foreach(_.serialize(builder))
    builder.dedent()
      .appendln(")")
  }

  def serializeContents(builder: IndentableStringBuilder): Unit


  def fetch[T <: AnyRef](id: String)(implicit programContext: Context[CucaFunction], localContext: Context[Any]): T = localContext(id).asInstanceOf[T]
}

case class Program(functions: Seq[CucaFunction]) extends ASTTree {
  def serializeContents(builder: IndentableStringBuilder): Unit =
    functions.foreach(_.serialize(builder))

  def semanticCheck(checker: SemanticChecker = DefaultSemantics): Unit = {
    checker.check(this)
  }

  def execute(): Unit = {
    val programContext = new Context[CucaFunction](funID => s"Function $funID is undefined")
    Seq(PredefinedFunctions.PutChar, PredefinedFunctions.PutNum).foreach(fun => programContext(fun.id) = fun)
    this.functions.foreach(fun => programContext(fun.id) = fun)
    functions.filter(_.id == "main").head.evalWith(Seq.empty)(programContext)
  }
}

case class CucaFunction(id: String, params: Seq[Parameter], body: Seq[Statement], returnType: Type) extends ASTTree {

  def serializeContents(builder: IndentableStringBuilder): Unit = {
    builder.appendln(id)
      .appendln(returnType.key)
    params.foreach(_.serialize(builder))
    wrapInBlock(builder, body)
  }

  def evalWith(values: Seq[Any])(implicit programContext: Context[CucaFunction]): Any = {
    implicit val localContext = new Context[Any](m => s"Error inesperado: $m")
    localContext ++= params.map(_.id).zip(values)
    body.foreach(_.iterpret)
    returnType match {
      case CucaUnit => CucaUnit
      case x => body.last.asInstanceOf[StmtReturn].value.eval
    }
  }
}

case class Parameter(id: String, paramType: Type) extends ASTTree {
  def serializeContents(builder: IndentableStringBuilder): Unit = {
    builder.appendln(id)
      .appendln(paramType.key)
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

  type InternalVec = scala.collection.mutable.MutableList[Int]

  case object CucaUnit extends Type

  case object CucaInt extends Type

  case object CucaBool extends Type

  case object CucaVec extends Type

  implicit def exprToType(expr: Expression)(implicit programContext: Context[CucaFunction], localContext: Context[Type]): Type = expr.infer
}