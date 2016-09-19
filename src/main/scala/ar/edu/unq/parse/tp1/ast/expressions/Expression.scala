package ar.edu.unq.parse.tp1.ast.expressions

import ar.edu.unq.parse.tp1.ast.CucaTypes._
import ar.edu.unq.parse.tp1.ast.{ASTTree, CucaFunction, IndentableStringBuilder}
import ar.edu.unq.parse.tp1.semantics.Context

trait Expression extends ASTTree {
  def infer(implicit programContext: Context[CucaFunction], localContext: Context[Type]): Type
}


case class ExprVar(id: String) extends Expression {
  def serializeContents(builder: IndentableStringBuilder): Unit = builder.appendln(id)

  def infer(implicit programContext: Context[CucaFunction], localContext: Context[Type]): Type = localContext(id)
}

case class ExprConstNum(value: Int) extends Expression {
  def serializeContents(builder: IndentableStringBuilder): Unit = builder.appendln(value.toString)

  def infer(implicit programContext: Context[CucaFunction], localContext: Context[Type]): Type = CucaInt
}

case class ExprConstBool(value: Boolean) extends Expression {
  def serializeContents(builder: IndentableStringBuilder): Unit = builder.appendln(value.toString.capitalize)

  def infer(implicit programContext: Context[CucaFunction], localContext: Context[Type]): Type = CucaBool
}

case class ExprVecMake(values: Seq[Expression]) extends Expression {
  def serializeContents(builder: IndentableStringBuilder): Unit = values.foreach(_.serialize(builder))

  def infer(implicit programContext: Context[CucaFunction], localContext: Context[Type]): Type = CucaVec
}

case class ExprVecLength(id: String) extends Expression {
  def serializeContents(builder: IndentableStringBuilder): Unit = builder.appendln(id)

  def infer(implicit programContext: Context[CucaFunction], localContext: Context[Type]): Type = CucaInt
}

case class ExprVecDeref(id: String, position: Expression) extends Expression {
  def serializeContents(builder: IndentableStringBuilder): Unit = {
    builder.appendln(id)
    position.serialize(builder)
  }

  def infer(implicit programContext: Context[CucaFunction], localContext: Context[Type]): Type = CucaInt
}

case class ExprCall(id: String, params: Seq[Expression]) extends Expression {
  def serializeContents(builder: IndentableStringBuilder): Unit = {
    builder.appendln(id)
    params.foreach(_.serialize(builder))
  }

  def infer(implicit programContext: Context[CucaFunction], localContext: Context[Type]): Type = programContext(id).returnType
}

case class ExprNot(expr: Expression) extends Expression {
  def serializeContents(builder: IndentableStringBuilder): Unit = expr.serialize(builder)

  def infer(implicit programContext: Context[CucaFunction], localContext: Context[Type]): Type = CucaBool
}