package ar.edu.unq.parse.tp1.ast.expressions

import ar.edu.unq.parse.tp1.ast.CucaTypes._
import ar.edu.unq.parse.tp1.ast.{ASTTree, CucaFunction, IndentableStringBuilder}
import ar.edu.unq.parse.tp1.semantics.{Context, SemanticException}

import scala.collection.mutable

trait Expression extends ASTTree {
  def infer(implicit programContext: Context[CucaFunction], localContext: Context[Type]): Type

  def eval(implicit programContext: Context[CucaFunction], localContext: Context[Any]): Any
}


case class ExprVar(id: String) extends Expression {
  def serializeContents(builder: IndentableStringBuilder): Unit = builder.appendln(id)

  def infer(implicit programContext: Context[CucaFunction], localContext: Context[Type]): Type = localContext(id)

  def eval(implicit programContext: Context[CucaFunction], localContext: Context[Any]) = localContext(id)
}

abstract class ConstantValue[T <: Any](thisType: Type) extends Expression {
  def value: T

  def infer(implicit programContext: Context[CucaFunction], localContext: Context[Type]): Type = thisType

  def eval(implicit programContext: Context[CucaFunction], localContext: Context[Any]) = value

  def serializeContents(builder: IndentableStringBuilder): Unit = builder.appendln(value.toString)

}

case class ExprConstNum(value: Int) extends ConstantValue[Int](CucaInt)

case class ExprConstBool(value: Boolean) extends ConstantValue[Boolean](CucaBool)

case class ExprVecMake(values: Seq[Expression]) extends Expression {
  def serializeContents(builder: IndentableStringBuilder): Unit = values.foreach(_.serialize(builder))

  def infer(implicit programContext: Context[CucaFunction], localContext: Context[Type]): Type = {
    values.foldLeft[Type](CucaInt)(_ <===> _)
    CucaVec
  }

  def eval(implicit programContext: Context[CucaFunction], localContext: Context[Any]) = values.map(_.eval).to[mutable.MutableList]
}

case class ExprVecLength(id: String) extends Expression {
  def serializeContents(builder: IndentableStringBuilder): Unit = builder.appendln(id)

  def infer(implicit programContext: Context[CucaFunction], localContext: Context[Type]): Type = {
    CucaVec <===> localContext(id)
    CucaInt
  }

  def eval(implicit programContext: Context[CucaFunction], localContext: Context[Any]) = fetch[InternalVec](id).size
}

case class ExprVecDeref(id: String, position: Expression) extends Expression {
  def serializeContents(builder: IndentableStringBuilder): Unit = {
    builder.appendln(id)
    position.serialize(builder)
  }

  def infer(implicit programContext: Context[CucaFunction], localContext: Context[Type]): Type = {
    CucaVec <===> localContext(id)
    CucaInt <===> position
  }

  override def eval(implicit programContext: Context[CucaFunction], localContext: Context[Any]): Any = fetch[InternalVec](id).apply(position.eval.asInstanceOf[Int])
}

case class ExprCall(id: String, args: Seq[Expression]) extends Expression {
  def serializeContents(builder: IndentableStringBuilder): Unit = {
    builder.appendln(id)
    args.foreach(_.serialize(builder))
  }

  def infer(implicit programContext: Context[CucaFunction], localContext: Context[Type]): Type = {
    val invoked = programContext(id)
    val expectedParams = invoked.params
    val expected = expectedParams.size
    val actual = args.size
    if (expected == actual) {
      (expectedParams zip args).foreach { t =>
        val (param, expr) = t
        param.paramType <===> expr
      }
    } else throw SemanticException(s"Function $id expects $expected arguments but was called with $actual")
    invoked.returnType
  }

  override def eval(implicit programContext: Context[CucaFunction], localContext: Context[Any]): Any = programContext(id).evalWith(args.map(_.eval))

}

case class ExprNot(expr: Expression) extends Expression {
  def serializeContents(builder: IndentableStringBuilder): Unit = expr.serialize(builder)

  def infer(implicit programContext: Context[CucaFunction], localContext: Context[Type]): Type = CucaBool <===> expr

  override def eval(implicit programContext: Context[CucaFunction], localContext: Context[Any]): Any = !expr.eval.asInstanceOf[Boolean]

}