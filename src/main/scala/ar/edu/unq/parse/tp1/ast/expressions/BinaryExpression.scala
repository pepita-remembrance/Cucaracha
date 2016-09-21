package ar.edu.unq.parse.tp1.ast.expressions

import ar.edu.unq.parse.tp1.ast.CucaTypes._
import ar.edu.unq.parse.tp1.ast.{CucaFunction, IndentableStringBuilder}
import ar.edu.unq.parse.tp1.semantics.Context


trait BinaryExpression extends Expression {
  def expr1: Expression

  def expr2: Expression

  def serializeContents(builder: IndentableStringBuilder): Unit = {
    expr1.serialize(builder)
    expr2.serialize(builder)
  }
}

trait LogicExpression extends BinaryExpression {
  override def infer(implicit programContext: Context[CucaFunction], localContext: Context[Type]): Type = CucaBool <===> expr1 <===> expr2
}

case class ExprAnd(expr1: Expression, expr2: Expression) extends LogicExpression

case class ExprOr(expr1: Expression, expr2: Expression) extends LogicExpression

trait ArithmeticExpression extends BinaryExpression {
  override def infer(implicit programContext: Context[CucaFunction], localContext: Context[Type]): Type = CucaInt <===> expr1 <===> expr2
}

case class ExprAdd(expr1: Expression, expr2: Expression) extends ArithmeticExpression

case class ExprSub(expr1: Expression, expr2: Expression) extends ArithmeticExpression

case class ExprMul(expr1: Expression, expr2: Expression) extends ArithmeticExpression

trait ComparisonExpression extends BinaryExpression {
  override def infer(implicit programContext: Context[CucaFunction], localContext: Context[Type]): Type = {
    CucaInt <===> expr1 <===> expr2
    CucaBool
  }
}

case class ExprLe(expr1: Expression, expr2: Expression) extends ComparisonExpression

case class ExprGe(expr1: Expression, expr2: Expression) extends ComparisonExpression

case class ExprLt(expr1: Expression, expr2: Expression) extends ComparisonExpression

case class ExprGt(expr1: Expression, expr2: Expression) extends ComparisonExpression

case class ExprEq(expr1: Expression, expr2: Expression) extends ComparisonExpression

case class ExprNe(expr1: Expression, expr2: Expression) extends ComparisonExpression