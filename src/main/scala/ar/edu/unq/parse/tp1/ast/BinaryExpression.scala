package ar.edu.unq.parse.tp1.ast

import ar.edu.unq.parse.tp1.ast.CucaTypes._


trait BinaryExpression extends Expression {
  def expr1: Expression
  def expr2: Expression

  def argumentType:Type
  def resultType:Type

  override def infer: Type = {
    expr1 <===> argumentType <===> expr2
    resultType
  }

  def serializeContents(builder: IndentableStringBuilder): Unit = {
    expr1.serialize(builder)
    expr2.serialize(builder)
  }
}

trait LogicExpression extends BinaryExpression {
  val argumentType = CucaBool
  val resultType = CucaBool
}

case class ExprAnd(expr1: Expression, expr2: Expression) extends LogicExpression

case class ExprOr(expr1: Expression, expr2: Expression) extends LogicExpression

trait ArithmeticExpression extends BinaryExpression {
  val argumentType = CucaInt
  val resultType = CucaInt
}

case class ExprAdd(expr1: Expression, expr2: Expression) extends ArithmeticExpression

case class ExprSub(expr1: Expression, expr2: Expression) extends ArithmeticExpression

case class ExprMul(expr1: Expression, expr2: Expression) extends ArithmeticExpression

trait ComparisonExpression extends BinaryExpression {
  val argumentType = CucaInt
  val resultType = CucaBool
}

case class ExprLe(expr1: Expression, expr2: Expression) extends ComparisonExpression

case class ExprGe(expr1: Expression, expr2: Expression) extends ComparisonExpression

case class ExprLt(expr1: Expression, expr2: Expression) extends ComparisonExpression

case class ExprGt(expr1: Expression, expr2: Expression) extends ComparisonExpression

case class ExprEq(expr1: Expression, expr2: Expression) extends ComparisonExpression

case class ExprNe(expr1: Expression, expr2: Expression) extends ComparisonExpression