package ar.edu.unq.parse.tp1.ast.expressions

import ar.edu.unq.parse.tp1.IndentableStringBuilder
import ar.edu.unq.parse.tp1.ast.CucaTypes._
import ar.edu.unq.parse.tp1.ast.expressions.BinaryType._
import ar.edu.unq.parse.tp1.ast.CucaFunction
import ar.edu.unq.parse.tp1.semantics.Context



object BinaryType {
  type Op = (Any, Any) => Any
  
  def aritmetic(f:(Int,Int)=>Int):(Any,Any)=>Any = (x,y) => f(x.asInstanceOf[Int], y.asInstanceOf[Int])  
  def comparison(f:(Int,Int)=>Boolean):(Any,Any)=>Any = (x,y) => f(x.asInstanceOf[Int], y.asInstanceOf[Int])
  def logic(f:(Boolean,Boolean)=>Boolean):(Any,Any)=>Any = (x,y) => f(x.asInstanceOf[Boolean], y.asInstanceOf[Boolean])
}

trait BinaryExpression extends Expression {

  def expr1: Expression

  def expr2: Expression

  def serializeContents(builder: IndentableStringBuilder): Unit = {
    expr1.serialize(builder)
    expr2.serialize(builder)
  }

  def f:Op

  override def eval(implicit programContext: Context[CucaFunction], localContext: Context[Any]): Any = f(expr1.eval, expr2.eval)
}

trait LogicExpression extends BinaryExpression {
  override def infer(implicit programContext: Context[CucaFunction], localContext: Context[Type]): Type = CucaBool <===> expr1 <===> expr2
}

case class ExprAnd(expr1: Expression, expr2: Expression ) extends LogicExpression { def f = logic(_&&_) }

case class ExprOr(expr1: Expression, expr2: Expression) extends LogicExpression { def f = logic(_||_) }

trait ArithmeticExpression extends BinaryExpression {
  override def infer(implicit programContext: Context[CucaFunction], localContext: Context[Type]): Type = CucaInt <===> expr1 <===> expr2
}

case class ExprAdd(expr1: Expression, expr2: Expression) extends ArithmeticExpression { def f = aritmetic(_+_) }

case class ExprSub(expr1: Expression, expr2: Expression) extends ArithmeticExpression { def f = aritmetic(_-_) }

case class ExprMul(expr1: Expression, expr2: Expression) extends ArithmeticExpression { def f = aritmetic(_*_) }

trait ComparisonExpression extends BinaryExpression {
  override def infer(implicit programContext: Context[CucaFunction], localContext: Context[Type]): Type = {
    CucaInt <===> expr1 <===> expr2
    CucaBool
  }
}

case class ExprLe(expr1: Expression, expr2: Expression) extends ComparisonExpression { def f = comparison(_<=_) }

case class ExprGe(expr1: Expression, expr2: Expression) extends ComparisonExpression  { def f = comparison(_>=_) }

case class ExprLt(expr1: Expression, expr2: Expression) extends ComparisonExpression  { def f = comparison(_<_) }

case class ExprGt(expr1: Expression, expr2: Expression) extends ComparisonExpression  { def f = comparison(_>_) }

case class ExprEq(expr1: Expression, expr2: Expression) extends ComparisonExpression  { def f = comparison(_==_) }

case class ExprNe(expr1: Expression, expr2: Expression) extends ComparisonExpression  { def f = comparison(_!=_) }