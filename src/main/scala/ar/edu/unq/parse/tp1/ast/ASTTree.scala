package ar.edu.unq.parse.tp1.ast

import ar.edu.unq.parse.tp1.ast.CucaTypes.{CucaUnit, Type}

case class Program(functions: Seq[CucaFunction])
case class CucaFunction(id: String, params: Seq[Param], body: Seq[Instruction], returnType: Type = CucaUnit)
case class Param(id: String, paramType: Type)

trait Instruction
case class Assign(id: String, value: Expression) extends Instruction
case class VectorAssign(id: String, position: Expression, value: Expression) extends Instruction
case class IfThenElse(condition: Expression, ifTrue: Seq[Instruction], ifFalse: Seq[Instruction]=List.empty[Instruction]) extends Instruction
case class While(condition: Expression, body: Seq[Instruction]) extends Instruction
case class Return(value: Expression) extends Instruction
case class Call(id:String, params: Seq[Expression]) extends Instruction with Expression

trait Expression
case class Variable(id:String) extends Expression
case class ConstantInt(value: Int) extends Expression
case class ConstantBool(value: Boolean) extends Expression
case class Vector(values: Seq[Expression]) extends Expression
case class VectorLength(id: String) extends Expression
case class VectorDeref(id:String, position: Expression) extends Expression
case class LogicNot(expr:Expression) extends Expression
class BinaryExpression(expr1:Expression, expr2:Expression) extends Expression
case class LogicAnd(expr1:Expression, expr2:Expression) extends BinaryExpression(expr1, expr2)
case class LogicOr(expr1:Expression, expr2:Expression) extends BinaryExpression(expr1, expr2)
case class MathPlus(expr1:Expression, expr2:Expression) extends BinaryExpression(expr1, expr2)
case class MathMinus(expr1:Expression, expr2:Expression) extends BinaryExpression(expr1, expr2)
case class MathTimes(expr1:Expression, expr2:Expression) extends BinaryExpression(expr1, expr2)
case class CompLE(expr1:Expression, expr2:Expression) extends BinaryExpression(expr1, expr2)
case class CompGE(expr1:Expression, expr2:Expression) extends BinaryExpression(expr1, expr2)
case class CompLT(expr1:Expression, expr2:Expression) extends BinaryExpression(expr1, expr2)
case class CompGT(expr1:Expression, expr2:Expression) extends BinaryExpression(expr1, expr2)
case class RelEQ(expr1:Expression, expr2:Expression) extends BinaryExpression(expr1, expr2)
case class RelNE(expr1:Expression, expr2:Expression) extends BinaryExpression(expr1, expr2)

object CucaTypes {
  sealed abstract class Type
  case object CucaUnit extends Type
  case object CucaInt extends Type
  case object CucaBool extends Type
  case object CucaVec extends Type
}