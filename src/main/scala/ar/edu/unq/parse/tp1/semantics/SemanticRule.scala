package ar.edu.unq.parse.tp1.semantics

import ar.edu.unq.parse.tp1.ast.CucaTypes.Type
import ar.edu.unq.parse.tp1.ast.CucaTypes._
import ar.edu.unq.parse.tp1.ast.expressions.Expression
import ar.edu.unq.parse.tp1.ast.statements.{Statement, StmtReturn}
import ar.edu.unq.parse.tp1.ast.{ASTTree, CucaFunction, Program}

import scala.reflect.ClassTag

trait SemanticRule[A <: ASTTree] {
  def check(tree: A): Unit
}

case class SemanticException(m: String) extends Exception(m)

case class TypeException(m: String) extends Exception(m)

case class HasFunction(expectedId: String) extends SemanticRule[Program] {
  def check(program: Program): Unit =
    program.functions.find(_.id == expectedId) match {
      case None => throw SemanticException(s"Program has no $expectedId function")
      case _ =>
    }
}

object HasNoDuplicateFunctions extends SemanticRule[Program] {
  def check(program: Program): Unit = {
    val expected = program.functions.size
    val actual = program.functions.map(_.id).distinct.size
    if (actual != expected) throw SemanticException("Each function must be declared only once")
  }
}

case class CantReturn(forbiddenType: Type) extends SemanticRule[CucaFunction] {
  def check(fun: CucaFunction): Unit =
    try {
      fun.returnType <===> forbiddenType
      throw SemanticException(s"Function ${fun.id} has invalid return type: ${forbiddenType.key}")
    } catch {
      case e: TypeException =>
    }
}

case class HasNParamenters(n: Int) extends SemanticRule[CucaFunction] {
  def check(fun: CucaFunction): Unit = if (fun.params.size != n) throw SemanticException(s"Function ${fun.id} is expected to have $n parameters")
}

case class HasNStatements[S <: Statement](expected: Int)(implicit tag: ClassTag[S]) extends SemanticRule[CucaFunction] {
  def message(fun: CucaFunction): String = s"Function ${fun.id} must have $expected ${tag.runtimeClass.getSimpleName.stripPrefix("Stmt").toLowerCase} statements"

  def check(fun: CucaFunction): Unit = {
    val actual = fun.body.collect({ case i: S => i }).size
    if (actual != expected) throw SemanticException(message(fun))
  }
}

class HasNReturns(expected: Int) extends SemanticRule[CucaFunction] {
  def message(fun: CucaFunction): String = s"Function ${fun.id} must have $expected return statements"

  def check(fun: CucaFunction): Unit = {
    val actual = fun.body.collect({ case i: StmtReturn => i }).size
    if (actual != expected) throw SemanticException(message(fun))
  }
}

object HasNoReturns extends HasNReturns(0)

object HasOneReturn extends HasNReturns(1)

object ReturnIsLastInstruction extends SemanticRule[CucaFunction] {
  def check(fun: CucaFunction): Unit = fun.body.last match {
    case i: StmtReturn =>
    case _ => throw SemanticException(s"Function ${fun.id} must have a return statement as last instruction")
  }
}

case class TypesAs(expectedType: Type)(implicit programContext: Context[CucaFunction], localContext: Context[Type]) extends SemanticRule[Expression] {
  def check(expr: Expression): Unit = expr <===> expectedType
}

