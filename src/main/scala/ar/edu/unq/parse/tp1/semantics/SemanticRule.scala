package ar.edu.unq.parse.tp1.semantics

import ar.edu.unq.parse.tp1.ast.CucaTypes.Type
import ar.edu.unq.parse.tp1.ast.instructions.StmtReturn
import ar.edu.unq.parse.tp1.ast.{ASTTree, CucaFunction, Program}

trait SemanticRule[A <: ASTTree] {
  def check(tree: A): Unit
}

case class SemanticException(m: String) extends Exception(m)

case class TypeException(m: String) extends Exception(m)

object HasMainFunction extends SemanticRule[Program] {
  def check(program: Program): Unit =
    program.functions.find(_.id == "main") match {
      case None => throw SemanticException("Program has no main function")
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

case class MustReturn(expectedType: Type) extends SemanticRule[CucaFunction] {
  def check(fun: CucaFunction): Unit =
    try {
      fun.returnType <===> expectedType
    }
    catch {
      case e: TypeException => throw SemanticException(s"Function ${fun.id} must return ${expectedType.key}")
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
    case i:StmtReturn =>
    case _ => throw SemanticException(s"Function ${fun.id} must have a return statement as last instruction")
  }
}


