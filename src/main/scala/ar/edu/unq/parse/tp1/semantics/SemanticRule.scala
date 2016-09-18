package ar.edu.unq.parse.tp1.semantics

import ar.edu.unq.parse.tp1.ast.CucaTypes.{CucaUnit, CucaVec}
import ar.edu.unq.parse.tp1.ast.{ASTTree, CucaFunction, Program}

trait SemanticRule[A <: ASTTree] {
  def validate(tree: A): Unit

  def apply(tree: A): Unit = validate(tree)
}

case class SemanticException(m: String) extends Exception(m)

case class TypeException(m: String) extends Exception(m)

object HasMainFunction extends SemanticRule[Program] {
  def validate(program: Program): Unit =
    program.functions.find(_.id == "main") match {
      case None => throw SemanticException("Program has no main function")
      case _ =>
    }
}

object FunMainReturnsUnit extends SemanticRule[Program] {
  def validate(program: Program): Unit =
    try {
      program.functions.find(_.id == "main").get.returnType <===> CucaUnit
    }
    catch {
      case e: TypeException => throw SemanticException("Main function must return Unit")
    }
}

object NoDuplicateFunctions extends SemanticRule[Program] {
  def validate(program: Program): Unit = {
    val expected = program.functions.size
    val actual = program.functions.map(_.id).distinct.size
    if (actual != expected) throw SemanticException("Each function must be declared only once")
  }
}

object CantReturnVec extends SemanticRule[CucaFunction] {
  def validate(fun: CucaFunction): Unit =
    try {
      fun.returnType <===> CucaVec
      throw SemanticException(s"Function ${fun.id} has invalid return type: ${CucaVec.key}")
    } catch {
      case e: TypeException =>
    }
}


