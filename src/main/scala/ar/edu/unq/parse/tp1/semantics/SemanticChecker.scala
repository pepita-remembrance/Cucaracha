package ar.edu.unq.parse.tp1.semantics

import ar.edu.unq.parse.tp1.PredefinedFunctions.{PutChar, PutNum}
import ar.edu.unq.parse.tp1.ast.CucaTypes.{CucaUnit, CucaVec, Type}
import ar.edu.unq.parse.tp1.ast.statements.StmtReturn
import ar.edu.unq.parse.tp1.ast.{CucaFunction, Program}

import scala.collection.mutable

trait SemanticChecker {
  def predefinedFunctions: Seq[CucaFunction]

  def check(program: Program): Unit

  def check(fun: CucaFunction)(implicit programContext: Context[CucaFunction]): Unit

  def buildFunctionContext(fun: CucaFunction)(implicit programContext: Context[CucaFunction]): Context[Type] = {
    implicit val localContext = new Context[Type](varName => s"Variable $varName in function ${fun.id} is undefined")
    fun.params.foreach(p => localContext(p.id) = p.paramType)
    fun.body.foreach(_.buildContext())
    localContext
  }
}

object DefaultSemantics extends SemanticChecker with SemanticDSL {
  def predefinedFunctions: Seq[CucaFunction] = List(PutChar, PutNum)

  def check(program: Program): Unit = checkThat(program)(
    hasNoDuplicateFunctions andThen
      ("main" isDefined) andThen
      ("main" has 0 returns) andThen
      ("main" has 0 parameters) andThen
      checkFunctionsBody)

  def check(fun: CucaFunction)(implicit programContext: Context[CucaFunction]): Unit = {
    CantReturn(CucaVec).check(fun)
    fun.returnType match {
      case CucaUnit =>
        HasNoReturns.check(fun)
        implicit val localContext = buildFunctionContext(fun)
      case _ =>
        HasOneReturn.check(fun)
        ReturnIsLastInstruction.check(fun)
        implicit val localContext = buildFunctionContext(fun)
        val returnExpr = fun.body.collect({ case s: StmtReturn => s }).head.value
        TypesAs(fun.returnType).check(returnExpr)
    }
  }

}

class Context[A](message: String => String) extends mutable.HashMap[String, A] {
  override def apply(key: String): A = getOrElse(key, throw SemanticException(message(key)))
}
