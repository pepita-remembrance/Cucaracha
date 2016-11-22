package ar.edu.unq.parse.tp1.semantics

import ar.edu.unq.parse.tp1.Context
import ar.edu.unq.parse.tp1.PredefinedFunctions.{PutChar, PutNum}
import ar.edu.unq.parse.tp1.ast.CucaTypes.{CucaUnit, CucaVec, Type}
import ar.edu.unq.parse.tp1.ast.{CucaFunction, Program}

import scala.collection.mutable

trait SemanticChecker {
  def predefinedFunctions: Seq[CucaFunction]

  def check(program: Program): Unit

  def check(fun: CucaFunction)(implicit programContext: Context[CucaFunction]): Unit

  def buildProgramContext(program: Program): Context[CucaFunction] = {
    val programContext = new Context[CucaFunction](funID => s"Function $funID is undefined")
    predefinedFunctions.foreach(fun => programContext(fun.id) = fun)
    program.functions.foreach(fun => programContext(fun.id) = fun)
    programContext
  }

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
    implicit val localContext = buildFunctionContext(fun)
    checkThat(fun)(
      cantReturn(CucaVec) andThen
        (fun.returnType match {
          case CucaUnit => has(0) returns
          case t => (has(1) returns) andThen
            returnIsLastStatement andThen
            returnTypesAs(t)
        })
    )
  }

}


