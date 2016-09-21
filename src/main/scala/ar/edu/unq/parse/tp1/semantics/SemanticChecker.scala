package ar.edu.unq.parse.tp1.semantics

import ar.edu.unq.parse.tp1.PredefinedFunctions.{PutChar, PutNum}
import ar.edu.unq.parse.tp1.ast.CucaTypes.{CucaUnit, CucaVec, Type}
import ar.edu.unq.parse.tp1.ast.statements.StmtReturn
import ar.edu.unq.parse.tp1.ast.{CucaFunction, Program}

import scala.collection.mutable

trait SemanticChecker {
  def predefinedFunctions: Seq[CucaFunction]

  def checkProgram(program: Program): Unit = {
    implicit val programContext = new Context[CucaFunction](funID => s"Function $funID is undefined")
    predefinedFunctions.foreach(fun => programContext(fun.id) = fun)
    program.functions.foreach(fun => programContext(fun.id) = fun)
    program.functions.foreach(checkFunction)
  }

  def buildFunctionContext(fun: CucaFunction)(implicit programContext: Context[CucaFunction]): Context[Type] = {
    implicit val localContext = new Context[Type](varName => s"Variable $varName in function ${fun.id} is undefined")
    fun.params.foreach(p => localContext(p.id) = p.paramType)
    fun.body.foreach(_.buildContext())
    localContext
  }

  def checkFunction(fun: CucaFunction)(implicit programContext: Context[CucaFunction]): Unit
}

object DefaultSemantics extends SemanticChecker {
  def predefinedFunctions: Seq[CucaFunction] = List(PutChar, PutNum)

  override def checkProgram(program: Program): Unit = {
    HasNoDuplicateFunctions.check(program)
    HasFunction("main").check(program)
    val mainFun = program.functions.find(_.id == "main").get
    HasNoReturns.check(mainFun)
    HasNParamenters(0).check(mainFun)
    super.checkProgram(program)
  }

  def checkFunction(fun: CucaFunction)(implicit programContext: Context[CucaFunction]): Unit = {
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
