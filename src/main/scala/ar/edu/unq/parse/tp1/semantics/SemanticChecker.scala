package ar.edu.unq.parse.tp1.semantics

import ar.edu.unq.parse.tp1.PredefinedFunctions.{PutChar, PutNum}
import ar.edu.unq.parse.tp1.ast.CucaTypes.{CucaUnit, CucaVec, Type}
import ar.edu.unq.parse.tp1.ast.{CucaFunction, Program}

import scala.collection.mutable

trait SemanticChecker {
  def predefinedFunctions: Seq[CucaFunction]

  def checkProgram(program: Program): Unit = {
    implicit val context = new Context[CucaFunction](funID => s"Function $funID is undefined")
    context ++= predefinedFunctions.map(f => (f.id, f))
    context ++= program.functions.map(f => (f.id, f))
    program.functions.foreach(checkFunction)
  }

  def buildFunctionContext(fun: CucaFunction)(implicit programContext: Context[CucaFunction]): Context[Type] = {
    val functionContext = new Context[Type](varName => s"Variable $varName in function ${fun.id} is undefined")
    functionContext ++= fun.params.map(p => (p.id, p.paramType))
    //TODO: apply function body to new context
    functionContext
  }

  def checkFunction(fun: CucaFunction)(implicit programContext: Context[CucaFunction]): Unit
}

object DefaultSemantics extends SemanticChecker {
  def predefinedFunctions: Seq[CucaFunction] = List(PutChar, PutNum)

  override def checkProgram(program: Program): Unit = {
    HasMainFunction.check(program)
    val mainFun = program.functions.find(_.id == "main").get
    MustReturn(CucaUnit).check(mainFun)
    HasNParamenters(0).check(mainFun)
    HasNoDuplicateFunctions.check(program)
    super.checkProgram(program)
  }

  def checkFunction(fun: CucaFunction)(implicit programContext: Context[CucaFunction]): Unit = {
    CantReturn(CucaVec).check(fun)
    fun.returnType match {
      case CucaUnit =>
        HasNoReturns.check(fun)
        buildFunctionContext(fun)
      case _ =>
        HasOneReturn.check(fun)
        ReturnIsLastInstruction.check(fun)
        val functionContext = buildFunctionContext(fun)
        //TODO: Use program and function context to check return statement
    }
  }

}

class Context[A](message: String => String) extends mutable.HashMap[String, A] {
  override def apply(key: String): A = getOrElse(key, throw SemanticException(message(key)))
}

class FunctionContext

trait ID {
  def id: String
}