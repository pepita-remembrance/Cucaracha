package ar.edu.unq.parse.tp1.semantics

import ar.edu.unq.parse.tp1.PredefinedFunctions.{PutChar, PutNum}
import ar.edu.unq.parse.tp1.ast.{CucaFunction, Program}

import scala.collection.mutable

trait SemanticChecker {

  def predefinedFunctions: Seq[CucaFunction]

  def checkProgram(program: Program): Unit = {
    implicit val context = new ProgramContext
    context ++= predefinedFunctions
    context ++= program.functions
    program.functions.foreach(checkFunction)
  }

  def checkFunction(fun: CucaFunction)(implicit programContext: ProgramContext): Unit

  implicit def ProgramContextToMap(ctx: ProgramContext): mutable.Map[String, CucaFunction] = ctx.map

}

object DefaultSemanticChecker extends SemanticChecker {
  def predefinedFunctions: Seq[CucaFunction] = List(PutChar, PutNum)

  override def checkProgram(program: Program): Unit = {
    HasMainFunction(program)
    FunMainReturnsUnit(program)
    NoDuplicateFunctions(program)
    super.checkProgram(program)
  }

  def checkFunction(fun: CucaFunction)(implicit programContext: ProgramContext): Unit = {
    CantReturnVec(fun)
  }
}

class ProgramContext(val map: mutable.Map[String, CucaFunction] = mutable.Map.empty[String, CucaFunction]) {
  def put(fun: CucaFunction) = map.put(fun.id, fun)

  def +=(fun: CucaFunction) = map.+=((fun.id, fun))

  def ++=(funs: Seq[CucaFunction]) = map.++=(funs.map(fun => (fun.id, fun)))

  def contains(fun: CucaFunction) = map.contains(fun.id)

  def apply(key: String): CucaFunction = map.getOrElse(key, throw SemanticException(s"Function $key is undefined"))
}

