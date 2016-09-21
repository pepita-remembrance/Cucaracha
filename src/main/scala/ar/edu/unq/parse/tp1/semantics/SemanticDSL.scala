package ar.edu.unq.parse.tp1.semantics

import ar.edu.unq.parse.tp1.ast.{CucaFunction, Program}
import ar.edu.unq.parse.tp1.ast.statements.{Statement, StmtReturn}

import scala.reflect.ClassTag

trait SemanticDSL extends SemanticChecker {

  type ProgramRule = Program => Program

  def checkThat: Program => ProgramRule => Program = p => r => r(p)

  def hasNoDuplicateFunctions: ProgramRule = { p =>
    HasNoDuplicateFunctions.check(p)
    p
  }

  def checkFunctionsBody: ProgramRule = { program =>
    implicit val programContext = new Context[CucaFunction](funID => s"Function $funID is undefined")
    predefinedFunctions.foreach(fun => programContext(fun.id) = fun)
    program.functions.foreach(fun => programContext(fun.id) = fun)
    program.functions.foreach(check)
    program
  }

  def getFun(funName: String, program: Program) =
    program.functions.find(_.id == funName).get

  implicit class StringToFunInProgram(funName: String) {

    def has(n: Int): FunHasChecker = FunHasChecker(n, funName)

    def isDefined: ProgramRule = { p =>
      HasFunction(funName).check(p)
      p
    }

  }

  case class FunHasChecker(n: Int, funName: String) {

    def parameters: ProgramRule = { p =>
      val fun = getFun(funName, p)
      HasNParamenters(n).check(fun)
      p
    }

    def statements[S <: Statement : ClassTag]: ProgramRule = { p =>
      val fun = getFun(funName, p)
      HasNStatements[S](n).check(fun)
      p
    }

    def returns = statements[StmtReturn]

  }

}
