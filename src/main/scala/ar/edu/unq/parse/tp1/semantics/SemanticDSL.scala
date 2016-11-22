package ar.edu.unq.parse.tp1.semantics

import ar.edu.unq.parse.tp1.Context
import ar.edu.unq.parse.tp1.ast.CucaTypes.Type
import ar.edu.unq.parse.tp1.ast.statements.{Statement, StmtReturn}
import ar.edu.unq.parse.tp1.ast.{CucaFunction, Program}

import scala.reflect.ClassTag

trait SemanticDSL extends SemanticChecker with ProgramSemanticDSL with FunctionSemanticDSL

trait ProgramSemanticDSL extends SemanticChecker {
  type ProgramRule = Program => Program

  def checkThat(program: Program): ProgramRule => Program = r => r(program)

  def getFun(funName: String, program: Program) =
    program.functions.find(_.id == funName).get

  def hasNoDuplicateFunctions: ProgramRule = { p =>
    HasNoDuplicateFunctions.check(p)
    p
  }

  def checkFunctionsBody: ProgramRule = { program =>
    implicit val programContext = buildProgramContext(program)
    program.functions.foreach(check)
    program
  }

  implicit class StringToFunInProgram(funName: String) {

    def has(n: Int): FunNameHasChecker = FunNameHasChecker(n, funName)

    def isDefined: ProgramRule = { p =>
      HasFunction(funName).check(p)
      p
    }

  }

  case class FunNameHasChecker(n: Int, funName: String) {

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

trait FunctionSemanticDSL extends SemanticChecker {
  type FunctionRule = CucaFunction => CucaFunction

  def checkThat(fun: CucaFunction): FunctionRule => CucaFunction = r => r(fun)

  def cantReturn(cucaType: Type): FunctionRule = { fun =>
    CantReturn(cucaType).check(fun)
    fun
  }

  def returnIsLastStatement : FunctionRule = { fun =>
    ReturnIsLastStatement.check(fun)
    fun
  }

  def returnTypesAs(cucaType:Type)(implicit programContext: Context[CucaFunction], localContext: Context[Type]) : FunctionRule = { fun =>
    val returnExpr = fun.body.collect({ case s: StmtReturn => s }).head.value
    TypesAs(fun.returnType).check(returnExpr)
    fun
  }

  def has(n: Int): FunHasChecker = FunHasChecker(n)

  case class FunHasChecker(n: Int) {

    def statements[S <: Statement : ClassTag]: FunctionRule = { fun =>
      HasNStatements[S](n).check(fun)
      fun
    }

    def returns = statements[StmtReturn]

  }

}
