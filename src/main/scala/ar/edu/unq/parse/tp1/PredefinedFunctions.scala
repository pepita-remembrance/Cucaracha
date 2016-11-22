package ar.edu.unq.parse.tp1

import ar.edu.unq.parse.tp1.ast.CucaTypes.{CucaInt, CucaUnit}
import ar.edu.unq.parse.tp1.ast.statements.Statement
import ar.edu.unq.parse.tp1.ast.{CucaFunction, Parameter}


object PredefinedFunctions {

  object PutChar extends CucaFunction("putChar", List(Parameter("char", CucaInt)), List.empty[Statement], CucaUnit) {
    override def evalWith(values: Seq[Any])(implicit programContext: Context[CucaFunction]): Any = {
      print(values.last.asInstanceOf[Int].toChar)
      CucaUnit
    }
  }

  object PutNum extends CucaFunction("putNum", List(Parameter("n", CucaInt)), List.empty[Statement], CucaUnit) {
    override def evalWith(values: Seq[Any])(implicit programContext: Context[CucaFunction]): Any = {
      print(values.last.asInstanceOf[Int])
      CucaUnit
    }
  }

}
