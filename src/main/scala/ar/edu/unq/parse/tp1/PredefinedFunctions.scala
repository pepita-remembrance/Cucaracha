package ar.edu.unq.parse.tp1

import ar.edu.unq.parse.tp1.ast.CucaTypes.{CucaInt, CucaUnit}
import ar.edu.unq.parse.tp1.ast.{CucaFunction, Instruction, Parameter}


object PredefinedFunctions {

  object PutChar extends CucaFunction("putChar", List(Parameter("char", CucaInt)), List.empty[Instruction], CucaUnit)
  object PutNum extends CucaFunction("putNum", List(Parameter("n", CucaInt)), List.empty[Instruction], CucaUnit)

}
