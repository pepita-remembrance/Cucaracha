package ar.edu.unq.parse.tp1.assembler

trait ExecutionEnviroment {

  //First integer register
  def reg1: String

  //Second integer register
  def reg2: String

  //Third integer register
  def reg3: String

  //Fourth integer register
  def reg4: String

}

object WindowsEnviroment extends ExecutionEnviroment {
  def reg1: String = "RCX"

  def reg3: String = "RDX"

  def reg2: String = "R8"

  def reg4: String = "R9"
}

object UnixEnviroment extends ExecutionEnviroment {
  def reg1: String = "RDI"

  def reg3: String = "RSI"

  def reg2: String = "RDX"

  def reg4: String = "RCX"
}