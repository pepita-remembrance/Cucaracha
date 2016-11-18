package ar.edu.unq.parse.tp1.assembler

trait ExecutionEnviroment extends NasmGenerator{

  def oldStackPointerReg = "rbp"
  def stackPointerReg = "rsp"
  def returnReg = "rax"

  //First integer register
  def reg1: String

  //Second integer register
  def reg2: String

  //Third integer register
  def reg3: String

  //Fourth integer register
  def reg4: String

}

trait WindowsEnviroment extends ExecutionEnviroment {
  def reg1: String = "rcx"

  def reg2: String = "rdx"

  def reg3: String = "r8"

  def reg4: String = "r9"
}

trait UnixEnviroment extends ExecutionEnviroment {
  def reg1: String = "rdi"

  def reg2: String = "rsi"

  def reg3: String = "rdx"

  def reg4: String = "rcx"
}
