package ar.edu.unq.parse.tp1.assembler

trait ExecutionEnviroment extends NasmGenerator {

  def oldStackPointerReg = Register("rbp")

  def stackPointerReg = Register("rsp")

  def returnReg = Register("rax")

  //First integer register
  def reg1: Register

  //Second integer register
  def reg2: Register

  //Third integer register
  def reg3: Register

  //Fourth integer register
  def reg4: Register

}

trait WindowsEnviroment extends ExecutionEnviroment {
  def reg1: Register = Register("rcx")

  def reg2: Register = Register("rdx")

  def reg3: Register = Register("r8")

  def reg4: Register = Register("r9")
}

trait UnixEnviroment extends ExecutionEnviroment {
  def reg1: Register = Register("rdi")

  def reg2: Register = Register("rsi")

  def reg3: Register = Register("rdx")

  def reg4: Register = Register("rcx")
}
