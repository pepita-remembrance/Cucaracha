package ar.edu.unq.parse.tp1.assembler

trait ExecutionEnviroment extends NasmGenerator {

  def oldStackPointerReg = Register("rbp")

  def stackPointerReg = Register("rsp")

  def returnReg = Register("rax")

  def usableRegisters = List(Register("r12"), Register("r13"), Register("r14"), Register("r15"))

  //First integer register
  def reserved1: Register

  //Second integer register
  def reserved2: Register

  //Third integer register
  def reserved3: Register

  //Fourth integer register
  def reserved4: Register

  //Fifth integer register
  def reserved5: Register

  //Sixth integer register
  def reserved6: Register

}

trait WindowsEnviroment extends ExecutionEnviroment {
  def reserved1: Register = Register("rcx")

  def reserved2: Register = Register("rdx")

  def reserved3: Register = Register("r8")

  def reserved4: Register = Register("r9")

  def reserved5: Register = Register("r10")

  def reserved6: Register = Register("r11")

  override def usableRegisters = Register("rbx") :: Register("rdi") :: Register("rsi") :: super.usableRegisters
}

trait UnixEnviroment extends ExecutionEnviroment {
  def reserved1: Register = Register("rdi")

  def reserved2: Register = Register("rsi")

  def reserved3: Register = Register("rdx")

  def reserved4: Register = Register("rcx")

  def reserved5: Register = Register("r8")

  def reserved6: Register = Register("r9")

  override def usableRegisters = Register("r10") :: Register("r11") :: super.usableRegisters
}
