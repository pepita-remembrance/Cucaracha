package ar.edu.unq.parse.tp1.assembler

import ar.edu.unq.parse.tp1.ast.expressions.Expression

trait ExecutionEnviroment extends NasmGenerator {

  def oldStackPointerReg = Register("rbp")

  def stackPointerReg = Register("rsp")

  def returnReg = Register("rax")

  def usableRegisters = List(
    Register("r8"),
    Register("r9"),
    Register("r10"),
    Register("r11"),
    Register("r12"),
    Register("r13"),
    Register("r14"),
    Register("r15")
  )

  //Reserved for putChar/putNum assembly
  def reserved1: Register

  //Reserved for putNum assembly
  def reserved2: Register

  //Reserved for vector assembly
  def reserved3: Register

  def assemblePutNum(value: Expression)(implicit addressContext: AddressContext, rootLabel: Label): List[NasmInstruction]

  def assemblePutChar(value: Expression)(implicit addressContext: AddressContext, rootLabel: Label): List[NasmInstruction]

}

trait WindowsEnviroment extends ExecutionEnviroment {
  def reserved1: Register = Register("rcx")

  def reserved2: Register = Register("rdx")

  def reserved3: Register = Register("rbx")

  override def usableRegisters = Register("rdi") :: Register("rsi") :: super.usableRegisters

  def assemblePutNum(value: Expression)(implicit addressContext: AddressContext, rootLabel: Label): List[NasmInstruction] =
    Sub(stackPointerReg, 32) ::
      assemble(value, reserved2) ++
        List(
          Mov(reserved1, Data("lli_format_string")),
          Mov(returnReg, 0),
          Call("printf"),
          Add(stackPointerReg, 32)
        )

  def assemblePutChar(value: Expression)(implicit addressContext: AddressContext, rootLabel: Label): List[NasmInstruction] =
    (Sub(stackPointerReg, 32) ::
      assemble(value, reserved1)) ++
      List(
        Call("putchar"),
        Add(stackPointerReg, 32)
      )

}

trait UnixEnviroment extends ExecutionEnviroment {
  def reserved1: Register = Register("rdi")

  def reserved2: Register = Register("rsi")

  def reserved3: Register = Register("rbx")

  override def usableRegisters = Register("rdx") :: Register("rcx") :: super.usableRegisters

  def assemblePutNum(value: Expression)(implicit addressContext: AddressContext, rootLabel: Label): List[NasmInstruction] =
    assemble(value, reserved2) ++
      List(
        Mov(reserved1, Data("lli_format_string")),
        Mov(returnReg, 0),
        Call("printf")
      )


  def assemblePutChar(value: Expression)(implicit addressContext: AddressContext, rootLabel: Label): List[NasmInstruction] =
    assemble(value, reserved1) :+ Call("putchar")

}
