package ar.edu.unq.parse.tp1.assembler

import ar.edu.unq.parse.tp1.IndentableStringBuilder

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class NasmProgram() {
  val dataSection = mutable.HashMap("lli_format_string" -> "\"%lli\"")
  val textSection = new TextSection(
    globals = mutable.MutableList("main"),
    externs = mutable.MutableList("exit", "putchar", "printf")
  )

  def toText: String = {
    val builder = new IndentableStringBuilder("  ")
    builder.appendln("section .data")
    dataSection.foreach { case (key, value) => builder.appendln(s"$key db $value") }
    builder.appendln("section .text")
    textSection.toText(builder)
    builder.toString
  }
}

class TextSection(val globals: mutable.MutableList[String], val externs: mutable.MutableList[String]) {
  val contents = ListBuffer.empty[NasmInstruction]

  def toText(builder: IndentableStringBuilder) = {
    builder
      .appendln("global " ++ globals.mkString(", "))
      .appendln("extern " ++ externs.mkString(", "))
    contents.foreach(instruction => builder.appendln(instruction.toText))
  }
}


/** ****************/
/** *Instructions **/
/** ****************/

trait NasmInstruction {
  def toText: String
}

abstract class Label(text: String) extends NasmInstruction {
  private var subLabels = 0

  def newSubLabel = {
    subLabels += 1
    LocalLabel(text, subLabels)
  }

  def toText: String

  def toJumpLabel: String
}

case class RootLabel(text: String) extends Label(text) {
  def toText: String = s"$text:"

  def toJumpLabel: String = text
}

case class LocalLabel(text: String, n: Int) extends Label(s"${text}_$n") {
  def toText: String = s".${text}_$n:"

  def toJumpLabel: String = s".${text}_$n"
}

case class Push(nasmAddress: NasmAddress) extends NasmInstruction {
  def toText: String = nasmAddress match {
    case _ => s"push ${nasmAddress.toText}"
  }
}

case class Pop(nasmAddress: NasmAddress) extends NasmInstruction {
  def toText: String = nasmAddress match {
    case _ => s"pop ${nasmAddress.toText}"
  }
}

case class Mov(nasmAddress: NasmAddress, nasmValue: NasmValue) extends NasmInstruction {
  def toText: String = (nasmAddress, nasmValue) match {
    case _ => s"mov ${nasmAddress.toText}, ${nasmValue.toText}"
  }
}

case class Call(funName: String) extends NasmInstruction {
  def toText: String = s"call $funName"
}

case class Not(nasmAddress: NasmAddress) extends NasmInstruction {
  def toText: String = s"not ${nasmAddress.toText}"
}

case class And(nasmAddress: NasmAddress, nasmValue: NasmValue) extends NasmInstruction {
  def toText: String = s"and ${nasmAddress.toText}, ${nasmValue.toText}"
}

case class Or(nasmAddress: NasmAddress, nasmValue: NasmValue) extends NasmInstruction {
  def toText: String = s"and ${nasmAddress.toText}, ${nasmValue.toText}"
}

case class Add(nasmAddress: NasmAddress, nasmValue: NasmValue) extends NasmInstruction {
  def toText: String = s"add ${nasmAddress.toText}, ${nasmValue.toText}"
}

case class Sub(nasmAddress: NasmAddress, nasmValue: NasmValue) extends NasmInstruction {
  def toText: String = s"sub ${nasmAddress.toText}, ${nasmValue.toText}"
}

case class Imul(nasmAddress: NasmAddress) extends NasmInstruction {
  def toText: String = s"imul ${nasmAddress.toText}"
}

case class Cmp(nasmAddress: NasmAddress, nasmValue: NasmValue) extends NasmInstruction {
  def toText: String = s"cmp ${nasmAddress.toText}, ${nasmValue.toText}"
}

class NasmJump(prefix: String, label: Label) extends NasmInstruction {
  def toText: String = s"$prefix ${label.toJumpLabel}"
}

case class Jump(label: Label) extends NasmJump("jmp", label)

case class JumpLe(label: Label) extends NasmJump("jle", label)

case class JumpGe(label: Label) extends NasmJump("jge", label)

case class JumpLt(label: Label) extends NasmJump("jl", label)

case class JumpGt(label: Label) extends NasmJump("jg", label)

case class JumpEq(label: Label) extends NasmJump("je", label)

case class JumpNe(label: Label) extends NasmJump("jne", label)

object Ret extends NasmInstruction {
  def toText: String = "ret"
}

/** ****************/
/** *Values ********/
/** ****************/

trait NasmValue {
  def toText: String
}

case class Constant(value: Int) extends NasmValue {
  def toText: String = value.toString
}

case class Data(key: String) extends NasmValue {
  def toText: String = key
}

trait NasmAddress extends NasmValue

case class Register(name: String) extends NasmAddress {
  def toText: String = name
}

case class IndirectAddress(register: Register, offset: Int) extends NasmAddress {
  def toText: String = s"qword [${register.toText} ${if (offset < 0) s"- ${Math.abs(offset)}" else s"+ $offset"}]"
}




