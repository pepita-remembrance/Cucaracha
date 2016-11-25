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

case class Label(text: String) extends NasmInstruction {
  def toText: String = s"$text:"
}

case class Push(nasmAddress: NasmAddress) extends NasmInstruction {
  def toText: String = nasmAddress match {
//    case _:IndirectAddress => s"push qword ${nasmAddress.toText}"
    case _ => s"push ${nasmAddress.toText}"
  }
}

case class Pop(nasmAddress: NasmAddress) extends NasmInstruction {
  def toText: String = nasmAddress match {
//    case _:IndirectAddress => s"pop qword ${nasmAddress.toText}"
    case _ => s"pop ${nasmAddress.toText}"
  }
}

case class Mov(nasmAddress: NasmAddress, nasmValue: NasmValue) extends NasmInstruction {
  def toText: String = (nasmAddress, nasmValue) match {
//    case (_: IndirectAddress, _: Constant) => s"mov qword ${nasmAddress.toText}, ${nasmValue.toText}"
//    case (_: IndirectAddress, _: IndirectAddress) => s"mov qword ${nasmAddress.toText}, qword ${nasmValue.toText}"
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




