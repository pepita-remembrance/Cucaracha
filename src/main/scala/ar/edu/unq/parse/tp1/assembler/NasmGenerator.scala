package ar.edu.unq.parse.tp1.assembler

import ar.edu.unq.parse.tp1.IndentableStringBuilder
import ar.edu.unq.parse.tp1.PredefinedFunctions.{PutChar, PutNum}
import ar.edu.unq.parse.tp1.ast.expressions.{ExprConstNum, Expression}
import ar.edu.unq.parse.tp1.ast.statements.{Statement, StmtCall}
import ar.edu.unq.parse.tp1.ast.{CucaFunction, Program}

abstract class NasmGenerator {
  this: ExecutionEnviroment =>
  def funPrefix = "cuca_"

  val builder = new IndentableStringBuilder("  ")

  import builder._

  def assemble(program: Program): String = {
    appendln("section .data")
    appendln("lli_format_string db \"%lli\"")
    appendln("section .text")
    appendln("global main")
    appendln("extern exit, putchar, printf")
    program.functions.foreach(assemble)
    appendln("main:")
    indent()
    appendln("call cuca_main")
    appendln(s"mov $reg1, 0")
    appendln("call exit")
    dedent()
    builder.toString
  }

  private def assemble(fun: CucaFunction): Unit = {
    appendln(fun.nasmName ++ ":")
    indent()
    appendln(s"push $oldStackPointerReg")
    appendln(s"mov $oldStackPointerReg, $stackPointerReg")
    appendln(s"sub $stackPointerReg, ${8 * fun.totalVariables}")
    fun.body.foreach(assemble)
    appendln(s"mov $oldStackPointerReg, $stackPointerReg")
    appendln(s"pop $oldStackPointerReg")
    appendln("ret")
    dedent()
  }

  private def assemble(stmt: Statement): Unit = stmt match {
    case StmtCall(PutNum.id, args) =>
      assemble(args.head, reg2)
      appendln(s"mov $reg1, lli_format_string")
      appendln(s"mov $returnReg, 0")
      appendln(s"call printf")
    case StmtCall(PutChar.id, args) =>
      assemble(args.head, reg1)
      appendln("call putchar")
    case StmtCall(name, args) =>
      appendln(s"sub $stackPointerReg, ${8 * args.size}")
      args.zipWithIndex.foreach { case (expr, i) => assemble(expr, s"[$stackPointerReg + ${8 * i}]") }
      appendln(s"call $funPrefix$name")
      appendln(s"add $stackPointerReg, ${8 * args.size}")
  }

  private def assemble(expr: Expression, position: String): Unit = expr match {
    case ExprConstNum(value) => appendln(s"mov $position, ${value.toString}")
  }

  implicit class NasmCucaFunction(fun: CucaFunction) {
    def nasmName = funPrefix ++ fun.id.toLowerCase

    def totalVariables = 0 //TODO: hacer que se calcule posta
  }

}