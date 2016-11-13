package ar.edu.unq.parse.tp1.assembler

import ar.edu.unq.parse.tp1.IndentableStringBuilder
import ar.edu.unq.parse.tp1.ast.{CucaFunction, Program}

class NasmAssemblerGenerator(enviroment: ExecutionEnviroment) {
  import enviroment._

  def assemble(program: Program): String = {
    val builder = new IndentableStringBuilder("  ")
    builder
      .appendln("section .data")
      .appendln("lli_format_string db \"%lli\"")
      .appendln("section .text")
      .appendln("global main")
      .appendln("extern exit, putchar, printf")
    program.functions.foreach(assemble)
    builder
      .appendln("main:")
      .appendln("call cuca_main")
      .appendln(s"mov $reg1, 0")
      .appendln("call exit")
    builder.toString
  }

  def assemble(fun: CucaFunction) = {

  }

}

object NasmAssemblerGenerator {
  def `for`(enviroment: ExecutionEnviroment) = new NasmAssemblerGenerator(enviroment)
}