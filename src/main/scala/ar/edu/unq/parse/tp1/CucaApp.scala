package ar.edu.unq.parse.tp1

import java.io.{FileInputStream, FileNotFoundException}

import ar.edu.unq.parse.tp1.assembler._
import ar.edu.unq.parse.tp1.ast.ASTifier
import org.antlr.v4.runtime.{ANTLRInputStream, CommonTokenStream}
import org.apache.commons.lang3.SystemUtils

abstract class CucaApp extends App {

  def text: String

  val defaultFile = "./src/test/tests_cucaracha/test01.input"

  val target = System.getProperty("target", "")

  val inputStream = if (target != "") {
    val fileStream = try {
      new FileInputStream(target)
    } catch {
      case e: FileNotFoundException =>
        println(s"Unable to find file $target, defaulting to $defaultFile")
        new FileInputStream(defaultFile)
    }
    new ANTLRInputStream(fileStream)
  } else {
    new ANTLRInputStream(text.stripMargin)
  }

  val lexer = new CucarachaGrammarLexer(inputStream)

  val tokens = new CommonTokenStream(lexer)

  val parser = new CucarachaGrammarParser(tokens)

  val parseTree = parser.program()

  val ast = ASTifier.visitProgram(parseTree)

  ast.semanticCheck()

}

object Run extends CucaApp {
  def text =
    """
      | fun main() {
      |   putChar(nextChar(65))
      | }
      |
      | fun nextChar(someChar:Int):Int {
      |   return someChar + 1
      | }
    """

  ast.execute()
}

object Compile extends CucaApp {
  def text =
    """
      | fun main() {
      |   putChar(65)
      |   putNum(65)
      |   asd()
      | }
      |
      | fun asd() {
      |   a := 5
      |   b := 6
      | }
    """

  val generator =
    if (SystemUtils.IS_OS_WINDOWS)
      new NasmGenerator(ast) with WindowsEnviroment
    else if (SystemUtils.IS_OS_UNIX)
      new NasmGenerator(ast) with UnixEnviroment
    else
      throw new RuntimeException("Unsuported operating system")

  println(generator.generate.toText)
}