package ar.edu.unq.parse.tp1

import java.io.{FileInputStream, FileNotFoundException}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import ar.edu.unq.parse.tp1.assembler._
import ar.edu.unq.parse.tp1.ast.ASTifier
import org.antlr.v4.runtime.{ANTLRInputStream, CommonTokenStream}
import org.apache.commons.lang3.SystemUtils

abstract class CucaApp extends App {

  def text: String

  val defaultFile = "./src/test/tests_cucaracha/test01.input"

  val target = System.getProperty("in", "")

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
      |   vec1 := [65, 66, 67, 68]
      |   vec2 := [65, 66, 67, 68]
      |   show(vec1[2] + vec2[0])
      | }
      |
      | fun show(n: Int) {
      |   if n <= 32 {
      |     putNum(n)
      |   } else {
      |     if n <= 255 {
      |       putChar(n)
      |       putChar(32)
      |     }
      |     putNum(n)
      |   }
      |   putChar(10)
      | }
    """

  val platform = System.getProperty("format", "")

  val generator = platform match {
    case "win64" => new NasmGenerator(ast) with WindowsEnviroment
    case "elf64" => new NasmGenerator(ast) with UnixEnviroment
    case "" if SystemUtils.IS_OS_WINDOWS => new NasmGenerator(ast) with WindowsEnviroment
    case "" if SystemUtils.IS_OS_UNIX => new NasmGenerator(ast) with UnixEnviroment
    case _ => throw new RuntimeException("Unsuported operating system")
  }

  val out = System.getProperty("out", "")
  if (out != "") {
    Files.write(Paths.get(out), generator.generate.toText.getBytes(StandardCharsets.UTF_8))
  } else {
    println(generator.generate.toText)
  }
}