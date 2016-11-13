package ar.edu.unq.parse.tp1

import java.io.{FileInputStream, FileNotFoundException}

import ar.edu.unq.parse.tp1.assembler.{NasmAssemblerGenerator, WindowsEnviroment}
import ar.edu.unq.parse.tp1.ast.ASTifier
import org.antlr.v4.runtime.{ANTLRInputStream, CommonTokenStream}

class CucaApp extends App {

  val text =
    """
      | fun main() {
      |   putChar(nextChar(65))
      | }
      |
      | fun nextChar(someChar:Int):Int {
      |   return someChar + 1
      | }
    """.stripMargin

  val target = System.getProperty("target")

  val inputStream = if (target != "") {
    val fileStream = try {
      new FileInputStream(target)
    } catch {
      case e: FileNotFoundException =>
        val default = "./src/test/tests_cucaracha/test01.input"
        println(s"Unable to find file $target, defaulting to $default")
        new FileInputStream(default)
    }
    new ANTLRInputStream(fileStream)
  } else {
    new ANTLRInputStream(text)
  }

  val lexer = new CucarachaGrammarLexer(inputStream)

  val tokens = new CommonTokenStream(lexer)

  val parser = new CucarachaGrammarParser(tokens)

  val parseTree = parser.program()

  val ast = ASTifier.visitProgram(parseTree)

  ast.semanticCheck()

}

object Run extends CucaApp {
  ast.execute()
}

object Compile extends CucaApp {
  val generator = NasmAssemblerGenerator.`for`(WindowsEnviroment)
  println(generator.assemble(ast))
}