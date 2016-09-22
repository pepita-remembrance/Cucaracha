package ar.edu.unq.parse.tp1

import java.io.{FileInputStream, FileNotFoundException}

import ar.edu.unq.parse.tp1.ast.{ASTifier, Program}
import org.antlr.v4.runtime.{ANTLRInputStream, CommonTokenStream}

object Main extends App {

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

  val inputStream = try {
    val fileStream = new FileInputStream(System.getProperty("target"))
    new ANTLRInputStream(fileStream)
  } catch {
    case e: FileNotFoundException => new ANTLRInputStream(text)
  }

  val lexer = new CucarachaGrammarLexer(inputStream)

  val tokens = new CommonTokenStream(lexer)

  val parser = new CucarachaGrammarParser(tokens)

  val parseTree = parser.program()

  val ast = ASTifier.visitProgram(parseTree)

  ast.semanticCheck()

  ast.execute()

}
