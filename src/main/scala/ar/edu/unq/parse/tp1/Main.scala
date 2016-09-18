package ar.edu.unq.parse.tp1

import ar.edu.unq.parse.tp1.ast.{ASTifier, Program}
import org.antlr.v4.runtime.{ANTLRInputStream, CommonTokenStream}

object Main extends App {

  val text =
    """
      | fun main(x:Int,y:Bool) {
      |    return 1 > 2
      | }
      |
    """.stripMargin

  val inputStream = new ANTLRInputStream(text)

  val lexer = new CucarachaGrammarLexer(inputStream)

  val tokens = new CommonTokenStream(lexer)

  val parser = new CucarachaGrammarParser(tokens)

  val parseTree = parser.program()

  val ast = ASTifier.visitProgram(parseTree)

  ast.semanticCheck()

}
