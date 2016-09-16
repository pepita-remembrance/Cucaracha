package ar.edu.unq.parse.tp1

import org.antlr.v4.runtime.{ANTLRInputStream, CommonTokenStream}

object Main extends App {

//  val text =
//    """
//      | fun sarasa(x:Int,y:Bool):Int {
//      |    return x + 1 * 5
//      | }
//      |
//    """.stripMargin
  val text =
    """
      | x + 1 * 5
      |
    """.stripMargin

  val inputStream = new ANTLRInputStream(text)

  val lexer = new CucarachaGrammarLexer(inputStream)

  val tokens = new CommonTokenStream(lexer)

  val parser = new CucarachaGrammarParser(tokens)

//  val parseTree = parser.program()
  val parseTree = parser.expr()

  println(parseTree.toStringTree(parser))

  println(parseTree.bin_op().toStringTree(parser))

}
