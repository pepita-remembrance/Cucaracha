package ar.edu.unq.parse.tp1

import org.antlr.samples.antlr4example._
import org.antlr.v4.runtime.{ANTLRInputStream, CommonTokenStream}


object Main extends App {

  val text = "(1+1)-2\n"

  val lalala = new ExprBaseListener()

  val inputStream = new ANTLRInputStream(text)

  val lexer = new ExprLexer(inputStream)

  val tokens = new CommonTokenStream(lexer)

  val parser = new ExprParser(tokens)

  val tree = parser.prog()

  println(tree.toStringTree(parser))

}
