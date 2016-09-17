package ar.edu.unq.parse.tp1

import java.io.FileInputStream
import scala.io.Source

import ar.edu.unq.parse.tp1.ast.ASTifier
import org.antlr.v4.runtime.tree.ParseTree
import org.antlr.v4.runtime.{ANTLRInputStream, CommonTokenStream}
import org.scalatest.{FlatSpec, Matchers}


trait BaseSpec extends FlatSpec with Matchers with ParsingTest {

}

trait ParsingTest {
  val cucaTestFilesFolder = "./src/test/tests_cucaracha/"

  def parseInput(filePath: String, startRule: CucarachaGrammarParser => ParseTree = _.program()) = {
    val file = new FileInputStream(cucaTestFilesFolder + filePath + ".input")
    val inputStream = new ANTLRInputStream(file)
    val lexer = new CucarachaGrammarLexer(inputStream)
    val tokens = new CommonTokenStream(lexer)
    val parser = new CucarachaGrammarParser(tokens)
    val parseTree = startRule(parser)

    ASTifier.visit(parseTree)
  }

  def expected(filePath: String) = {
    Source.fromFile(cucaTestFilesFolder + filePath + ".expected").getLines().mkString("\n")+"\n"
  }
}