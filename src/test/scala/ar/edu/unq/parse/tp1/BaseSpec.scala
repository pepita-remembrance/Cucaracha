package ar.edu.unq.parse.tp1

import java.io.FileInputStream

import ar.edu.unq.parse.tp1.ast.{ASTTree, ASTifier}
import org.antlr.v4.runtime.tree.ParseTree
import org.antlr.v4.runtime.{ANTLRInputStream, CommonTokenStream}
import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

trait BaseSpec extends FlatSpec with Matchers with ParsingTest

trait ParsingTest {
  val cucaTestFilesFolder = "./src/test/tests_cucaracha/"

  def parseInput[A <: ParseTree, B <: ASTTree](filePath: String, startRule: CucarachaGrammarParser => A, astifierMethod: A => B): B = {
    val file = new FileInputStream(cucaTestFilesFolder + filePath + ".input")
    val inputStream = new ANTLRInputStream(file)
    val lexer = new CucarachaGrammarLexer(inputStream)
    val tokens = new CommonTokenStream(lexer)
    val parser = new CucarachaGrammarParser(tokens)
    val parseTree = startRule(parser)

    astifierMethod(parseTree)
  }

  def parseInputProgram(filePath: String) = parseInput(filePath, _.program, ASTifier.visitProgram)

  def expected(filePath: String) = {
    Source.fromFile(cucaTestFilesFolder + filePath + ".expected").getLines().mkString("\n") + "\n"
  }
}