package ar.edu.unq.parse.tp1

import ar.edu.unq.parse.tp1.semantics.{SemanticException, TypeException}

import scala.reflect.ClassTag

class CucarachaTest extends BaseSpec {

  val correctlyFormedPrograms = List("test01", "test02", "test03", "test05", "test06", "test08", "test09", "test10")
  val incorreclyFormedPrograms = List("test00", "test04", "test07")

  val allPrograms = correctlyFormedPrograms ++ incorreclyFormedPrograms

  allPrograms.foreach(testSerializeCorrectly)

  correctlyFormedPrograms.foreach(testCorrectlyFormed)

  testIncorrectlyFormed[SemanticException]("test00", "Program has no main function")
  testIncorrectlyFormed[SemanticException]("test04", "Variable x in function conj is undefined")
  testIncorrectlyFormed[TypeException]("test07", "Type Int does not match Bool")


  def testSerializeCorrectly(programFilePath: String): Unit = {
    programFilePath + " file" should "serialize correctly" in {
      val ast = parseInputProgram(programFilePath)
      ast.serialize should ===(expected(programFilePath))
    }
  }

  def testCorrectlyFormed(programFilePath: String): Unit = {
    programFilePath should "be correctly formed" in {
      val ast = parseInputProgram(programFilePath)
      noException should be thrownBy ast.semanticCheck()
    }
  }

  def testIncorrectlyFormed[E <: Exception : ClassTag](programFilePath: String, exceptionMessage: String): Unit = {
    programFilePath should "be incorrectly formed" in {
      val ast = parseInputProgram(programFilePath)
      the[E] thrownBy ast.semanticCheck() should have message exceptionMessage
    }
  }

}


