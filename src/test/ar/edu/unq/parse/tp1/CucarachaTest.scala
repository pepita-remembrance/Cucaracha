package ar.edu.unq.parse.tp1

class CucarachaTest extends BaseSpec with ASTBehaviour {

  val files = List("test00", "test01", "test02", "test03", "test04", "test05", "test06", "test07", "test08", "test09", "test10")

  files.foreach(testSerializeCorrectly)

}

trait ASTBehaviour extends BaseSpec {

  def testSerializeCorrectly(filePath: String): Unit = {
    filePath + " file" should "serialize correctly" in {
      val ast = parseInput(filePath)
      ast.serialize should === (expected(filePath))
    }
  }
}

