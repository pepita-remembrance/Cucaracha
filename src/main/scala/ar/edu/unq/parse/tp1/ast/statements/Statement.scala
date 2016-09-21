package ar.edu.unq.parse.tp1.ast.statements

import ar.edu.unq.parse.tp1.ast.CucaTypes._
import ar.edu.unq.parse.tp1.ast.expressions.Expression
import ar.edu.unq.parse.tp1.ast.{ASTTree, CucaFunction, IndentableStringBuilder}
import ar.edu.unq.parse.tp1.semantics.{Context, SemanticException}

trait Statement extends ASTTree {
  def buildContext()(implicit programContext: Context[CucaFunction], localContext: Context[Type]): Unit

  def iterpret(implicit programContext: Context[CucaFunction], localContext: Context[Any]):Unit


  implicit class Block(statements:Seq[Statement]) {
    def interpret(implicit programContext: Context[CucaFunction], localContext: Context[Any]): Unit = {
      statements.foreach(_.iterpret)
    }
  }
}

case class StmtAssign(id: String, value: Expression) extends Statement {
  def serializeContents(builder: IndentableStringBuilder): Unit = {
    builder.appendln(id)
    value.serialize(builder)
  }

  def buildContext()(implicit programContext: Context[CucaFunction], localContext: Context[Type]): Unit = {
    val inferred = value.infer
    if (localContext.contains(id)) localContext(id) <===> inferred
    else localContext(id) = inferred
  }

  override def iterpret(implicit programContext: Context[CucaFunction], localContext: Context[Any]): Unit = {
    localContext(id) = value.eval
  }
}

case class StmtVecAssign(id: String, position: Expression, value: Expression) extends Statement {
  def serializeContents(builder: IndentableStringBuilder): Unit = {
    builder.appendln(id)
    position.serialize(builder)
    value.serialize(builder)
  }

  def buildContext()(implicit programContext: Context[CucaFunction], localContext: Context[Type]): Unit = {
    localContext(id) <===> CucaVec
    value <===> position <===> CucaInt
  }


  override def iterpret(implicit programContext: Context[CucaFunction], localContext: Context[Any]): Unit = {
    val vec = fetch[InternalVec](id)
    vec(position.eval.asInstanceOf[Int]) = value.eval.asInstanceOf[Int]
  }
}

trait StatementIf extends Statement {
  def condition: Expression

  def branchTrue: Seq[Statement]

  def branchFalse: Seq[Statement]

  def serializeContents(builder: IndentableStringBuilder): Unit = {
    condition.serialize(builder)
    wrapInBlock(builder, branchTrue)
  }

  def buildContext()(implicit programContext: Context[CucaFunction], localContext: Context[Type]): Unit = {
    condition <===> CucaBool
    branchTrue.foreach(_.buildContext())
    branchFalse.foreach(_.buildContext())
  }

  override def iterpret(implicit programContext: Context[CucaFunction], localContext: Context[Any]): Unit = {
    if (condition.eval.asInstanceOf[Boolean]) {
      branchTrue.interpret
    } else {
      branchFalse.interpret
    }
  }
}

case class StmtIf(condition: Expression, branchTrue: Seq[Statement]) extends StatementIf {
  override def branchFalse: Seq[Statement] = Seq.empty
}

case class StmtIfElse(condition: Expression, branchTrue: Seq[Statement], branchFalse: Seq[Statement]) extends StatementIf {
  override def serializeContents(builder: IndentableStringBuilder): Unit = {
    super.serializeContents(builder)
    wrapInBlock(builder, branchFalse)
  }
}

case class StmtWhile(condition: Expression, body: Seq[Statement]) extends Statement {
  def serializeContents(builder: IndentableStringBuilder): Unit = {
    condition.serialize(builder)
    wrapInBlock(builder, body)
  }

  override def buildContext()(implicit programContext: Context[CucaFunction], localContext: Context[Type]): Unit = {
    condition <===> CucaBool
    body.foreach(_.buildContext())
  }

  override def iterpret(implicit programContext: Context[CucaFunction], localContext: Context[Any]): Unit = {
    while (condition.eval.asInstanceOf[Boolean]) {
      body.interpret
    }
  }

}

case class StmtReturn(value: Expression) extends Statement {
  def serializeContents(builder: IndentableStringBuilder): Unit = value.serialize(builder)

  def buildContext()(implicit programContext: Context[CucaFunction], localContext: Context[Type]): Unit = {}

  override def iterpret(implicit programContext: Context[CucaFunction], localContext: Context[Any]): Unit = {
    // do nothing
  }
}

case class StmtCall(id: String, args: Seq[Expression]) extends Statement {
  def serializeContents(builder: IndentableStringBuilder): Unit = {
    builder.appendln(id)
    args.foreach(_.serialize(builder))
  }

  def buildContext()(implicit programContext: Context[CucaFunction], localContext: Context[Type]): Unit = {
    val expectedParams = programContext(id).params
    val expected = expectedParams.size
    val actual = args.size
    if (expected == actual) {
      (expectedParams zip args).foreach { t =>
        val (param, expr) = t
        param.paramType <===> expr
      }
    } else throw SemanticException(s"Function $id expects $expected arguments but was called with $actual")
  }

  override def iterpret(implicit programContext: Context[CucaFunction], localContext: Context[Any]): Unit = programContext(id).evalWith(args.map(_.eval))
}