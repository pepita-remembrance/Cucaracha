package ar.edu.unq.parse.tp1.ast.statements

import ar.edu.unq.parse.tp1.ast.CucaTypes._
import ar.edu.unq.parse.tp1.ast.expressions.Expression
import ar.edu.unq.parse.tp1.ast.{ASTTree, CucaFunction, IndentableStringBuilder}
import ar.edu.unq.parse.tp1.semantics.{Context, SemanticException}

trait Statement extends ASTTree {
  def buildContext()(implicit programContext: Context[CucaFunction], localContext: Context[Type]): Unit
}

case class StmtAssign(id: String, value: Expression) extends Statement {
  def serializeContents(builder: IndentableStringBuilder): Unit = {
    builder.appendln(id)
    value.serialize(builder)
  }

  def buildContext()(implicit programContext: Context[CucaFunction], localContext: Context[Type]): Unit = {
    val infered = value.infer
    if (localContext.contains(id)) localContext(id) <===> infered
    else localContext(id) = infered
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
}

trait StatementIf extends Statement {
  def condition: Expression

  def branchTrue: Seq[Statement]

  def serializeContents(builder: IndentableStringBuilder): Unit = {
    condition.serialize(builder)
    wrapInBlock(builder, branchTrue)
  }

  def buildContext()(implicit programContext: Context[CucaFunction], localContext: Context[Type]): Unit = {
    condition <===> CucaBool
    branchTrue.foreach(_.buildContext())
  }
}

case class StmtIf(condition: Expression, branchTrue: Seq[Statement]) extends StatementIf

case class StmtIfElse(condition: Expression, branchTrue: Seq[Statement], branchFalse: Seq[Statement]) extends StatementIf {
  override def serializeContents(builder: IndentableStringBuilder): Unit = {
    super.serializeContents(builder)
    wrapInBlock(builder, branchFalse)
  }

  override def buildContext()(implicit programContext: Context[CucaFunction], localContext: Context[Type]): Unit = {
    super.buildContext()
    branchFalse.foreach(_.buildContext())
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
}

case class StmtReturn(value: Expression) extends Statement {
  def serializeContents(builder: IndentableStringBuilder): Unit = value.serialize(builder)

  def buildContext()(implicit programContext: Context[CucaFunction], localContext: Context[Type]): Unit = {}
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
}