package ar.edu.unq.parse.tp1.ast.instructions

import ar.edu.unq.parse.tp1.ast.CucaTypes.Type
import ar.edu.unq.parse.tp1.ast.expressions.Expression
import ar.edu.unq.parse.tp1.ast.{ASTTree, CucaFunction, IndentableStringBuilder}
import ar.edu.unq.parse.tp1.semantics.Context

trait Instruction extends ASTTree {
  def buildContext()(implicit programContext: Context[CucaFunction], localContext: Context[Type]): Unit = {}
}

case class StmtAssign(id: String, value: Expression) extends Instruction {
  def serializeContents(builder: IndentableStringBuilder): Unit = {
    builder.appendln(id)
    value.serialize(builder)
  }

  override def buildContext()(implicit programContext: Context[CucaFunction], localContext: Context[Type]): Unit = {
    val infered = value.infer
    if (localContext.contains(id)) localContext(id) <===> infered
    else localContext(id) = infered
  }
}

case class StmtVecAssign(id: String, position: Expression, value: Expression) extends Instruction {
  def serializeContents(builder: IndentableStringBuilder): Unit = {
    builder.appendln(id)
    position.serialize(builder)
    value.serialize(builder)
  }

}

trait StatementIf extends Instruction {
  def condition: Expression

  def branchTrue: Seq[Instruction]

  def serializeContents(builder: IndentableStringBuilder): Unit = {
    condition.serialize(builder)
    wrapInBlock(builder, branchTrue)
  }

  override def buildContext()(implicit programContext: Context[CucaFunction], localContext: Context[Type]): Unit = branchTrue.foreach(_.buildContext())
}

case class StmtIf(condition: Expression, branchTrue: Seq[Instruction]) extends StatementIf

case class StmtIfElse(condition: Expression, branchTrue: Seq[Instruction], branchFalse: Seq[Instruction]) extends StatementIf {
  override def serializeContents(builder: IndentableStringBuilder): Unit = {
    super.serializeContents(builder)
    wrapInBlock(builder, branchFalse)
  }

  override def buildContext()(implicit programContext: Context[CucaFunction], localContext: Context[Type]): Unit = {
    super.buildContext()
    branchFalse.foreach(_.buildContext())
  }
}

case class StmtWhile(condition: Expression, body: Seq[Instruction]) extends Instruction {
  def serializeContents(builder: IndentableStringBuilder): Unit = {
    condition.serialize(builder)
    wrapInBlock(builder, body)
  }

  override def buildContext()(implicit programContext: Context[CucaFunction], localContext: Context[Type]): Unit = body.foreach(_.buildContext())
}

case class StmtReturn(value: Expression) extends Instruction {
  def serializeContents(builder: IndentableStringBuilder): Unit = value.serialize(builder)

}

case class StmtCall(id: String, params: Seq[Expression]) extends Instruction {
  def serializeContents(builder: IndentableStringBuilder): Unit = {
    builder.appendln(id)
    params.foreach(_.serialize(builder))
  }
}