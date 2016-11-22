package ar.edu.unq.parse.tp1.assembler

import ar.edu.unq.parse.tp1.Context
import ar.edu.unq.parse.tp1.PredefinedFunctions.{PutChar, PutNum}
import ar.edu.unq.parse.tp1.ast.expressions.{ExprConstNum, Expression}
import ar.edu.unq.parse.tp1.ast.statements._
import ar.edu.unq.parse.tp1.ast.{CucaFunction, Parameter, Program}

import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag

abstract class NasmGenerator(prog: Program) {
  this: ExecutionEnviroment =>
  def funPrefix = "cuca_"

  val program = new NasmProgram

  def contents = program.textSection.contents

  def generate: NasmProgram = {
    prog.functions.foreach(assemble(_))
    contents.append(
      Label("main"),
      Call(s"${funPrefix}main"),
      Mov(reg1, 0),
      Call("exit")
    )
    program
  }

  private def emptyBuffer = ListBuffer.empty[NasmInstruction]

  private def assemble(fun: NasmCucaFunction): Unit = {
    implicit val variableContext = fun.variableContext
    val assembledBody = fun.body.foldLeft(emptyBuffer)(assemble)
    contents.append(
      Label(fun.nasmName),
      Push(oldStackPointerReg),
      Mov(oldStackPointerReg, stackPointerReg),
      Sub(oldStackPointerReg, 8 * fun.totalLocalVariables)
    )
    contents.appendAll(assembledBody)
    contents.append(
      Mov(oldStackPointerReg, stackPointerReg),
      Pop(oldStackPointerReg),
      Ret
    )
  }

  private def assemble(buffer: ListBuffer[NasmInstruction], stmt: Statement)(implicit variableContext: Context[NasmAddress]): ListBuffer[NasmInstruction] = {
    stmt match {
      case StmtCall(PutNum.id, args) =>
        buffer.appendAll(assemble(args.head, reg2))
        buffer.append(
          Mov(reg1, Data("lli_format_string")),
          Mov(returnReg, 0),
          Call("printf")
        )
      case StmtCall(PutChar.id, args) =>
        buffer.appendAll(assemble(args.head, reg1))
        buffer.append(Call("putchar"))
      case StmtCall(name, args) =>
        buffer.append(Sub(stackPointerReg, 8 * args.size))
        args.zipWithIndex.foreach {
          case (expr, i) => buffer.appendAll(assemble(expr, IndirectAddress(stackPointerReg, 8 * i)))
        }
        buffer.append(
          Call(s"$funPrefix$name"),
          Add(stackPointerReg, 8 * args.size)
        )
      case _ =>
    }
    buffer
  }

  private def assemble(expr: Expression, position: NasmAddress)(implicit variableContext: Context[NasmAddress]): Seq[NasmInstruction] = expr match {
    case ExprConstNum(value) => List(Mov(position, value))
  }

  implicit class NasmCucaFunction(fun: CucaFunction) {
    def nasmName = funPrefix ++ fun.id.toLowerCase

    def totalLocalVariables = allAssignements().map(_.id).distinct.size

    def body = fun.body

    lazy val variableContext: Context[NasmAddress] = {
      val context = new Context[NasmAddress](varName => s"Variable $varName is undefined")
      fun.params.zipWithIndex.foreach {
        case (Parameter(name, _), i) => context(name) = IndirectAddress(oldStackPointerReg, 8 * (i + 1))
      }
      val localVariables = allAssignements().map(_.id).distinct
      localVariables.zipWithIndex.foreach {
        case (variable, i) => context(variable) = IndirectAddress(oldStackPointerReg, -8 * i)
      }
      context
    }

    def allStatementsIn[T <: Statement : ClassTag](block: Seq[Statement]): Seq[T] = {
      block.foldLeft(ListBuffer.empty[T]) { case (acc, stmt) =>
        stmt match {
          case s: T => acc.append(s)
          case _ =>
        }
        stmt match {
          case s: StatementIf =>
            acc.appendAll(allStatementsIn[T](s.branchTrue))
            acc.appendAll(allStatementsIn[T](s.branchFalse))
          case s: StmtWhile =>
            acc.appendAll(allStatementsIn[T](s.body))
          case _ =>
        }
        acc
      }
    }

    def allAssignements() = allStatementsIn[StmtAssign](fun.body)
  }

  implicit def intToNasmConstant(n: Int): Constant = Constant(n)

}