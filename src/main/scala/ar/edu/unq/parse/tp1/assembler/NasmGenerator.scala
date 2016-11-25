package ar.edu.unq.parse.tp1.assembler

import ar.edu.unq.parse.tp1.Context
import ar.edu.unq.parse.tp1.PredefinedFunctions.{PutChar, PutNum}
import ar.edu.unq.parse.tp1.ast.expressions._
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
      Mov(reserved1, 0),
      Call("exit")
    )
    program
  }

  private def emptyBuffer = ListBuffer.empty[NasmInstruction]

  private def assemble(fun: NasmCucaFunction): Unit = {
    implicit val addressContext = fun.addressContext
    val assembledBody = fun.body.foldLeft(emptyBuffer)(assemble)
    contents.append(
      Label(fun.nasmName),
      Push(oldStackPointerReg),
      Mov(oldStackPointerReg, stackPointerReg)
    )
    if (fun.stackVariables != 0) contents.append(Sub(stackPointerReg, 8 * fun.stackVariables))
    contents.appendAll(assembledBody)
    if (fun.stackVariables != 0) contents.append(Mov(stackPointerReg, oldStackPointerReg))
    contents.append(
      Pop(oldStackPointerReg),
      Ret
    )
  }

  private def assemble(buffer: ListBuffer[NasmInstruction], stmt: Statement)(implicit addressContext: AddressContext): ListBuffer[NasmInstruction] = {
    stmt match {
      case StmtAssign(variable, value) => buffer.appendAll(assemble(value, addressContext(variable)))
      case StmtVecAssign(name, index, value) => ???
      case StmtIf(condition, branchTrue) => ???
      case StmtIfElse(condition, branchTrue, branchFalse) => ???
      case StmtWhile(condition, body) => ???
      case StmtReturn(value) => buffer.appendAll(assemble(value, returnReg))
      //Function Calls
      case StmtCall(PutNum.id, args) =>
        buffer.appendAll(assemble(args.head, reserved2))
        buffer.append(
          Mov(reserved1, Data("lli_format_string")),
          Mov(returnReg, 0),
          Call("printf")
        )
      case StmtCall(PutChar.id, args) =>
        buffer.appendAll(assemble(args.head, reserved1))
        buffer.append(Call("putchar"))
      case StmtCall(name, args) =>
        if (args.nonEmpty) buffer.append(Sub(stackPointerReg, 8 * args.size))
        args.zipWithIndex.foreach {
          case (expr, i) => buffer.appendAll(assemble(expr, IndirectAddress(stackPointerReg, 8 * i)))
        }
        buffer.append(Call(s"$funPrefix$name"))
        if (args.nonEmpty) buffer.append(Add(stackPointerReg, 8 * args.size))
    }
    buffer
  }

  //Ojo que la posicion no sea un registro "usable"!
  private def assemble(expr: Expression, position: NasmAddress)(implicit addressContext: AddressContext): Seq[NasmInstruction] = {
    addressContext.resetTempVars()
    assembleRecursive(expr, position)
  }

  private def assembleRecursive(expr: Expression, position: NasmAddress)(implicit addressContext: AddressContext): Seq[NasmInstruction] =
    expr match {
      //Constants
      case ExprConstNum(value) => List(Mov(position, value))
      case ExprConstBool(value) => List(Mov(position, if (value) -1 else 0))
      //Logic
      case ExprNot(expression) => assembleRecursive(expression, position) :+ Not(position)
      case ExprAnd(expr1, expr2) =>
        val temp = addressContext.tempVar
        assembleRecursive(expr1, position) ++ assembleRecursive(expr2, temp) :+ And(position, temp)
      case ExprOr(expr1, expr2) =>
        val temp = addressContext.tempVar
        assembleRecursive(expr1, position) ++ assembleRecursive(expr2, temp) :+ Or(position, temp)
      //Math
      case ExprAdd(expr1, expr2) =>
        val temp = addressContext.tempVar
        assembleRecursive(expr1, position) ++ assembleRecursive(expr2, temp) :+ Add(position, temp)
      case ExprSub(expr1, expr2) =>
        val temp = addressContext.tempVar
        assembleRecursive(expr1, position) ++ assembleRecursive(expr2, temp) :+ Sub(position, temp)
      case ExprMul(expr1, expr2) =>
        val temp = addressContext.tempVar
        assembleRecursive(expr1, temp) ++ assembleRecursive(expr2, returnReg) :+ Imul(temp) :+ Mov(position, returnReg)
      //Compare
      case ExprLe(expr1, expr2) => ???
      case ExprGe(expr1, expr2) => ???
      case ExprLt(expr1, expr2) => ???
      case ExprGt(expr1, expr2) => ???
      case ExprEq(expr1, expr2) => ???
      case ExprNe(expr1, expr2) => ???
      //Vector
      case ExprVecMake(expressions) => ???
      case ExprVecLength(vecName) => ???
      case ExprVecDeref(vecName, index) => ???
      //Call
      case ExprCall(name, args) => ???
      //Variable
      case ExprVar(name) => position match {
        case _: IndirectAddress => List(Push(addressContext(name)), Pop(position))
        case _: Register => List(Mov(position, addressContext(name)))
      }
    }

  implicit def intToNasmConstant(n: Int): Constant = Constant(n)

  class AddressContext extends Context[NasmAddress](varName => s"Attempt to access undefined variable: $varName") {
    var vars = 0
    var tempVars = 0
    var maxTempVars = 0
    var freeRegisters = usableRegisters
    var usedRegisters = List.empty[NasmAddress]

    def stackVariables = vars + maxTempVars

    def parameter = this

    def variable = {
      vars += 1
      this
    }

    def tempVar: NasmAddress = {
      if (freeRegisters.isEmpty) {
        tempVars += 1
        if (tempVars > maxTempVars) {
          val newVarName = s"${funPrefix}temp_var_${maxTempVars.toString}"
          val newVarAddress = IndirectAddress(oldStackPointerReg, -8 * (stackVariables + 1))
          this (newVarName) = newVarAddress
          maxTempVars += 1
          newVarAddress
        } else {
          this (s"${funPrefix}temp_var_${(tempVars - 1).toString}")
        }
      } else {
        val register = freeRegisters.head
        freeRegisters = freeRegisters.tail
        usedRegisters = register :: usedRegisters
        register
      }
    }

    def resetTempVars() = {
      freeRegisters = usableRegisters
      usedRegisters = List.empty[NasmAddress]
      tempVars = 0
    }

  }

  implicit class NasmCucaFunction(fun: CucaFunction) {
    def nasmName = funPrefix ++ fun.id.toLowerCase

    def stackVariables = addressContext.stackVariables

    def body = fun.body

    lazy val addressContext: AddressContext = {
      val context = new AddressContext
      fun.params.zipWithIndex.foreach {
        case (Parameter(name, _), i) => context.parameter(name) = IndirectAddress(oldStackPointerReg, 8 * (i + 2))
      }
      val localVariables = allAssignements().map(_.id).diff(fun.params.map(_.id)).distinct
      localVariables.zipWithIndex.foreach {
        case (name, i) => context.variable(name) = IndirectAddress(oldStackPointerReg, -8 * (i + 1))
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

}