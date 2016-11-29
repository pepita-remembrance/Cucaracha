package ar.edu.unq.parse.tp1.assembler

import ar.edu.unq.parse.tp1.{Context, assembler}
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
      RootLabel("main"),
      Call(s"${funPrefix}main"),
      Mov(reserved1, 0),
      Call("exit")
    )
    program
  }

  private def emptyBuffer = ListBuffer.empty[NasmInstruction]

  private def assemble(fun: NasmCucaFunction): Unit = {
    implicit val addressContext = fun.addressContext
    implicit val rootLabel = RootLabel(fun.nasmName)
    val assembledBody = fun.body.foldLeft(emptyBuffer)(assemble)
    contents.append(
      rootLabel,
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

  private def assemble(buffer: ListBuffer[NasmInstruction], stmt: Statement)(implicit addressContext: AddressContext, rootLabel: Label): ListBuffer[NasmInstruction] = {
    stmt match {
      case StmtAssign(variable, valueExpr) => buffer.appendAll(assemble(valueExpr, addressContext(variable)))
      case StmtVecAssign(vecName, indexExpr, valueExpr) =>
        val temp1 = addressContext.tempVar
        val temp2 = addressContext.tempVar
        buffer.appendAll(assemble(indexExpr, temp1))
        buffer.appendAll(assemble(valueExpr, temp2))
        addressContext.released(temp2)
        addressContext.released(temp1)
        buffer.append(
          Mov(returnReg, temp1),
          Add(returnReg, 1),
          Mov(temp1, 8),
          Imul(temp1),
          Add(returnReg, addressContext(vecName))
        )
        temp2 match {
          case _: Register => buffer.append(Mov(IndirectAddress(returnReg, 0), temp2))
          case _: IndirectAddress => buffer.append(
            Push(temp2),
            Pop(IndirectAddress(returnReg, 0))
          )
        }
      case s: StatementIf =>
        val falseLabel = rootLabel.newSubLabel
        val endLabel = rootLabel.newSubLabel
        val temp = addressContext.tempVar
        buffer.appendAll(assemble(s.condition, temp))
        addressContext.released(temp)
        buffer.append(
          Cmp(temp, 0),
          JumpEq(falseLabel)
        )
        s.branchTrue.foldLeft(buffer)(assemble)
        buffer.append(
          Jump(endLabel),
          falseLabel
        )
        s.branchFalse.foldLeft(buffer)(assemble)
        buffer.append(endLabel)
      case StmtWhile(conditionExpr, body) =>
        val beginLabel = rootLabel.newSubLabel
        val endLabel = rootLabel.newSubLabel
        val temp = addressContext.tempVar
        buffer.append(beginLabel)
        buffer.appendAll(assemble(conditionExpr, temp))
        addressContext.released(temp)
        buffer.append(
          Cmp(temp, 0),
          JumpEq(endLabel)
        )
        body.foldLeft(buffer)(assemble)
        buffer.append(
          Jump(beginLabel),
          endLabel
        )
      case StmtReturn(valueExpr) => buffer.appendAll(assemble(valueExpr, returnReg))
      //Function Calls
      case StmtCall(PutNum.id, argsExprs) => buffer.appendAll(assemblePutNum(argsExprs.head))
      case StmtCall(PutChar.id, argsExprs) => buffer.appendAll(assemblePutChar(argsExprs.head))
      case StmtCall(name, argsExprs) => buffer.appendAll(assembleCucaCall(name, argsExprs))
    }
    buffer
  }

  private def assembleCucaCall(funName: String, args: Seq[Expression])(implicit addressContext: AddressContext, rootLabel: Label): List[NasmInstruction] = {
    val buffer = emptyBuffer
    if (args.nonEmpty) buffer.append(Sub(stackPointerReg, 8 * args.size))
    args.zipWithIndex.foreach {
      case (expr, i) => buffer.appendAll(assemble(expr, IndirectAddress(stackPointerReg, 8 * i)))
    }
    buffer.append(Call(s"$funPrefix$funName"))
    if (args.nonEmpty) buffer.append(Add(stackPointerReg, 8 * args.size))
    buffer.toList
  }

  private def protectTempVars(code: List[NasmInstruction], tempVars: List[NasmAddress]): List[NasmInstruction] =
    tempVars.collect { case r: Register => r } match {
      case Nil => code
      case v :: vs => (Push(v) :: protectTempVars(code, vs)) :+ Pop(v)
    }

  private def assembleBinaryExpr(expr1: Expression, expr2: Expression, finalPosition: NasmAddress, extraCode: NasmAddress => List[NasmInstruction])(implicit addressContext: AddressContext, rootLabel: Label): List[NasmInstruction] = {
    val temp = addressContext.tempVar
    val memOp = (temp, finalPosition) match {
      case (_: IndirectAddress, _: IndirectAddress) => address: NasmAddress => Mov(reserved1, address) :: extraCode(reserved1)
      case _ => extraCode
    }
    val code1 = assemble(expr1, temp)
    val code2 = assemble(expr2, finalPosition)
    addressContext.released(temp)
    code1 ++ code2 ++ memOp(temp)
  }

  private def assembleCmp(expr1: Expression, expr2: Expression, finalPosition: NasmAddress, jump: Label => NasmJump)(implicit addressContext: AddressContext, rootLabel: Label): List[NasmInstruction] = {
    val trueLabel = rootLabel.newSubLabel
    val endLabel = rootLabel.newSubLabel
    assembleBinaryExpr(expr1, expr2, finalPosition, temp => List(
      Cmp(temp, finalPosition),
      jump(trueLabel),
      Mov(finalPosition, 0),
      Jump(endLabel),
      trueLabel,
      Mov(finalPosition, -1),
      endLabel
    ))
  }

  protected[this] def assemble(expr: Expression, position: NasmAddress)(implicit addressContext: AddressContext, rootLabel: Label): List[NasmInstruction] =
    expr match {
      //Constants
      case ExprConstNum(value) => List(Mov(position, value))
      case ExprConstBool(value) => List(Mov(position, if (value) -1 else 0))
      //Logic
      case ExprNot(expression) =>
        assemble(expression, position) :+ Not(position)
      case ExprAnd(expr1, expr2) =>
        assembleBinaryExpr(expr1, expr2, position, temp => List(And(position, temp)))
      case ExprOr(expr1, expr2) =>
        assembleBinaryExpr(expr1, expr2, position, temp => List(Or(position, temp)))
      //Math
      case ExprAdd(expr1, expr2) =>
        assembleBinaryExpr(expr1, expr2, position, temp => List(Add(position, temp)))
      case ExprSub(expr1, expr2) =>
        assembleBinaryExpr(expr1, expr2, position, temp => List(Sub(temp, position), Mov(position, temp)))
      case ExprMul(expr1, expr2) =>
        assembleBinaryExpr(expr1, expr2, returnReg, temp => List(Imul(temp), Mov(position, returnReg)))
      //Compare
      case ExprLe(expr1, expr2) =>
        assembleCmp(expr1, expr2, position, JumpLe)
      case ExprGe(expr1, expr2) =>
        assembleCmp(expr1, expr2, position, JumpGe)
      case ExprLt(expr1, expr2) =>
        assembleCmp(expr1, expr2, position, JumpLt)
      case ExprGt(expr1, expr2) =>
        assembleCmp(expr1, expr2, position, JumpGt)
      case ExprEq(expr1, expr2) =>
        assembleCmp(expr1, expr2, position, JumpEq)
      case ExprNe(expr1, expr2) =>
        assembleCmp(expr1, expr2, position, JumpNe)
      //Vector
      case ExprVecMake(expressions) =>
        val (size, addresses) = addressContext.vectorVars(expressions.size)
        (Mov(size, expressions.size) ::
          expressions.zip(addresses).toList.flatMap { case (expression, address) => assemble(expression, address) }) :+
          Mov(position, stackPointerReg)
      case ExprVecLength(vecName) => Mov(returnReg, addressContext(vecName)) :: (position match {
        case _: Register => List(
          Mov(position, IndirectAddress(returnReg, 0))
        )
        case _: IndirectAddress => List(
          Push(IndirectAddress(returnReg, 0)),
          Pop(position)
        )
      })
      case ExprVecDeref(vecName, indexExpr) =>
        val temp = addressContext.tempVar
        val code = assemble(indexExpr, temp)
        addressContext.released(temp)
        code ++ List(
          Mov(returnReg, temp),
          Add(returnReg, 1),
          Mov(temp, 8),
          Imul(temp),
          Add(returnReg, addressContext(vecName))
        ) ++ (position match {
          case _: Register => List(
            Mov(position, IndirectAddress(returnReg, 0))
          )
          case _: IndirectAddress => List(
            Push(IndirectAddress(returnReg, 0)),
            Pop(position))
        })
      //Call
      case ExprCall(name, args) =>
        protectTempVars(assembleCucaCall(name, args) :+ Mov(position, returnReg), addressContext.usedRegisters.filterNot(_ == position))
      //Variable
      case ExprVar(name) => position match {
        case _: IndirectAddress => List(Push(addressContext(name)), Pop(position))
        case _: Register => List(Mov(position, addressContext(name)))
      }
    }

  implicit def intToNasmConstant(n: Int): Constant = Constant(n)

  class AddressContext extends Context[NasmAddress](varName => s"Attempt to access undefined variable: $varName") {
    private var vars = 0
    private var tempVars = 0
    private var maxTempVars = 0
    private var vectorVars = 0
    var freeRegisters = usableRegisters
    var usedRegisters = List.empty[Register]

    def stackVariables = vars + maxTempVars + vectorVars

    def parameter = this

    def variable = {
      vars += 1
      this
    }

    def vectorVars(size: Int): (IndirectAddress, List[IndirectAddress]) = {
      val vars = (0 to size+1).map(i => IndirectAddress(stackPointerReg, 8 * (i + vectorVars))).toList
      vectorVars += size + 1
      (vars.head, vars.tail)
    }

    private def tempVarInMemory: NasmAddress = {
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
    }

    def tempVar: NasmAddress =
      if (freeRegisters.isEmpty) {
        tempVarInMemory
      } else {
        val register = freeRegisters.head
        freeRegisters = freeRegisters.tail
        usedRegisters = register :: usedRegisters
        register
      }

    def released(tempVar: NasmAddress) = tempVar match {
      case r: Register =>
        freeRegisters = r :: freeRegisters
        usedRegisters = usedRegisters.filterNot(_ == r)
      case address: IndirectAddress => tempVars -= 1
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