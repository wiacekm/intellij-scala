package org.jetbrains.plugins.scala.dfa.testlang

import org.jetbrains.plugins.scala.dfa.testlang.Interpreter._

import scala.collection.mutable
import scala.reflect.ClassTag

class Interpreter(val initContext: IterableOnce[(String, Value)]) {
  class StackFrame(val vars: Vars = mutable.Map.empty)

  private val stack = mutable.Stack(new StackFrame(initContext.iterator.to(mutable.Map)))
  def frame: StackFrame = stack.top

  def exec(ast: Ast.Node): Unit = ast match {
    case Ast.Script(main) => execBlock(main)
    case stmt: Ast.Statement => execStmt(stmt)
    case expr: Ast.Expression => execExpr(expr)
    case _: Ast.Property => fail("Cannot interpret property!")
  }

  def execBlock(block: Ast.Block): Option[Value] =
    block.iterator.flatMap(execStmt).nextOption()

  def execStmt(statement: Ast.Statement): Option[Value] = statement match {
    case Ast.ExpressionStmt(expr) =>
      execExpr(expr)
      None
    case Ast.ReturnStmt(Some(retExpr)) =>
      Some(execExpr(retExpr))
    case Ast.ReturnStmt(None) =>
      Some(Undefined)
    case Ast.IfStmt(condition, success, fail) =>
      if (execExpr(condition).cast[Bool].bool) execBlock(success)
      else fail.flatMap(execBlock)
    case Ast.WhileStmt(condition, body) =>
      def cond = execExpr(condition).cast[Bool].bool
      Iterator.continually(execBlock(body))
        .takeWhile(_ => cond)
        .flatten
        .nextOption()
    case Ast.AssignmentStmt(target, expr) =>
      def value = execExpr(expr)
      target match {
        case Ast.Identifier(name) => frame.vars.put(name, value)
        case Ast.PropertyAccess(base, name) => execExpr(base).cast[Obj].properties.put(name, value)
        case _ => fail(s"Cannot assign to $target")
      }
      None
  }

  def execExpr(expr: Ast.Expression): Value = expr match {
    case Ast.Object(properties) => Obj(properties.map(p => (p.name, execExpr(p.init))).to(mutable.Map))
    case Ast.Identifier(id) => frame.vars.getOrElse(id, fail(s"Failed to resolve local variable $id"))
    case Ast.StringLiteral(str) => Str(str)
    case Ast.BooleanLit(bool) => Bool(bool)
    case Ast.NumberLit(num) => Num(num)
    case func: Ast.Function => new Func(func, stack.foldRight(Map.empty[String, Value]) { (ctx, props) => props ++ ctx.vars })
    case Ast.Call(base, args) =>
      def argsValues = args.map(execExpr)
      execExpr(base) match {
        case f: Func => callFunc(f.func, argsValues, f.context)
        case f: CustomFunc => f.inner(argsValues)
        case noF => fail(s"$noF is not a function that can be called!")
      }
    case Ast.Operator(op, left, right) =>
      def lhs = execExpr(left)
      def rhs = execExpr(right)
      def iLhs: Int = execExpr(left).cast[Num].num
      def iRhs: Int = execExpr(right).cast[Num].num
      op match {
        case "+" => Num(iLhs + iRhs)
        case "-" => Num(iLhs - iRhs)
        case "*" => Num(iLhs * iRhs)
        case "/" => Num(iLhs / iRhs)
        case "==" => Bool(lhs == rhs)
        case "!=" => Bool(lhs != rhs)
        case ">" => Bool(iLhs > iRhs)
        case "<" => Bool(iLhs < iRhs)
        case ">=" => Bool(iLhs >= iRhs)
        case "<=" => Bool(iLhs <= iRhs)
        case _ => fail(s"Unknown operator $op")
      }
    case Ast.PropertyAccess(base, property) =>
      execExpr(base).cast[Obj]
        .properties.getOrElse(property, fail(s"Couldn't find property $property"))
    case Ast.Union(_) => fail("Cannot interpret union operator!")
    case Ast.UndefinedLiteral => Undefined
  }

  def callFunc(func: Ast.Function, args: Seq[Value], ctx: IterableOnce[(String, Value)]): Value = {
    val endlessArgs = args.iterator ++ Iterator.continually(Undefined)
    val vars = (func.params zip endlessArgs).to(mutable.Map) ++ ctx
    stack.push(new StackFrame(vars))
    try execBlock(func.block).getOrElse(Undefined)
    finally stack.pop()
  }

}

object Interpreter {
  class InterpretException(msg: String) extends Exception(msg)

  private def fail(msg: String): Nothing =
    throw new InterpretException(msg)

  final type Vars = mutable.Map[String, Value]

  sealed trait Value {
    def toInnerString: String

    final def cast[T <: Value : ClassTag]: T = this match {
      case correct: T => correct
      case _ => fail(s"Failed to cast $toInnerString into ${implicitly[ClassTag[T]].runtimeClass.getSimpleName}")
    }
  }

  final case object Undefined extends Value {
    override def toString: String = toInnerString
    def toInnerString: String = "undefined"
  }
  case class Bool(bool: Boolean) extends Value {
    override def toInnerString: String = bool.toString
  }
  case class Num(num: Int) extends Value {
    override def toInnerString: String = num.toString
  }
  case class Str(str: String) extends Value {
    override def toInnerString: String = str
  }
  case class Obj(properties: Vars = mutable.Map.empty) extends Value {
    override def toInnerString: String = properties.iterator
      .map { case (name, v) => s"$name: ${v.toInnerString}" }
      .mkString("{", ", ", "}")
  }
  class Func(val func: Ast.Function, val context: Map[String, Value]) extends Value {
    override def toInnerString: String = s"Function[at index ${func.index}]"
  }
  case class CustomFunc(name: String)(val inner: Seq[Value] => Value) extends Value {
    override def toInnerString: String = s"Function[$name]"
  }

  def mkProc(name: String)(f: Seq[Value] => Unit): (String, CustomFunc) = name -> CustomFunc(name) { args =>
    f(args)
    Undefined
  }

  def mkFunc(name: String)(f: Seq[Value] => Value): (String, CustomFunc) = name -> CustomFunc(name)(f)

  val defaultEnv: Map[String, Value] = Map(
    mkProc("println")(args => println(args.map(_.toInnerString).mkString(" "))),
  )

  def createWithDefaultEnv: Interpreter = new Interpreter(defaultEnv)

  def main(args: Array[String]): Unit = {
    val program = LangParser.parse(
      """
        |facStart = 1
        |fac = (n) => {
        |  result = facStart
        |  while (n > 1) {
        |    result = result * n
        |    n = n - 1
        |  }
        |  return result
        |}
        |
        |println("3! =", fac(3))
        |println("5! =", fac(5))
        |""".stripMargin
    )

    createWithDefaultEnv.exec(program)
  }
}