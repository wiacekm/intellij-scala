package org.jetbrains.plugins.scala.util

import org.jetbrains.plugins.scala.util.DebugLogger._
import upickle.default.{ReadWriter => RW, _}

import java.io.FileWriter
import scala.runtime.NonLocalReturnControl

private[this] class DebugLogger(writer: java.io.Writer) {
  def log(msg: String, values: Seq[sourcecode.Text[Any]]): Unit = {
    writeMsg(LogKind.Msg(msg, values.map(text => text.source -> text.value.toString)))
  }

  def func[T](body: => T, args: sourcecode.Args): T = {
    val argValues = args.value.flatten.map(s => (s.source, s.value.toString))
    writeMsg(LogKind.FunctionStart(argValues))

    try {
      val result = body
      writeMsg(LogKind.FunctionEnd(FunctionResult.Succeed(result.toString)))
      result
    } catch {
      case e: NonLocalReturnControl[_] =>
        writeMsg(LogKind.FunctionEnd(FunctionResult.Succeed(e.value.toString)))
        throw e
      case e: Throwable =>
        writeMsg(LogKind.FunctionEnd(FunctionResult.Failed(e.toString)))
        throw e
    }
  }

  private[this] def writeMsg(kind: LogKind): Unit = {
    val stackTrace = Thread.currentThread().getStackTrace.toSeq.slice(4, 9).map {
      e => StackTraceEntry(e.getMethodName, e.getClassName, e.getLineNumber)
    }
    write(DebugLogMsg(kind, stackTrace))
  }

  private[this] def write(msg: DebugLogMsg): Unit = {
    writeTo(msg, writer)
    writer.append('\n')
    writer.flush()
  }
}

object DebugLogger {
  private val loggers = ThreadLocal.withInitial[DebugLogger](createDebugLogger _)

  def log(msg: => String, values: sourcecode.Text[Any]*): Unit =
    loggers.get().log(msg, values)

  def record(values: sourcecode.Text[Any]*): Unit =
    loggers.get().log("", values)

  def func[T]()(body: => T)(implicit args: sourcecode.Args): T =
    loggers.get().func(body, args)

  def func[T](self: AnyRef)(body: => T)(implicit args: sourcecode.Args): T =
    loggers.get().func(body, args.copy(value = Seq(sourcecode.Text(self, "this")) +: args.value))

  
  private def createDebugLogger(): DebugLogger = {
    val thread = Thread.currentThread()
    val home = System.getProperty("user.home")
    val path = s"$home/debug-logger-${thread.getName}-${thread.getId}.log"
    new DebugLogger(new FileWriter(path))
  }
  

  private final case class DebugLogMsg(kind: LogKind, stackTrace: Seq[StackTraceEntry])

  private object DebugLogMsg {
    implicit val rw: RW[DebugLogMsg] = macroRW
  }

  
  private final case class StackTraceEntry(method: String, className: String, line: Int)

  private object StackTraceEntry {
    implicit val rw: RW[StackTraceEntry] = macroRW
  }

  
  private sealed trait LogKind
  private object LogKind {
    implicit val rw: RW[LogKind] = macroRW

    @upickle.implicits.key("msg")
    final case class Msg(msg: String, variables: Seq[(String, String)]) extends LogKind

    @upickle.implicits.key("func-start")
    final case class FunctionStart(args: Seq[(String, String)]) extends LogKind

    @upickle.implicits.key("func-end")
    final case class FunctionEnd(result: FunctionResult) extends LogKind


    object Msg {
      implicit val rw: RW[Msg] = macroRW
    }

    object FunctionStart {
      implicit val rw: RW[FunctionStart] = macroRW
    }

    object FunctionEnd {
      implicit val rw: RW[FunctionEnd] = macroRW
    }
  }
  
  
  private sealed trait FunctionResult

  private object FunctionResult {
    implicit val rw: RW[FunctionResult] = macroRW

    @upickle.implicits.key("succ")
    final case class Succeed(result: String) extends FunctionResult
    @upickle.implicits.key("fail")
    final case class Failed(exception: String) extends FunctionResult

    object Succeed {
      implicit val rw: RW[Succeed] = macroRW
    }

    object Failed {
      implicit val rw: RW[Failed] = macroRW
    }
  }
}