package org.jetbrains.plugins.scala.traceLogViewer.viewer

import org.jetbrains.plugins.scala.traceLogViewer.viewer.TraceLogModel.EnclosingNode
import org.jetbrains.plugins.scala.traceLogger.Data

import java.awt.event.MouseEvent
import scala.util.Random

class TraceLogValueCellRenderer extends TraceLogBaseCellRenderer {
  private def anyToHexColor(obj: Any): String = {
    val random = new Random(obj.hashCode())
    s"#${(random.nextInt() % (1 << 24)).toHexString}"
  }

  private def wrapHtmlColor(text: String): String = {
    val color = anyToHexColor(text)
    s"""<font color="$color">$text</font>"""
  }

  private def forResult(f: (String, Data) => Unit): Unit = {
    currentNode match {
      case en: EnclosingNode =>
        en.result match {
          case Right("()") =>
          case Right(data) => f("return", data)
          case Left(msg) => f("threw", msg)
        }
      case _ =>
    }
  }

  override def setup(): Unit = {
    var pairs = currentNode.values
      .filterNot { case (name, _) => name == "this" }
    forResult((name, data) => pairs +:= name -> data)
    setValue(
      pairs
        .map { case (name, data) => s"${wrapHtmlColor(name)}: $data" }
        .mkString("<html>", ", ", "</html>")
    )
  }

  override def getToolTipText(event: MouseEvent): String = {
    if (currentNode.values.isEmpty) {
      return null
    }

    val builder = new StringBuilder

    builder.append("<html><table>")

    def add(name: String, value: String): Unit = {
      builder.append("<tr style=\"vertical-align:top>")
      builder.append(s"""<td><b style="${anyToHexColor(name)}">""")
      builder.append(wrapHtmlColor(name))
      builder.append(":</b> </td>")
      builder.append("<td> ")
      builder.append(value)
      builder.append("</td>")
      builder.append("</tr>")
    }

    forResult(add)

    for ((name, value) <- currentNode.values) {
      add(name, value)
    }

    builder.append("</table></html>")

    builder.toString()
  }
}
