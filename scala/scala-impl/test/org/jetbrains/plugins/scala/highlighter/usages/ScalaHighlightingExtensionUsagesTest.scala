package org.jetbrains.plugins.scala.highlighter.usages

import org.jetbrains.plugins.scala.ScalaVersion
import org.jetbrains.plugins.scala.traceLogger.TraceLogger

class ScalaHighlightingExtensionUsagesTest extends ScalaHighlightingUsagesTestBase {

  override protected def supportedIn(version: ScalaVersion): Boolean = version >= ScalaVersion.Latest.Scala_3_0

  def testExtensionMember(): Unit = TraceLogger.runWithTraceLogger("extension-members") {
    val code =
      s"""
         |extension (i: Int)
         |  def ${|<}test${>|}: Int = i.${|<}test${>|}
         |  def test2: Int = i.${|<}test${>|}
         |
         |val blub = 3.${|<}test${>|}
         |""".stripMargin
    doTest(code)
  }

  def testExtensionParam(): Unit = {
    val code =
      s"""
         |extension (${|<}param${>|}: Int)
         |  def test: Int = ${|<}param${>|}.test
         |  def test2: Int = ${|<}param${>|}.test
         |
         |val blub = 3.test
         |""".stripMargin
    doTest(code)
  }
}
