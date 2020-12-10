package org.jetbrains.plugins.scala.lang.dfa

import org.jetbrains.plugins.scala.dfa.{DfInt, DfNothing}

class SimpleDfaTests extends ScalaDfaTestBase {
  def testSimple(): Unit = check(
    s"""
       |val a = 10
       |
       |${m0}a
       |""".stripMargin,
    0 -> DfInt(10),
  )

  def testSimpleOverride(): Unit = check(
    s"""
       |var a = 10
       |
       |${m0}a
       |
       |a = 20
       |
       |${m1}a
       |""".stripMargin,
    0 -> DfInt(10),
    1 -> DfInt(20),
  )

  def testIf(): Unit = check(
    s"""
       |def test(b: Boolean): Int =
       |  ${m0}if (b) 10
       |  else 20
       |""".stripMargin,
    0 -> DfInt.Top
  )

  def testSimpleFunctionCall(): Unit = check(
    s"""
       |def x(): Int = 3
       |def test(b: Boolean): Int =
       |  x$m0()
       |""".stripMargin,
    0 -> DfInt.Top
  )

}
