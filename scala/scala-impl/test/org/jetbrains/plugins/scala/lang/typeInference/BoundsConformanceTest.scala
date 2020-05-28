package org.jetbrains.plugins.scala.lang.typeInference

import org.jetbrains.plugins.scala.base.ScalaLightCodeInsightFixtureTestAdapter

/**
  * Created by Anton Yalyshev on 07/09/18.
  */

class BoundsConformanceTest extends ScalaLightCodeInsightFixtureTestAdapter {

  def testSCL10029(): Unit = {
    checkTextHasNoErrors(
      """
        |sealed trait Feeling
        |sealed trait Hungry extends Feeling
        |sealed trait Thirsty extends Feeling
        |
        |class Person[F <: Feeling] {
        |  def eat[T >: F <: Hungry] = println("Chomp!")
        |  def drink[T >: F <: Thirsty] = println("Glug!")
        |}
      """.stripMargin
    )
  }

  def testSCL11261(): Unit = {
    checkTextHasNoErrors(
      """
        |  sealed trait TOption
        |
        |  sealed trait TNumericLowerTypeBound
        |
        |  def sum[T2 >: TOption, T1 >: TNumericLowerTypeBound <: T2, A1, A2]
        |  (b: Map[A1, T1])
        |  (implicit f: Map[A2, T2]) = {}
      """.stripMargin
    )
  }

  def testSCL17121(): Unit = checkHasErrorAroundCaret(
    s"""
       |trait Bar
       |def t[T <: Bar](t: T): Unit = ???
       |val f : String = ???
       |t(${CARET}f)
       |""".stripMargin
  )

  def testSCL17532(): Unit = checkHasErrorAroundCaret(
    s"""
       |def findFirst(array: Array[String], key: String): Int = {
       |  def loop(index: Int): Int =
       |    if (index >= array.length) -1
       |    else if (array(index) == key) index
       |    else loop(index + 1)
       |  loop(0)
       |}
       |findFirst(Array(${CARET}1, 2, 3), "3")
       |""".stripMargin
  )
}
