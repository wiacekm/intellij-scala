package org.jetbrains.plugins.scala.annotator

class MethodCallAnnotatorTest extends ScalaHighlightingTestBase {
  def testSCL17121(): Unit =
    assertMatches(
      errorsFromScalaCode(
        """
          |trait Bar
          |def t[T <: Bar](t: T): Unit = ???
          |val f : String = ???
          |t(f)
          |""".stripMargin
      )
    ) {
      case Error(_, "Type mismatch, expected: Bar, actual: String") :: Nil =>
    }

  def testSCL17532(): Unit =
    assertMatches(
      errorsFromScalaCode(
        """
          |def findFirst(array: Array[String], key: String): Int = {
          |  def loop(index: Int): Int =
          |    if (index >= array.length) -1
          |    else if (array(index) == key) index
          |    else loop(index + 1)
          |  loop(0)
          |}
          |findFirst(Array(1, 2, 3), "3")
          |""".stripMargin
      )
    ) {
      case Error(_, "Type mismatch, expected: Array[String], actual: Array[Int]") :: Nil =>
    }
}
