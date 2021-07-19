package org.jetbrains.plugins.scala
package highlighter
package usages

class ScalaHighlightConstructorInvocationUsagesTest extends ScalaHighlightingUsagesTestBase {

  def testClassDefinitionUsage(): Unit = {
    val code =
      s"""
         |object Obj {
         |  class ${|<}Test${>|}
         |  val x: ${|<}Test${>|} = new ${|<}Test${>|}
         |}
       """.stripMargin
    doTest(code)
  }

  def testAuxiliaryConstructorUsage(): Unit = {
    val code =
      s"""
         |object Obj {
         |  class ${|<}Test${>|} {
         |    def ${|<}this${>|}(i: Int) = this()
         |  }
         |  val x: ${|<}Test${>|} = new ${|<}Te${|}st${>|}(3)
         |  new ${|<}Test${>|}
         |}
       """.stripMargin
    doTest(code)
  }

  def testAuxiliaryConstructorInvocationUsage(): Unit = {
    val code =
      s"""
         |object Obj {
         |  class Test {
         |    def ${|<}th${|}is${>|}(i: Int) = this()
         |  }
         |  val x: Test = new ${|<}Test${>|}(3)
         |  new Test
         |}
       """.stripMargin
    doTest(code)
  }

  def testTraitTypeAnnotationUsage(): Unit = {
    val code =
      s"""
         |object Obj {
         |  trait ${|<}Test${>|}
         |  val x: ${|<}Test${>|} = new ${|<}Test${>|} {}
         |  new ${|<}Test${>|} {}
         |}
       """.stripMargin
    doTest(code)
  }

  def testCaseClassUsageOn(): Unit = doTest(
    s"""
       |object Obj {
       |  case class ${|<}Test${>|}(i: Int)
       |
       |  val t: ${|<}Test${>|} = ${|<}Test${>|}(3)
       |  new ${|<}Test${>|}(3)
       |}
     """.stripMargin
  )
}
