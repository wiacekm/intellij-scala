package org.jetbrains.plugins.scala.lang.dfa

import com.intellij.psi.util.PsiTreeUtil
import org.jetbrains.plugins.scala.base.ScalaLightCodeInsightFixtureTestAdapter
import org.jetbrains.plugins.scala.dfa.DfAny
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import org.jetbrains.plugins.scala.util.MarkersUtils
import org.junit.Assert._

class ScalaDfaTestBase extends ScalaLightCodeInsightFixtureTestAdapter {
  def start(i: Int): String = MarkersUtils.start(i)
  def end(i: Int): String = MarkersUtils.start(i)
  def marker(i: Int): String = MarkersUtils.start(i) + MarkersUtils.endMarker(i)

  def m0: String = marker(0)
  def m1: String = marker(1)
  def m2: String = marker(2)
  def m3: String = marker(3)
  def m4: String = marker(4)
  def m5: String = marker(5)


  def check(code: String, expectedResults: (Int, DfAny)*): Unit = {
    val (codeWithoutMarkers, ranges) = MarkersUtils.extractMarker(code.strip)
    assert(ranges.length == expectedResults.size)
    val actualFile = configureFromFileText(codeWithoutMarkers)

    val expectedResultFor = expectedResults.toMap
    for ((range, i) <- ranges.zipWithIndex) {
      val expected = expectedResultFor(i)
      val psi =
        if (range.getStartOffset == range.getEndOffset) PsiTreeUtil.getNonStrictParentOfType(actualFile.findElementAt(range.getStartOffset), classOf[ScExpression])
        else PsiTreeUtil.findElementOfClassAtRange(actualFile, range.getStartOffset, range.getEndOffset, classOf[ScExpression])
      assert(psi != null)

      def selected = s"<${psi.getText}>[Range $i, ${psi.getTextRange}]"
      psi.dfaValue match {
        case Some(value) => assert(value == expected, s"For $selected found $value, but expected $expected")
        case None => fail(s"Couldn't calculate value for $selected")
      }
    }
  }
}
