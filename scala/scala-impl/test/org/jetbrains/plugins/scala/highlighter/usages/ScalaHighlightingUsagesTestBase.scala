package org.jetbrains.plugins.scala.highlighter.usages

import com.intellij.codeInsight.highlighting.{HighlightManager, HighlightUsagesHandler}
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.util.TextRange
import com.intellij.psi.PsiFile
import org.jetbrains.plugins.scala.AssertionMatchers
import org.jetbrains.plugins.scala.base.{ScalaLightCodeInsightFixtureTestAdapter, SharedTestProjectToken}
import org.jetbrains.plugins.scala.base.ScalaLightCodeInsightFixtureTestAdapter.findCaretOffsets
import org.jetbrains.plugins.scala.editor.DocumentExt
import org.jetbrains.plugins.scala.extensions.{StringExt, inWriteCommandAction}
import org.jetbrains.plugins.scala.util.Markers

abstract class ScalaHighlightingUsagesTestBase extends ScalaLightCodeInsightFixtureTestAdapter with AssertionMatchers with Markers {
  val |< = startMarker
  val >| = endMarker
  val | = startMarker + endMarker

  protected override def sharedProjectToken: SharedTestProjectToken =
    SharedTestProjectToken(classOf[ScalaHighlightingUsagesTestBase])

  def doTest(fileText: String): Unit = {
    val (fileTextWithoutMarkers, allRanges) = extractMarker(fileText.withNormalizedSeparator)
    val (expectedRanges, cursorRangesOrg) = allRanges.partition(_.getLength != 0)
    val cursorRanges =
      if (cursorRangesOrg.nonEmpty) cursorRangesOrg
      else expectedRanges.map(r => TextRange.from(r.getStartOffset + r.getLength / 2, 0))

    val highlightManager = HighlightManager.getInstance(getProject)

    for (cursor <- cursorRanges) {
      val (file, editor) = setupText(fileTextWithoutMarkers.patch(cursor.getStartOffset, CARET, 0))

      val finalFileText = file.getText

      HighlightUsagesHandler.invoke(myFixture.getProject, editor, file)

      val highlighters = editor.getMarkupModel.getAllHighlighters
      val actualRanges = highlighters.map(hr => TextRange.create(hr.getStartOffset, hr.getEndOffset)).toSeq

      highlighters.foreach(h => highlightManager.removeSegmentHighlighter(editor, h))

      val expected = rangeSeqToComparableString(cursor.getStartOffset, expectedRanges, finalFileText)
      val actual = rangeSeqToComparableString(editor.getCaretModel.getOffset, actualRanges, finalFileText)

      actual shouldBe expected
    }
  }

  private def setupText(text: String): (PsiFile, Editor) = {
    val (textActual, caretOffsets) = findCaretOffsets(text, trimText = false)

    getFixture.getEditor match {
      case null =>
        getFixture.configureByText("dummy.scala", textActual)
      case editor =>
        // optimization for sequential this.configureByText calls in a single test
        // getFixture.configureByText is quite resource consuming for simple sequence of typing tests
        inWriteCommandAction {
          editor.getDocument.setText(textActual)
          editor.getDocument.commit(getProject)
        }(getProject)
    }
    val editor = getFixture.getEditor
    editor.getCaretModel.moveToOffset(caretOffsets.head)

    (getFile, editor)
  }

  private def rangeSeqToComparableString(cursorIdx: Int, ranges: Seq[TextRange], fileText: String): String = {
    case class Mark(idx: Int, text: String)
    ranges.flatMap(r => Seq(Mark(r.getStartOffset, "|<"), Mark(r.getEndOffset, ">|")))
      .appended(Mark(cursorIdx, "<cursor>"))
      .sortBy(_.idx)
      .foldRight(fileText) {
        case (Mark(idx, mark), fileText) =>
          fileText.patch(idx, mark, 0)
      }
  }
}
