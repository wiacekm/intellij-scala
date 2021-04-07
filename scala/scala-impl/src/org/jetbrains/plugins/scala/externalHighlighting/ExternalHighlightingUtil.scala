package org.jetbrains.plugins.scala.externalHighlighting

import com.intellij.codeInsight.daemon.impl.{HighlightInfo, UpdateHighlightersUtil}
import com.intellij.openapi.editor.{Document, Editor, EditorFactory}
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.TextRange
import com.intellij.psi.{JavaTokenType, PsiElement, PsiFile, PsiJavaToken, PsiManager, PsiWhiteSpace}
import com.intellij.xml.util.XmlStringUtil
import org.jetbrains.plugins.scala.editor.DocumentExt
import org.jetbrains.plugins.scala.extensions.{PsiElementExt, ToNullSafe, invokeLater}
import org.jetbrains.plugins.scala.externalHighlighting.ExternalHighlighting.Pos

import java.util.Collections
import scala.jdk.CollectionConverters.SeqHasAsJava

object ExternalHighlightingUtil {

  def setHighlightings(editor: Editor,
                       highlightings: Set[ExternalHighlighting],
                       group: Int,
                       onlyFirstLineInToolTip: Boolean): Unit =
    editor.getProject.nullSafe.foreach { project =>
      val document = editor.getDocument
      invokeLater {
        val highlightInfos = highlightings.flatMap(toHighlightInfo(_, editor, group, onlyFirstLineInToolTip))
        UpdateHighlightersUtil.setHighlightersToEditor(
          project,
          document, 0, document.getTextLength,
          highlightInfos.toSeq.asJava,
          editor.getColorsScheme,
          group
        )
      }
    }

  def eraseAllHighlightings(project: Project, group: Int): Unit =
    for {
      editor <- EditorFactory.getInstance.getAllEditors
      editorProject <- Option(editor.getProject)
      if editorProject == project
    } invokeLater {
      if (!project.isDisposed) {
        val document = editor.getDocument
        UpdateHighlightersUtil.setHighlightersToEditor(
          project,
          document, 0, document.getTextLength,
          Collections.emptyList(),
          editor.getColorsScheme,
          group
        )
      }
    }

  private def toHighlightInfo(highlighting: ExternalHighlighting,
                              editor: Editor,
                              group: Int,
                              onlyFirstLineInToolTip: Boolean): Option[HighlightInfo] = {
    val message = highlighting.message
    //noinspection ReferencePassedToNls
    for {
      startOffset <- convertToOffset(highlighting.from, message, editor)
      highlightRange <- calculateRangeToHighlight(startOffset, highlighting.to, message, editor)
      description = message.trim.stripSuffix(lineText(message))
      tooltip = if (onlyFirstLineInToolTip) description else message
    } yield HighlightInfo
      .newHighlightInfo(highlighting.highlightType)
      .range(highlightRange)
      .description(description)
      .escapedToolTip(escapeHtmlWithNewLines(tooltip))
      .group(group)
      .create()
  }

  private def escapeHtmlWithNewLines(unescapedTooltip: String): String = {
    val escaped0 = XmlStringUtil.escapeString(unescapedTooltip)
    val escaped1 = escaped0.replace("\n", "<br>")
    val escaped2 = XmlStringUtil.wrapInHtml(escaped1)
    escaped2
  }

  private def calculateRangeToHighlight(startOffset: Int,
                                        to: Pos,
                                        message: String,
                                        editor: Editor): Option[TextRange] =
    convertToOffset(to, message, editor)
      .filter(_ != startOffset)
      .map { endOffset => TextRange.create(startOffset, endOffset) }
      .orElse(guessRangeToHighlight(editor, startOffset))

  private def guessRangeToHighlight(editor: Editor, startOffset: Int): Option[TextRange] =
    for {
      virtualFile <- editor.getDocument.virtualFile
      psiFile <- Option(PsiManager.getInstance(editor.getProject).findFile(virtualFile))
      element <- elementToHighlight(psiFile, startOffset)
    } yield element.getTextRange

  private def elementToHighlight(file: PsiFile, offset: Int): Option[PsiElement] =
    Option(file.findElementAt(offset)).flatMap {
      case whiteSpace: PsiWhiteSpace =>
        whiteSpace.prevElementNotWhitespace
      case javaToken: PsiJavaToken if javaToken.getTokenType == JavaTokenType.DOT =>
        javaToken.nextElementNotWhitespace
      case other =>
        Some(other)
    }

  private def convertToOffset(pos: Pos,
                              message: String,
                              editor: Editor): Option[Int] = pos match {
    case Pos.LineColumn(l, c) =>
      val line = l - 1
      val column = (c - 1).max(0)
      if (line < 0) {
        None
      } else {
        val lineTextFromMessage = lineText(message)
        val document = editor.getDocument
        // TODO: dotc and scalac report different lines in their messages :(
        val actualLine =
          Seq(line, line - 1, line + 1)
            .find { lineNumber =>
              documentLine(document, lineNumber).contains(lineTextFromMessage)
            }
        actualLine.map(line => document.getLineStartOffset(line) + column)
      }
    case Pos.Offset(offset) =>
      Some(offset)
  }

  private def lineText(messageText: String): String = {
    val trimmed = messageText.trim
    val lastLineSeparator = trimmed.lastIndexOf('\n')
    if (lastLineSeparator > 0) trimmed.substring(lastLineSeparator).trim else ""
  }

  private def documentLine(document: Document, line: Int): Option[String] =
    if (line >= 0 && line < document.getLineCount) {
      val lineStart = document.getLineStartOffset(line)
      val lineEnd = document.getLineEndOffset(line)
      Some(document.getText(TextRange.create(lineStart, lineEnd)).trim)
    } else {
      None
    }
}
