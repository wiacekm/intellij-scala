package org.jetbrains.plugins.scala.tasty.compare

import com.intellij.codeInsight.daemon.impl.HighlightInfoType
import com.intellij.openapi.actionSystem.{AnAction, AnActionEvent, CommonDataKeys}
import com.intellij.openapi.editor.Editor
import com.intellij.psi.util.PsiUtilBase
import com.intellij.psi.{PsiElement, PsiFile}
import org.jetbrains.plugins.scala.NlsString
import org.jetbrains.plugins.scala.actions.ScalaActionUtil
import org.jetbrains.plugins.scala.annotator.gutter.ScalaGoToDeclarationHandler
import org.jetbrains.plugins.scala.extensions.{PsiElementExt, ToNullSafe}
import org.jetbrains.plugins.scala.externalHighlighting.ExternalHighlighting.Pos
import org.jetbrains.plugins.scala.externalHighlighting.{ExternalHighlighting, ExternalHighlightingGroups, ExternalHighlightingUtil}
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.tasty.{isTastyEnabledFor, referenceTargetsAt}

class CompareReferencesWithTastyAction
  extends AnAction(NlsString.force("Compare References with TASTy"), null, null) {

  import CompareReferencesWithTastyAction.CompareReferenceResult

  override def update(e: AnActionEvent): Unit = extractFileAndEditor(e) match {
    case Some((file, _)) if isTastyEnabledFor(file) => ScalaActionUtil.enablePresentation(e)
    case _ => ScalaActionUtil.disablePresentation(e)
  }

  override def actionPerformed(e: AnActionEvent): Unit =
    extractFileAndEditor(e).foreach { case (file, editor) =>
      doAction(file, editor)
    }

  private def extractFileAndEditor(e: AnActionEvent): Option[(PsiFile, Editor)] = {
    val context = e.getDataContext
    val result = for {
      editor <- CommonDataKeys.EDITOR.getData(context).nullSafe
      project <- CommonDataKeys.PROJECT.getData(context).nullSafe
      file <- PsiUtilBase.getPsiFileInEditor(editor, project).nullSafe
    } yield (file, editor)
    result.toOption
  }

  private def doAction(file: PsiFile, editor: Editor): Unit = {
    val sourceElements = file.elements.filter(_.getNode.getElementType == ScalaTokenTypes.tIDENTIFIER)
    val highlightings = sourceElements.flatMap { sourceElement =>
      val results = compareReferences(file, sourceElement)
      if (results.nonEmpty) {
        val msg = resultsAsString(results)
        val textRange = sourceElement.getTextRange
        Some(ExternalHighlighting(
          highlightType = HighlightInfoType.DUPLICATE_FROM_SERVER,
          message = msg,
          from = Pos.Offset(textRange.getStartOffset),
          to = Pos.Offset(textRange.getEndOffset)
        ))
      } else {
        None
      }
    }

    val message = if (highlightings.isEmpty)
      "No difference"
    else
      "Difference found"
    showNotification(message)
    
    ExternalHighlightingUtil.setHighlightings(
      editor,
      highlightings.toSet,
      ExternalHighlightingGroups.CompareReferencesWithTasty,
      onlyFirstLineInToolTip = false
    )
  }

  private def compareReferences(file: PsiFile, sourceElement: PsiElement): Seq[CompareReferenceResult] = {
    val tastyTargetOffsets = getTargetOffsetsByTasty(sourceElement, sourceElement.getTextOffset)
    if (tastyTargetOffsets.nonEmpty) {
      val targetOffsets = getTargetOffsets(file, sourceElement)
      tastyTargetOffsets.flatMap { case (path, tastyOffset) =>
        val offset = targetOffsets.get(path)
        if (!offset.contains(tastyOffset))
          Some(CompareReferenceResult(path, tastyOffset, offset))
        else
          None
      }.toSeq
    } else {
      Seq.empty
    }
  }

  private def getTargetOffsets(file: PsiFile, sourceElement: PsiElement): Map[String, Int] = {
    val targetElements = ScalaGoToDeclarationHandler.findTargetElements(file, sourceElement).nullSafe
      .map(_.toSeq)
      .getOrElse(Seq.empty)
    targetElements.map { targetElement =>
      val path = targetElement.getContainingFile.getVirtualFile.getCanonicalPath
      path -> targetElement.getTextOffset
    }.toMap
  }

  private def getTargetOffsetsByTasty(sourceElement: PsiElement, sourceOffset: Int): Map[String, Int] =
    referenceTargetsAt(sourceElement, sourceOffset, useOriginalTastyOnly = true).toMap

  private def resultsAsString(results: Seq[CompareReferenceResult]): String =
    results.map { case CompareReferenceResult(path, tastyOffset, offset) =>
      s"""$path
         |Resolve: ${offset.getOrElse("?")}
         |TASTy:   $tastyOffset
         |""".stripMargin
    }.mkString("\n")
}

object CompareReferencesWithTastyAction {

  private case class CompareReferenceResult(path: String, tastyOffset: Int, offset: Option[Int])
}