package org.jetbrains.plugins.scala.compilerBasedHighlighting

import com.intellij.codeInsight.daemon.impl.HighlightInfoType
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.Project
import com.intellij.problems.WolfTheProblemSolver
import com.intellij.psi.PsiManager
import org.jetbrains.plugins.scala.editor.DocumentExt
import org.jetbrains.plugins.scala.settings.ProblemSolverUtils
import org.jetbrains.plugins.scala.extensions.inReadAction
import org.jetbrains.plugins.scala.externalHighlighting.{ExternalHighlightingGroups, ExternalHighlightingUtil}

object CompilerBasedHighlightingUtil {
  
  private final val Group = ExternalHighlightingGroups.CompilerBasedHighlighting

  def applyHighlighting(project: Project,
                        editor: Editor,
                        state: HighlightingState): Unit =
    if (ScalaHighlightingMode.isShowErrorsFromCompilerEnabled(project))
      for {
        virtualFile <- editor.getDocument.virtualFile
        psiFile <- Option(inReadAction(PsiManager.getInstance(project).findFile(virtualFile)))
      } if (ScalaHighlightingMode.isShowErrorsFromCompilerEnabled(psiFile)) {
        val externalHighlights = state.externalHighlightings(virtualFile)
        ExternalHighlightingUtil.setHighlightings(editor, externalHighlights, Group, onlyFirstLineInToolTip = true)
      }
  
  def eraseAllHighlightings(project: Project): Unit = {
    ExternalHighlightingUtil.eraseAllHighlightings(project, Group)
    ProblemSolverUtils.clearAllProblemsFromExternalSource(project, this)
  }

  def informWolf(project: Project, state: HighlightingState): Unit =
    if (ScalaHighlightingMode.isShowErrorsFromCompilerEnabled(project)) {
      val errorTypes = Set(HighlightInfoType.ERROR, HighlightInfoType.WRONG_REF)
      ProblemSolverUtils.clearAllProblemsFromExternalSource(project, this)
      val wolf = WolfTheProblemSolver.getInstance(project)
      val errorFiles = state.filesWithHighlightings(errorTypes)
      errorFiles.foreach(wolf.reportProblemsFromExternalSource(_, this))
    }
}
