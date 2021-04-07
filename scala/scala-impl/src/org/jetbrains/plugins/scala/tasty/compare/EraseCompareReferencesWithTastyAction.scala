package org.jetbrains.plugins.scala.tasty.compare

import com.intellij.openapi.actionSystem.{AnAction, AnActionEvent, CommonDataKeys}
import org.jetbrains.plugins.scala.NlsString
import org.jetbrains.plugins.scala.extensions.ToNullSafe
import org.jetbrains.plugins.scala.externalHighlighting.{ExternalHighlightingGroups, ExternalHighlightingUtil}

class EraseCompareReferencesWithTastyAction
  extends AnAction(NlsString.force("Erase Compare References with TASTy"), null, null) {

  override def actionPerformed(e: AnActionEvent): Unit =
    CommonDataKeys.PROJECT.getData(e.getDataContext).nullSafe.foreach { project =>
      ExternalHighlightingUtil.eraseAllHighlightings(project, ExternalHighlightingGroups.CompareReferencesWithTasty)
      showNotification("Erased")
    }
}
