package org.jetbrains.plugins.scala.codeInspection.dfa

import com.intellij.codeInspection.{InspectionManager, LocalQuickFix, ProblemDescriptor, ProblemHighlightType}
import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.codeInspection.{AbstractRegisteredInspection, ScalaInspectionBundle}
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScReferenceExpression

class NullUseInspection extends AbstractRegisteredInspection {
  override protected def problemDescriptor(element: PsiElement,
                                           maybeQuickFix: Option[LocalQuickFix] = None,
                                           descriptionTemplate: String = getDisplayName,
                                           highlightType: ProblemHighlightType = ProblemHighlightType.GENERIC_ERROR_OR_WARNING)
                                          (implicit manager: InspectionManager, isOnTheFly: Boolean): Option[ProblemDescriptor] = {
    element match {
      case ref@ScReferenceExpression.withQualifier(qual) if qual.dfaValue.exists(_.nullability.canBeNullAndIsExpected) =>
        super.problemDescriptor(ref, maybeQuickFix, ScalaInspectionBundle.message("qualifier.might.be.null"), highlightType)
      case _ =>
        None
    }
  }

    /*expr
      .map {
        case Annotatable(ref) if isInProjectSource(ref) =>
          val quickFixes = Array[LocalQuickFix](new AnnotateWithNls(ref)) ++ maybeQuickFix
          manager.createProblemDescriptor(element, descriptionTemplate, isOnTheFly, quickFixes, highlightType)

        case _ =>
          val quickFixes = Array[LocalQuickFix]() ++ maybeQuickFix
          manager.createProblemDescriptor(element, descriptionTemplate, isOnTheFly, quickFixes, highlightType)
      }
  }*/
}
