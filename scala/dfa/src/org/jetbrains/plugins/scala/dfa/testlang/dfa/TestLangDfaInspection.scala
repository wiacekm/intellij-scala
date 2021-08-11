package org.jetbrains.plugins.scala.dfa.testlang.dfa

import com.intellij.codeInspection.{LocalInspectionTool, LocalInspectionToolSession, ProblemsHolder}
import com.intellij.psi.PsiElementVisitor

class TestLangDfaInspection extends LocalInspectionTool {

  override def buildVisitor(holder: ProblemsHolder, isOnTheFly: Boolean, session: LocalInspectionToolSession): PsiElementVisitor = super.buildVisitor(holder, isOnTheFly, session)
}
