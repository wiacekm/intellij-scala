package org.jetbrains.plugins.scala.dfa.testlang.dfa

import com.intellij.codeInspection.dataFlow.lang.DfaAnchor
import org.jetbrains.plugins.scala.dfa.testlang.Ast.Expression

sealed trait TestLangAnchor extends DfaAnchor
case class TestLangExpressionAnchor(val expression: Expression) extends TestLangAnchor
