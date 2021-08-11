package org.jetbrains.plugins.scala.dfa.testlang.dfa

import com.intellij.codeInspection.dataFlow.lang.UnsatisfiedConditionProblem

sealed case class TestLangProblem() extends UnsatisfiedConditionProblem
