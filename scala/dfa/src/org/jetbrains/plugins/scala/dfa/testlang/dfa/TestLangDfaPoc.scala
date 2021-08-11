package org.jetbrains.plugins.scala.dfa.testlang.dfa

import com.intellij.codeInspection.{InspectionManager, ProblemsHolder}
import com.intellij.codeInspection.dataFlow.interpreter.{RunnerResult, StandardDataFlowInterpreter}
import com.intellij.codeInspection.dataFlow.jvm.JvmDfaMemoryStateImpl
import com.intellij.codeInspection.dataFlow.lang.ir.DfaInstructionState
import com.intellij.codeInspection.dataFlow.value.DfaValueFactory
import org.jetbrains.plugins.scala.dfa.testlang.Ast.NumberLit

import scala.jdk.CollectionConverters._
