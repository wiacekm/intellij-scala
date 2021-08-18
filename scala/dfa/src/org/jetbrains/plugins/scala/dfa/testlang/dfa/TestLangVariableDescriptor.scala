package org.jetbrains.plugins.scala.dfa.testlang.dfa

import com.intellij.codeInspection.dataFlow.types.DfType
import com.intellij.codeInspection.dataFlow.value.{DfaVariableValue, VariableDescriptor}

case class TestLangVariableDescriptor(name: String) extends VariableDescriptor {

  override val isStable: Boolean = false

  override def getDfType(qualifier: DfaVariableValue): DfType = DfType.TOP
}
