package org.jetbrains.plugins.scala.lang.psi.api.statements

import org.jetbrains.plugins.scala.lang.psi.api.ScalaPsiElement

trait ScExtensionDefinitions extends ScalaPsiElement {
  def functions: Seq[ScFunctionDefinition]
}
