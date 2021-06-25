package org.jetbrains.plugins.scala.lang.psi.impl.statements

import com.intellij.lang.ASTNode
import org.jetbrains.plugins.scala.JavaArrayFactoryUtil.ScFunctionDefinitionFactory
import org.jetbrains.plugins.scala.extensions.StubBasedExt
import org.jetbrains.plugins.scala.lang.parser.ScalaElementType.{EXTENSION_DEFINITIONS, FUNCTION_DEFINITION}
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScExtensionDefinitions, ScFunctionDefinition}
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaStubBasedElementImpl
import org.jetbrains.plugins.scala.lang.psi.stubs.ScExtensionDefinitionsStub

final class ScExtensionDefinitionsImpl private (stub: ScExtensionDefinitionsStub, node: ASTNode)
    extends ScalaStubBasedElementImpl(stub, EXTENSION_DEFINITIONS, node)
    with ScExtensionDefinitions {

  def this(node: ASTNode) = this(null, node)

  def this(stub: ScExtensionDefinitionsStub) = this(stub, null)

  override def toString: String = "ScExtensionDefinitions"

  override def functions: Seq[ScFunctionDefinition] =
    this.stubOrPsiChildren(FUNCTION_DEFINITION, ScFunctionDefinitionFactory).toSeq
}
