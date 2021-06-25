package org.jetbrains.plugins.scala.lang.psi.stubs.elements

import com.intellij.lang.ASTNode
import com.intellij.psi.PsiElement
import com.intellij.psi.stubs.{StubElement, StubInputStream}
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScExtensionDefinitions
import org.jetbrains.plugins.scala.lang.psi.impl.statements.ScExtensionDefinitionsImpl
import org.jetbrains.plugins.scala.lang.psi.stubs.ScExtensionDefinitionsStub
import org.jetbrains.plugins.scala.lang.psi.stubs.impl.ScExtensionDefinitionsStubImpl

class ScExtensionDefinitionsElementType
    extends ScStubElementType[ScExtensionDefinitionsStub, ScExtensionDefinitions]("extension body") {

  override def deserialize(dataStream: StubInputStream, parentStub: StubElement[_ <: PsiElement]): ScExtensionDefinitionsStub =
    new ScExtensionDefinitionsStubImpl(parentStub, this)

  override def createStubImpl(
    extBody:    ScExtensionDefinitions,
    parentStub: StubElement[_ <: PsiElement]
  ): ScExtensionDefinitionsStub =
    new ScExtensionDefinitionsStubImpl(parentStub, this)

  override def createElement(node: ASTNode): ScExtensionDefinitions =
    new ScExtensionDefinitionsImpl(node)

  override def createPsi(stub: ScExtensionDefinitionsStub): ScExtensionDefinitions =
    new ScExtensionDefinitionsImpl(stub)
}
