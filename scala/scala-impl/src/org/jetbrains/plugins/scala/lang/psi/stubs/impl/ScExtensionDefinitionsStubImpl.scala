package org.jetbrains.plugins.scala.lang.psi.stubs.impl

import com.intellij.psi.PsiElement
import com.intellij.psi.stubs.{IStubElementType, StubBase, StubElement}
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScExtensionDefinitions
import org.jetbrains.plugins.scala.lang.psi.stubs.ScExtensionDefinitionsStub

class ScExtensionDefinitionsStubImpl(
  parent:      StubElement[_ <: PsiElement],
  elementType: IStubElementType[_ <: StubElement[_ <: PsiElement], _ <: PsiElement]
) extends StubBase[ScExtensionDefinitions](parent, elementType)
    with ScExtensionDefinitionsStub
