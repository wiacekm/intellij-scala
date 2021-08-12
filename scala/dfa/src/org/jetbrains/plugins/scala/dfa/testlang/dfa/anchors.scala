package org.jetbrains.plugins.scala.dfa.testlang.dfa

import com.intellij.codeInspection.dataFlow.lang.DfaAnchor
import com.intellij.lang.{ASTNode, Language}
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.{Key, TextRange}
import com.intellij.psi.scope.PsiScopeProcessor
import com.intellij.psi.search.{GlobalSearchScope, SearchScope}
import com.intellij.psi._
import org.jetbrains.plugins.scala.dfa.testlang.Ast
import org.jetbrains.plugins.scala.dfa.testlang.Ast.Expression

import javax.swing.Icon

object anchors {

  sealed trait TestLangAnchor extends DfaAnchor
  case class TestLangExpressionAnchor(expression: Expression) extends TestLangAnchor

  implicit class TestLangNodePsiWrapper(node: Ast.Node) extends PsiElement {

    override def getResolveScope: GlobalSearchScope = ???

    override def putCopyableUserData[T](key: Key[T], value: T): Unit = ???

    override def isPhysical: Boolean = ???

    override def getContext: PsiElement = ???

    override def getUseScope: SearchScope = ???

    override def isEquivalentTo(another: PsiElement): Boolean = ???

    override def processDeclarations(processor: PsiScopeProcessor, state: ResolveState, lastParent: PsiElement, place: PsiElement): Boolean = ???

    override def getNode: ASTNode = ???

    override def getProject: Project = ???

    override def getLanguage: Language = ???

    override def getManager: PsiManager = ???

    override def getChildren: Array[PsiElement] = ???

    override def getParent: PsiElement = ???

    override def getFirstChild: PsiElement = ???

    override def getLastChild: PsiElement = ???

    override def getNextSibling: PsiElement = ???

    override def getPrevSibling: PsiElement = ???

    override def getContainingFile: PsiFile = ???

    override def getTextRange: TextRange = ???

    override def getStartOffsetInParent: Int = ???

    override def getTextLength: Int = ???

    override def findElementAt(offset: Int): PsiElement = ???

    override def findReferenceAt(offset: Int): PsiReference = ???

    override def getTextOffset: Int = ???

    override def getText: String = ???

    override def textToCharArray(): Array[Char] = ???

    override def getNavigationElement: PsiElement = ???

    override def getOriginalElement: PsiElement = ???

    override def textMatches(text: CharSequence): Boolean = ???

    override def textMatches(element: PsiElement): Boolean = ???

    override def textContains(c: Char): Boolean = ???

    override def accept(visitor: PsiElementVisitor): Unit = ???

    override def acceptChildren(visitor: PsiElementVisitor): Unit = ???

    override def copy(): PsiElement = ???

    override def add(element: PsiElement): PsiElement = ???

    override def addBefore(element: PsiElement, anchor: PsiElement): PsiElement = ???

    override def addAfter(element: PsiElement, anchor: PsiElement): PsiElement = ???

    override def checkAdd(element: PsiElement): Unit = ???

    override def addRange(first: PsiElement, last: PsiElement): PsiElement = ???

    override def addRangeBefore(first: PsiElement, last: PsiElement, anchor: PsiElement): PsiElement = ???

    override def addRangeAfter(first: PsiElement, last: PsiElement, anchor: PsiElement): PsiElement = ???

    override def delete(): Unit = ???

    override def checkDelete(): Unit = ???

    override def deleteChildRange(first: PsiElement, last: PsiElement): Unit = ???

    override def replace(newElement: PsiElement): PsiElement = ???

    override def isValid: Boolean = ???

    override def isWritable: Boolean = ???

    override def getReference: PsiReference = ???

    override def getReferences: Array[PsiReference] = ???

    override def getCopyableUserData[T](key: Key[T]): T = ???

    override def putUserData[T](key: Key[T], value: T): Unit = ???

    override def getUserData[T](key: Key[T]): T = ???

    override def getIcon(flags: Int): Icon = ???
  }
}
