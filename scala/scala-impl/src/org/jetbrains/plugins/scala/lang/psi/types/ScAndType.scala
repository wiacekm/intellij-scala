package org.jetbrains.plugins.scala.lang.psi.types

import org.jetbrains.plugins.scala.extensions.ObjectExt
import org.jetbrains.plugins.scala.lang.psi.types.api.{ParameterizedType, TypeParameter}
import org.jetbrains.plugins.scala.project.ProjectContext

/**
 * Scala 3 intersection type, e.g. `Foo & Bar`
 */
final case class ScAndType private(lhs: ScType, rhs: ScType) extends ScCompoundOrAndType {
  override def visitType(visitor: ScalaTypeVisitor): Unit = visitor.visitAndType(this)

  override implicit def projectContext: ProjectContext = lhs.projectContext

  override def components: Seq[ScType] = Seq(lhs, rhs)
}

object ScAndType {
  def apply(lhs: ScType, rhs: ScType): ScType = {
    if (lhs == rhs || rhs.isAny) lhs
    else if (lhs.isAny)          rhs
    else                         makeAndType(lhs, rhs)
  }

  private[this] def checkEquiv(lhs: ScType, rhs: ScType): Boolean =
    lhs.equiv(rhs, ConstraintSystem.empty, falseUndef = false).isRight

  private[this] def makeAndType(lhs: ScType, rhs: ScType): ScType = (lhs, rhs) match {
    case (ParameterizedType(des1, args1), ParameterizedType(des2, args2))
      if checkEquiv(des1, des2) =>
      val jointArgs = extractTypeParameters(des1).flatMap(glbArgs(args1, args2, _))
      jointArgs.fold()
      ???
    case _ => new ScAndType(lhs, rhs)
  }

  private[this] def glbArgs(args1: Seq[ScType], args2: Seq[ScType], typeParams: Seq[TypeParameter]): Option[Seq[ScType]] = {
    val zippedArgs = args1.lazyZip(args2).lazyZip(typeParams).iterator
    val jointArgs = Seq.newBuilder[ScType]

    while (zippedArgs.hasNext) {
      val (arg1, arg2, typeParam) = zippedArgs.next()

      if (checkEquiv(arg1, arg2))         jointArgs += arg1
      else if (typeParam.isCovariant)     arg1.glb(arg2)
      else if (typeParam.isContravariant) arg1.lub(arg2)
      else                                return None
    }

    jointArgs.result().toOption
  }
}
