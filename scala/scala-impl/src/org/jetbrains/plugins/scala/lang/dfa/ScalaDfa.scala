package org.jetbrains.plugins.scala.lang.dfa

import com.intellij.codeInsight.AnnotationUtil
import com.intellij.psi.{PsiElement, PsiJvmModifiersOwner, PsiMethod, PsiParameter}
import org.jetbrains.plugins.scala.dfa.{DfAny, DfAnyRef, DfAnyVal, DfBool, DfInt, DfNothing, DfNull, DfUnit, Nullability}
import org.jetbrains.plugins.scala.dfa.analysis.DataFlowAnalysis.SpecialMethodProcessorFactories
import org.jetbrains.plugins.scala.dfa.analysis.{DataFlowAnalysis, DfaResult}
import org.jetbrains.plugins.scala.dfa.cfg.CallInfo
import org.jetbrains.plugins.scala.extensions.PsiTypeExt
import org.jetbrains.plugins.scala.lang.psi.api.ScalaPsiElement
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScParameter
import org.jetbrains.plugins.scala.lang.psi.cfg.PsiGraph
import org.jetbrains.plugins.scala.lang.psi.types.ScType
import org.jetbrains.plugins.scala.lang.psi.types.api.{StdType, StdTypes}

object ScalaDfa {
  val specialMethodProcessorFactories: SpecialMethodProcessorFactories = Map(
    CallInfo("equals", isStatic = false, abstractReturnValue = DfBool.Top) -> specials.Defaults.equalsImpl
  )

  def apply(graph: PsiGraph): DataFlowAnalysis[PsiElement] =
    new DataFlowAnalysis(graph, specialMethodProcessorFactories)

  def computeResult(graph: PsiGraph): DfaResult[PsiElement] = {
    val dfa = ScalaDfa(graph)
    dfa.run()
    dfa.result
  }

  def nullability(psi: PsiJvmModifiersOwner): Nullability = {
    import AnnotationUtil._
    val flags = CHECK_EXTERNAL | CHECK_HIERARCHY | CHECK_INFERRED | CHECK_TYPE
    nullability(
      notNull =  isAnnotated(psi, NON_NLS, flags),
      nullable = isAnnotated(psi, NULLABLE, flags),
      isScala =  psi.isInstanceOf[ScalaPsiElement]
    )
  }

  def nullability(notNull: Boolean, nullable: Boolean, isScala: Boolean = true): Nullability =
    if (notNull) Nullability.NeverNull
    else if (nullable) Nullability.MaybeNull
    else Nullability.MaybeNullButNotExpected

  def typeToValue(ty: ScType, nullability: Nullability = Nullability.MaybeNull): DfAny = {
    import StdType.{Name => StdName}

    val nully: DfNull = nullability match {
      case Nullability.MaybeNull => DfNull.Top
      case Nullability.NeverNull => DfNull.Bottom
      case Nullability.MaybeNullButNotExpected => DfNull.Unexpected
      case Nullability.AlwaysNull =>
        // doesn't make much sense, but if nullability says that it is always null, so be it!
        return DfNull.Top
    }

    ty match {
      case stdTy: StdType =>
        stdTy.name match {
          case StdName.Int => DfInt.Top
          case StdName.Null => DfNull.Top
          case StdName.Nothing => DfNothing
          case StdName.Unit => DfUnit.Top
          case StdName.Boolean => DfUnit.Top
          case StdName.Any => DfAny.Top
          case StdName.AnyRef => DfAnyRef.Top | nully
          case StdName.AnyVal => DfAnyVal.Top
          case _ => DfAny.withoutNull
        }
      case _ =>
        DfAnyRef.Top | nully
    }
  }

  def returnTypeToValue(method: PsiMethod): DfAny =
    typeToValue(method.getReturnType.toScType()(method), ScalaDfa.nullability(method))

  def paramTypeToValue(param: ScParameter): DfAny =
    typeToValue(param.`type`().getOrElse(StdTypes.instance(param).AnyRef), ScalaDfa.nullability(param))

  def paramTypeToValue(param: PsiParameter): DfAny =
    typeToValue(param.getType.toScType()(param), ScalaDfa.nullability(param))
}
