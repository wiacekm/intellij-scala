package org.jetbrains.plugins.scala.annotator.element

import org.jetbrains.plugins.scala.ScalaBundle
import org.jetbrains.plugins.scala.annotator.ScalaAnnotationHolder
import org.jetbrains.plugins.scala.lang.psi.api.base.types.{ScFunctionalTypeElement, ScPolyFunctionTypeElement}
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScFunctionExpr, ScPolyFunctionExpr}

object ScPolyFunctionExprAnnotator extends ElementAnnotator[ScPolyFunctionExpr] {
  override def annotate(
    element:   ScPolyFunctionExpr,
    typeAware: Boolean
  )(implicit
    holder: ScalaAnnotationHolder
  ): Unit = element.result match {
    case None | Some(_: ScFunctionExpr) => ()
    case Some(nonFn) =>
      holder.createErrorAnnotation(
        nonFn,
        ScalaBundle.message("poly.function.without.parameters", "literals")
      )
  }
}

object ScPolyFunctionTypeElementAnnotator extends ElementAnnotator[ScPolyFunctionTypeElement] {
  override def annotate(
    element:   ScPolyFunctionTypeElement,
    typeAware: Boolean
  )(implicit
    holder: ScalaAnnotationHolder
  ): Unit = element.resultTypeElement match {
    case None | Some(_: ScFunctionalTypeElement) => ()
    case Some(nonFn) =>
      holder.createErrorAnnotation(
        nonFn,
        ScalaBundle.message("poly.function.without.parameters", "types")
      )
  }
}
