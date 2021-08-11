package org.jetbrains.plugins.scala.dfa.testlang.dfa

import com.intellij.codeInspection.dataFlow.lang.ir.{ControlFlow, PopInstruction, PushValueInstruction}
import com.intellij.codeInspection.dataFlow.types.DfTypes
import com.intellij.codeInspection.dataFlow.value.DfaValueFactory
import org.jetbrains.plugins.scala.dfa.testlang.Ast.{Expression, NumberLit}

class TestLangControlFlowBuilder(val factory: DfaValueFactory, val context: Expression) {

  private val flow = new ControlFlow(factory, DummyAnchor)

  def buildFlow(): ControlFlow = {
    processExpression(context)
    flow.addInstruction(new PopInstruction()) // return value
    flow.finish()
    flow
  }

  private def processExpression(expression: Expression): Unit = {
    expression match {
      case NumberLit(int) => processIntLiteral(expression, int)
      case _ => throw new IllegalStateException("Not supported")
    }
  }

  private def processIntLiteral(expression: Expression, value: Int): Unit = {
    flow.addInstruction(new PushValueInstruction(DfTypes.intValue(value), TestLangExpressionAnchor(expression)))
  }
}
