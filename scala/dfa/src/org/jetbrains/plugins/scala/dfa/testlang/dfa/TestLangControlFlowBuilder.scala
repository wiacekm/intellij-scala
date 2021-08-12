package org.jetbrains.plugins.scala.dfa.testlang.dfa

import com.intellij.codeInspection.dataFlow.java.inst.NumericBinaryInstruction
import com.intellij.codeInspection.dataFlow.lang.ir.ControlFlow.DeferredOffset
import com.intellij.codeInspection.dataFlow.lang.ir.{ControlFlow, PopInstruction, PushValueInstruction}
import com.intellij.codeInspection.dataFlow.rangeSet.LongRangeBinOp
import com.intellij.codeInspection.dataFlow.types.DfTypes
import com.intellij.codeInspection.dataFlow.value.DfaValueFactory
import org.jetbrains.plugins.scala.dfa.testlang.Ast
import org.jetbrains.plugins.scala.dfa.testlang.dfa.anchors.{TestLangExpressionAnchor, TestLangNodePsiWrapper}

import scala.annotation.tailrec

class TestLangControlFlowBuilder(val factory: DfaValueFactory, val context: Ast.Node) {

  private val flow = new ControlFlow(factory, context)

  def buildFlow(): ControlFlow = {
    processAstNode(context)
    flow.addInstruction(new PopInstruction()) // return value
    flow.finish()
    flow
  }

  def processProgram(program: Ast.Script): Unit = program.main.foreach(processAstNode)

  private def processAstNode(node: Ast.Node): Unit = {
    node match {
      case program: Ast.Script => processProgram(program)
      case expressionStatement: Ast.ExpressionStmt => processAstNode(expressionStatement.expr)
      case literal: Ast.NumberLit => processIntLiteral(literal)
      case operator: Ast.Operator => processBinaryOperator(operator)
      case _ => throw new IllegalStateException("Not supported")
    }

    flow.finishElement(node)
  }

  private def processIntLiteral(expression: Ast.NumberLit): Unit = {
    flow.addInstruction(new PushValueInstruction(DfTypes.intValue(expression.value), TestLangExpressionAnchor(expression)))
  }

  private def processBinaryOperator(expression: Ast.Operator): Unit = {
    val mathOperation = mathOperationFromToken(expression.op)
    processAstNode(expression.left)
    processAstNode(expression.right)
    flow.addInstruction(new NumericBinaryInstruction(mathOperation, TestLangExpressionAnchor(expression)))
  }

  private def mathOperationFromToken(op: String): LongRangeBinOp = op match {
    case "+" => LongRangeBinOp.PLUS
    case "-" => LongRangeBinOp.MINUS
    case "*" => LongRangeBinOp.MUL
    case "/" => LongRangeBinOp.DIV
    case "%" => LongRangeBinOp.MOD
    case _ => throw new IllegalStateException(s"Illegal operator: $op")
  }
}
