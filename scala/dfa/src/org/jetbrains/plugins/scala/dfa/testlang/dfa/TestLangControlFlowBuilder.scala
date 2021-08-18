package org.jetbrains.plugins.scala.dfa.testlang.dfa

import com.intellij.codeInspection.dataFlow.java.inst.{BooleanBinaryInstruction, JvmPushInstruction, NumericBinaryInstruction}
import com.intellij.codeInspection.dataFlow.lang.ir.ControlFlow.DeferredOffset
import com.intellij.codeInspection.dataFlow.lang.ir._
import com.intellij.codeInspection.dataFlow.rangeSet.LongRangeBinOp
import com.intellij.codeInspection.dataFlow.types.{DfType, DfTypes}
import com.intellij.codeInspection.dataFlow.value.{DfaValueFactory, RelationType}
import org.jetbrains.plugins.scala.dfa.testlang.Ast
import org.jetbrains.plugins.scala.dfa.testlang.Ast.Block
import org.jetbrains.plugins.scala.dfa.testlang.dfa.anchors.{TestLangExpressionAnchor, TestLangNodePsiWrapper}

class TestLangControlFlowBuilder(val factory: DfaValueFactory, val context: Ast.Node) {

  private val flow = new ControlFlow(factory, context)

  def buildFlow(): ControlFlow = {
    processAstNode(context)
    flow.addInstruction(new PopInstruction()) // return value
    flow.finish()
    flow
  }

  def setOffset(offset: DeferredOffset): Unit = {
    offset.setOffset(flow.getInstructionCount)
  }

  def processAssignment(assignmentStatement: Ast.AssignmentStmt): Unit = {
    val variableName = assignmentStatement.target match {
      case identifier: Ast.Identifier => identifier.name
      case _ => throw new IllegalStateException(s"Illegal assignment: $assignmentStatement")
    }

    val variableDescriptor = TestLangVariableDescriptor(variableName)
    val dfaVariable = factory.getVarFactory.createVariableValue(variableDescriptor)

    processAstNode(assignmentStatement.expression)
    flow.addInstruction(new SimpleAssignmentInstruction(TestLangExpressionAnchor(assignmentStatement), dfaVariable))
  }

  def processIdentifier(identifier: Ast.Identifier): Unit = {
    val dfaVariable = factory.getVarFactory.createVariableValue(TestLangVariableDescriptor(identifier.name))
    flow.addInstruction(new JvmPushInstruction(dfaVariable, TestLangExpressionAnchor(identifier)))
  }

  private def processAstNode(node: Ast.Node): Unit = {
    node match {
      case program: Ast.Script => processBlock(program.main)
      case blockWrapper: Ast.BlockAstNodeWrapper => processBlock(blockWrapper.block)
      case expressionStatement: Ast.ExpressionStmt => processAstNode(expressionStatement.expr)
      case literal: Ast.NumberLit => processIntLiteral(literal)
      case operator: Ast.Operator => processBinaryOperator(operator)
      case ifStatement: Ast.IfStmt => processIf(ifStatement)
      case assignmentStmt: Ast.AssignmentStmt => processAssignment(assignmentStmt)
      case identifier: Ast.Identifier => processIdentifier(identifier)
      case _ => throw new IllegalStateException(s"Unsupported AST element: $node")
    }

    flow.finishElement(node)
  }

  private def processBlock(block: Block): Unit = {
    if (block.isEmpty) {
      pushUnknown()
    } else {
      for (statement <- block) {
        processAstNode(statement)
        if (statement != block.last) {
          flow.addInstruction(new PopInstruction)
        }
      }

      flow.addInstruction(new FinishElementInstruction(block))
    }
  }

  private def pushUnknown(): Unit = {
    flow.addInstruction(new PushValueInstruction(DfType.TOP))
  }

  private def processIntLiteral(expression: Ast.NumberLit): Unit = {
    flow.addInstruction(new PushValueInstruction(DfTypes.intValue(expression.value), TestLangExpressionAnchor(expression)))
  }

  private def processBinaryOperator(expression: Ast.Operator): Unit = {
    processAstNode(expression.left)
    processAstNode(expression.right)
    val expressionAnchor = TestLangExpressionAnchor(expression)

    val mathOperator = mathOperatorFromToken(expression.op)
    if (mathOperator.isDefined) {
      flow.addInstruction(new NumericBinaryInstruction(mathOperator.get, expressionAnchor))
    } else {
      relationalOperatorFromToken(expression.op)
        .map(op => flow.addInstruction(new BooleanBinaryInstruction(op, true, expressionAnchor)))
        .orElse(throw new IllegalStateException(s"Unsupported operator ${expression.op}"))
    }
  }

  private def mathOperatorFromToken(op: String): Option[LongRangeBinOp] = op match {
    case "+" => Some(LongRangeBinOp.PLUS)
    case "-" => Some(LongRangeBinOp.MINUS)
    case "*" => Some(LongRangeBinOp.MUL)
    case "/" => Some(LongRangeBinOp.DIV)
    case "%" => Some(LongRangeBinOp.MOD)
    case _ => None
  }

  private def relationalOperatorFromToken(op: String): Option[RelationType] = op match {
    case "<" => Some(RelationType.LT)
    case "<=" => Some(RelationType.LE)
    case ">" => Some(RelationType.GT)
    case ">=" => Some(RelationType.GE)
    case _ => None
  }

  private def processIf(ifStatement: Ast.IfStmt): Unit = {
    val condition = ifStatement.condition
    processAstNode(condition)

    val skipThenOffset = new DeferredOffset
    val thenStatement = ifStatement.success
    val elseStatement = ifStatement.fail

    flow.addInstruction(new ConditionalGotoInstruction(skipThenOffset, DfTypes.FALSE, condition))
    flow.addInstruction(new FinishElementInstruction(null))
    processAstNode(thenStatement)

    val skipElseOffset = new DeferredOffset
    flow.addInstruction(new GotoInstruction(skipElseOffset))
    setOffset(skipThenOffset)
    flow.addInstruction(new FinishElementInstruction(null))

    for (elseBlock <- elseStatement) {
      processAstNode(elseBlock)
    }

    setOffset(skipElseOffset)
    flow.addInstruction(new FinishElementInstruction(ifStatement))
  }
}
