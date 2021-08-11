package org.jetbrains.plugins.scala.dfa.testlang.dfa

import com.intellij.codeInspection.dataFlow.lang.{DfaAnchor, DfaListener, UnsatisfiedConditionProblem}
import com.intellij.codeInspection.dataFlow.memory.DfaMemoryState
import com.intellij.codeInspection.dataFlow.types.DfTypes
import com.intellij.codeInspection.dataFlow.value.DfaValue
import com.intellij.util.ThreeState

import scala.collection.mutable
import scala.jdk.CollectionConverters._

class TestLangDfaListener extends DfaListener {

  val constantConditions = mutable.Map.empty[TestLangAnchor, ConstantValue]
  val problems = mutable.Map.empty[TestLangProblem, ThreeState]

  override def beforePush(args: Array[DfaValue], value: DfaValue, anchor: DfaAnchor, state: DfaMemoryState): Unit = {
    anchor match {
      case testLangAnchor: TestLangAnchor => recordExpressionValue(testLangAnchor, state, value)
    }
  }

  private def recordExpressionValue(anchor: TestLangAnchor, state: DfaMemoryState, value: DfaValue): Unit = {
    var newValue = state.getDfType(value) match {
      case DfTypes.TRUE => ConstantValue.True
      case DfTypes.FALSE => ConstantValue.False
      case DfTypes.NULL => ConstantValue.Null
      case x if x == DfTypes.intValue(0) || x == DfTypes.longValue(0) => ConstantValue.Zero
      case _ => ConstantValue.Unknown
    }

    constantConditions.get(anchor) match {
      case Some(oldValue) =>
        if (oldValue == ConstantValue.Unknown) return
        if (oldValue != newValue) {
          newValue = ConstantValue.Unknown
        }
      case None => ()
    }

    constantConditions(anchor) = newValue
  }

  override def onCondition(problem: UnsatisfiedConditionProblem, value: DfaValue, failed: ThreeState, state: DfaMemoryState): Unit = {
    problem match {
      case testLangProblem: TestLangProblem => problems.updateWith(testLangProblem) {
        case Some(previousValue) => Some(ThreeState.merge(Iterable(previousValue, failed).asJava))
        case None => Some(failed)
      }
    }
  }
}
