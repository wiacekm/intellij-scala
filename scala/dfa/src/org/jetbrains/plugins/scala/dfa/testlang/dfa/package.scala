package org.jetbrains.plugins.scala.dfa.testlang

package object dfa {

  sealed trait ConstantValue
  object ConstantValue {
    case object True extends ConstantValue
    case object False extends ConstantValue
    case object Null extends ConstantValue
    case object Zero extends ConstantValue
    case object Unknown extends ConstantValue
  }
}
