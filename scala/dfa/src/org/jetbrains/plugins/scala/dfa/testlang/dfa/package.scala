package org.jetbrains.plugins.scala.dfa.testlang

package object dfa {

  sealed trait ConstantValue
  case class BooleanValue(value: Boolean) extends ConstantValue
  case class IntegerValue(value: Long) extends ConstantValue
  case object NullValue extends ConstantValue
  case object UnknownValue extends ConstantValue
}
