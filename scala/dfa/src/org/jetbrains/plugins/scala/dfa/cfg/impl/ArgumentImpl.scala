package org.jetbrains.plugins.scala.dfa
package cfg
package impl

private final class ArgumentImpl(override val argumentName: String, override val abstractValue: DfAny) extends ValueImpl with Argument { this: Value =>
  override def argumentIndex: Int = valueId

  override protected def asmString: String = s"$valueIdString <- $argumentName [argument $argumentIndex]"
}
