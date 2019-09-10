package org.jetbrains.plugins.hocon
package semantics

sealed abstract class ConfigValue
case class StringValue(value: String) extends ConfigValue
case class NumberValue(value: Number) extends ConfigValue
case class BooleanValue(value: Boolean) extends ConfigValue
case object NullValue extends ConfigValue
case object ArrayValue extends ConfigValue
case object ObjectValue extends ConfigValue
