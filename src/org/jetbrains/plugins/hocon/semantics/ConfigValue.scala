package org.jetbrains.plugins.hocon
package semantics

import org.jetbrains.plugins.hocon.psi.{HNumber, HStringValue}

sealed abstract class ConfigValue {
  def concat(other: ConfigValue): ConfigValue = (this, other) match {
    case (InvalidValue, _) | (_, InvalidValue) => InvalidValue
    case (_, NoValue) => this
    case (NoValue, _) => other
    case (SimpleValue(ltext, concatWsLeft), SimpleValue(rtext, concatWsRight)) =>
      StringValue(ltext + rtext, concatWsLeft && concatWsRight)
    case (ArrayValue, ArrayValue) => ArrayValue
    case (ObjectValue, ObjectValue) => ObjectValue
    case (ArrayValue | ObjectValue, StringValue(_, true)) => this
    case (StringValue(_, true), ArrayValue | ObjectValue) => other
    case _ => InvalidValue
  }

  def typeHint: String = this match {
    case NullValue => "null"
    case _: BooleanValue => "boolean"
    case _: NumberValue => "number"
    case _: StringValue => "string"
    case ArrayValue => "array"
    case ObjectValue => "object"
    case _ => ""
  }

  def valueHint: String = this match {
    case SimpleValue(value, _) => s" = ${HStringValue.quoteIfNecessary(value)}"
    case ArrayValue => " = [...]"
    case ObjectValue => " = {...}"
    case _ => ""
  }
}
sealed abstract class SimpleValue(val text: String) extends ConfigValue
object SimpleValue {
  def unapply(sv: SimpleValue): Some[(String, Boolean)] =
    Some((sv.text, sv match {
      case StringValue(_, concatWhitespace) => concatWhitespace
      case _ => false
    }))
}
final case class StringValue(value: String, concatWhitespace: Boolean = false) extends SimpleValue(value) {
  def quotedIfNecessary: String = HStringValue.quoteIfNecessary(value)
}
// keeping numeric value as String to preserve exact representation
final case class NumberValue(value: String) extends SimpleValue(value) {
  def number: Number = HNumber.parse(value)
}
final case class BooleanValue(value: Boolean) extends SimpleValue(value.toString)
case object NullValue extends SimpleValue("null")
case object ArrayValue extends ConfigValue
case object ObjectValue extends ConfigValue
case object NoValue extends ConfigValue
case object InvalidValue extends ConfigValue
