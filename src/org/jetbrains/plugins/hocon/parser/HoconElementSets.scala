package org.jetbrains.plugins.hocon
package parser

import com.intellij.psi.TokenType

object HoconElementSets {

  import org.jetbrains.plugins.hocon.parser.HoconElementType._

  final val KeyedField = PrefixedField | ValuedField
  final val Literal = Null | Boolean | Number | StringValue
  final val Value = Literal | Object | Array | Substitution | Concatenation
  final val ForcedLeafBlock = Key | Path | UnquotedString | Number | Null | Boolean | TokenType.ERROR_ELEMENT
}
