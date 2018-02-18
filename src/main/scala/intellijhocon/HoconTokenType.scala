package intellijhocon

import com.intellij.psi.tree.IElementType
import com.intellij.psi.TokenType

sealed class HoconTokenType(debugString: String) extends IElementType(debugString, HoconLanguage)

object HoconTokenType extends TokenType {

  case object LBrace extends HoconTokenType("LBRACE")

  case object RBrace extends HoconTokenType("RBRACE")

  case object LSquare extends HoconTokenType("LSQUARE")

  case object RSquare extends HoconTokenType("RSQUARE")

  case object Colon extends HoconTokenType("COLON")

  case object Comma extends HoconTokenType("COMMA")

  case object Equals extends HoconTokenType("EQUALS")

  case object PlusEquals extends HoconTokenType("PLUS_EQUALS")

  case object NewLine extends HoconTokenType("NEWLINE")

  case object RefStart extends HoconTokenType("REF_START")

  case object RefEnd extends HoconTokenType("REF_END")

  case object Null extends HoconTokenType("NULL")

  case object True extends HoconTokenType("TRUE")

  case object False extends HoconTokenType("FALSE")

  case object Comment extends HoconTokenType("COMMENT")

  case object Number extends HoconTokenType("NUMBER")

  case object UnquotedString extends HoconTokenType("UNQUOTED_STRING")

  case object QuotedString extends HoconTokenType("QUOTED_STRING")

  case object MultilineString extends HoconTokenType("MULTILINE_STRING")

  case object WhitespaceString extends HoconTokenType("WHITESPACE_STRING")

}
