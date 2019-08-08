package org.jetbrains.plugins.hocon
package lexer

import com.intellij.psi.TokenType
import com.intellij.psi.tree.IElementType
import org.jetbrains.plugins.hocon.lang.HoconLanguage

sealed class HoconTokenType(debugString: String) extends IElementType(debugString, HoconLanguage)

object HoconTokenType extends TokenType {
  final val InlineWhitespace = new HoconTokenType("INLINE_WHITESPACE")
  final val LineBreakingWhitespace = new HoconTokenType("LINE_BREAKING_WHITESPACE")
  final val BadCharacter = new HoconTokenType("BAD_CHARACTER")
  final val LBrace = new HoconTokenType("LBRACE")
  final val RBrace = new HoconTokenType("RBRACE")
  final val LBracket = new HoconTokenType("LBRACKET")
  final val RBracket = new HoconTokenType("RBRACKET")
  final val LParen = new HoconTokenType("LPAREN")
  final val RParen = new HoconTokenType("RPAREN")
  final val Colon = new HoconTokenType("COLON")
  final val Comma = new HoconTokenType("COMMA")
  final val Equals = new HoconTokenType("EQUALS")
  final val PlusEquals = new HoconTokenType("PLUS_EQUALS")
  final val Period = new HoconTokenType("PERIOD")
  final val Dollar = new HoconTokenType("DOLLAR")
  final val SubLBrace = new HoconTokenType("SUB_LBRACE")
  final val QMark = new HoconTokenType("QMARK")
  final val SubRBrace = new HoconTokenType("SUB_RBRACE")
  final val HashComment = new HoconTokenType("HASH_COMMENT")
  final val DoubleSlashComment = new HoconTokenType("DOUBLE_SLASH_COMMENT")
  final val UnquotedChars = new HoconTokenType("UNQUOTED_CHARS")
  final val QuotedString = new HoconTokenType("QUOTED_STRING")
  final val MultilineString = new HoconTokenType("MULTILINE_STRING")
}
