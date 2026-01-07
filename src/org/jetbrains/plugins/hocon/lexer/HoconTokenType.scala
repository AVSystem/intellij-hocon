package org.jetbrains.plugins.hocon
package lexer

import lang.HoconLanguage

import com.intellij.psi.tree.IElementType

enum HoconTokenType(debugString: String) extends IElementType(debugString, HoconLanguage) {
  case InlineWhitespace extends HoconTokenType("INLINE_WHITESPACE")
  case LineBreakingWhitespace extends HoconTokenType("LINE_BREAKING_WHITESPACE")
  case BadCharacter extends HoconTokenType("BAD_CHARACTER")
  case LBrace extends HoconTokenType("LBRACE")
  case RBrace extends HoconTokenType("RBRACE")
  case LBracket extends HoconTokenType("LBRACKET")
  case RBracket extends HoconTokenType("RBRACKET")
  case LParen extends HoconTokenType("LPAREN")
  case RParen extends HoconTokenType("RPAREN")
  case Colon extends HoconTokenType("COLON")
  case Comma extends HoconTokenType("COMMA")
  case Equals extends HoconTokenType("EQUALS")
  case PlusEquals extends HoconTokenType("PLUS_EQUALS")
  case Period extends HoconTokenType("PERIOD")
  case Dollar extends HoconTokenType("DOLLAR")
  case SubLBrace extends HoconTokenType("SUB_LBRACE")
  case QMark extends HoconTokenType("QMARK")
  case SubRBrace extends HoconTokenType("SUB_RBRACE")
  case HashComment extends HoconTokenType("HASH_COMMENT")
  case DoubleSlashComment extends HoconTokenType("DOUBLE_SLASH_COMMENT")
  case UnquotedChars extends HoconTokenType("UNQUOTED_CHARS")
  case QuotedString extends HoconTokenType("QUOTED_STRING")
  case MultilineString extends HoconTokenType("MULTILINE_STRING")
}
