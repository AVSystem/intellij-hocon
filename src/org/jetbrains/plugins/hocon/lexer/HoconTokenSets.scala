package org.jetbrains.plugins.hocon.lexer

import com.intellij.psi.tree.TokenSet

import scala.language.implicitConversions

object HoconTokenSets {

  import org.jetbrains.plugins.hocon.CommonUtil._
  import org.jetbrains.plugins.hocon.lexer.HoconTokenType._

  final val Empty = TokenSet.EMPTY
  final val Whitespace = InlineWhitespace | LineBreakingWhitespace
  final val Comment = HashComment | DoubleSlashComment
  final val WhitespaceOrComment = Whitespace | Comment
  final val StringLiteral = QuotedString | MultilineString
  final val KeyValueSeparator = Colon | Equals | PlusEquals
  final val ArrayElementsEnding = RBracket | RBrace
  final val ValueEnding = Comma | RBrace | RBracket
  final val PathEnding = KeyValueSeparator | LBrace | SubRBrace | ValueEnding
  final val KeyEnding = PathEnding | Period
  final val UnquotedCharsOrParens = UnquotedChars | LParen | RParen
  final val ValueUnquotedChars = UnquotedCharsOrParens | Period
  final val SimpleValuePart = UnquotedCharsOrParens | Period | StringLiteral
  final val PathStart = UnquotedCharsOrParens | StringLiteral | Period | Dollar | BadCharacter
  final val SubstitutionPathStart = PathStart | KeyValueSeparator
  final val ValueStart = SimpleValuePart | LBrace | LBracket | Dollar | KeyValueSeparator | BadCharacter
  final val ObjectEntryStart = PathStart | UnquotedCharsOrParens

  case class Matcher(tokenSet: TokenSet, requireNoNewLine: Boolean, matchNewLine: Boolean, matchEof: Boolean) {
    def noNewLine: Matcher = copy(requireNoNewLine = true)
    def orNewLineOrEof: Matcher = copy(matchNewLine = true, matchEof = true)
    def orEof: Matcher = copy(matchEof = true)
  }

  implicit def token2Matcher(token: HoconTokenType): Matcher =
    Matcher(TokenSet.create(token), requireNoNewLine = false, matchNewLine = false, matchEof = false)

  implicit def tokenSet2Matcher(tokenSet: TokenSet): Matcher =
    Matcher(tokenSet, requireNoNewLine = false, matchNewLine = false, matchEof = false)
}
