package org.jetbrains.plugins.hocon
package lexer

import com.intellij.psi.tree.TokenSet

object HoconTokenSets {

  import org.jetbrains.plugins.hocon.lexer.HoconTokenType.*

  final val Empty: TokenSet = TokenSet.EMPTY
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

  into case class Matcher(tokenSet: TokenSet, requireNoNewLine: Boolean, matchNewLine: Boolean, matchEof: Boolean)

  extension(matcher: Matcher){
    def noNewLine: Matcher = matcher.copy(requireNoNewLine = true)
    def orNewLineOrEof: Matcher =matcher. copy(matchNewLine = true, matchEof = true)
    def orEof: Matcher = matcher.copy(matchEof = true)
  }

  given Conversion[HoconTokenType, Matcher] = token =>
    Matcher(TokenSet.create(token), requireNoNewLine = false, matchNewLine = false, matchEof = false)

  given Conversion[TokenSet, Matcher] = tokenSet =>
    Matcher(tokenSet, requireNoNewLine = false, matchNewLine = false, matchEof = false)
}