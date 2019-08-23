package org.jetbrains.plugins.hocon
package lexer

import com.intellij.lexer.LexerBase
import com.intellij.psi.tree.IElementType

import scala.annotation.{switch, tailrec}

object HoconLexer {

  case class State(raw: Int) extends AnyVal

  final val Initial = State(0)
  final val Value = State(1)
  final val SubStarting = State(2)
  final val SubStarted = State(3)
  final val Substitution = State(4)

  final val ForbiddenChars = """$"{}[]:=,+#`^?!@*&\"""
  final val UnquotedSpecialChars = """.()"""
  final val KeyForbiddenChars = ForbiddenChars + '.'
  final val SpecialWhitespace = "\u00A0\u2007\u202F\uFEFF"

  def isHoconWhitespace(char: Char): Boolean =
    char.isWhitespace || SpecialWhitespace.contains(char)
}

class HoconLexer extends LexerBase {

  import org.jetbrains.plugins.hocon.lexer.HoconLexer._
  import org.jetbrains.plugins.hocon.lexer.HoconTokenType._

  private def onContents(state: State): State = state match {
    case Initial | SubStarting => Value
    case SubStarted => Substitution
    case _ => state
  }

  private def onDollar(state: State): State = state match {
    case Initial | Value => SubStarting
    case SubStarted => Substitution
    case _ => state
  }

  private def onWhitespace(state: State, newLine: Boolean): State = state match {
    case _ if newLine => Initial
    case SubStarting => Value
    case SubStarted => Substitution
    case _ => state
  }

  private var input: CharSequence = _
  private var endOffset: Int = _
  private var stateBefore: State = Initial
  private var stateAfter: State = Initial

  private var tokenStart: Int = _
  private var tokenEnd: Int = _
  private var token: IElementType = _

  private def setNewToken(newToken: HoconTokenType, length: Int, newState: State): Unit = {
    tokenEnd = tokenStart + length
    token = newToken
    stateBefore = stateAfter
    stateAfter = newState
  }

  def getBufferEnd: Int = endOffset

  def getBufferSequence: CharSequence = input

  def advance(): Unit = {
    @tailrec def indexOfNewlineOrEof(idx: Int): Int =
      if (idx >= endOffset || input.charAt(idx) == '\n') idx
      else indexOfNewlineOrEof(idx + 1)

    def continuesUnquotedChars(seq: CharSequence, index: Int): Boolean = index < endOffset && {
      val char = seq.charAt(index)
      !UnquotedSpecialChars.contains(char) && !ForbiddenChars.contains(char) &&
        !isHoconWhitespace(char) && (char != '/' || index + 1 >= endOffset || seq.charAt(index + 1) != '/')
    }

    tokenStart = tokenEnd
    if (endOffset > tokenStart) {
      (input.charAt(tokenStart): @switch) match {
        case '$' => setNewToken(Dollar, 1, onDollar(stateAfter))
        case '?' if stateAfter == SubStarted => setNewToken(QMark, 1, Substitution)
        case '{' => stateAfter match {
          case SubStarting => setNewToken(SubLBrace, 1, SubStarted)
          case _ => setNewToken(LBrace, 1, Initial)
        }
        case '}' => stateAfter match {
          case SubStarted | Substitution => setNewToken(SubRBrace, 1, Value)
          case _ => setNewToken(RBrace, 1, Value)
        }
        case '[' => setNewToken(LBracket, 1, Initial)
        case ']' => setNewToken(RBracket, 1, Value)
        case '(' => setNewToken(LParen, 1, Initial)
        case ')' => setNewToken(RParen, 1, Value)
        case ':' => setNewToken(Colon, 1, Initial)
        case ',' => setNewToken(Comma, 1, Initial)
        case '=' => setNewToken(Equals, 1, Initial)
        case '+' if input.containsAt(tokenStart, "+=") =>
          setNewToken(PlusEquals, 2, Initial)
        case '.' => setNewToken(Period, 1, onContents(stateAfter))
        case '#' => setNewToken(HashComment, indexOfNewlineOrEof(tokenStart) - tokenStart, stateAfter)
        case '/' if input.containsAt(tokenStart, "//") =>
          setNewToken(DoubleSlashComment, indexOfNewlineOrEof(tokenStart) - tokenStart, stateAfter)

        case '\"' if input.containsAt(tokenStart, "\"\"\"") =>
          val strWithoutOpening = input.subSeqView(tokenStart + 3)
          val length = HoconConstants.MultilineStringEnd.findFirstMatchIn(strWithoutOpening)
            .map(m => m.end + 3).getOrElse(endOffset - tokenStart)
          setNewToken(MultilineString, length, onContents(stateAfter))

        case '\"' =>
          @tailrec
          def drainQuoted(offset: Int, escaping: Boolean): Int =
            if (offset >= endOffset) offset else {
              input.charAt(offset) match {
                case '\n' => offset
                case '\"' if !escaping => offset + 1
                case '\\' if !escaping => drainQuoted(offset + 1, escaping = true)
                case _ => drainQuoted(offset + 1, escaping = false)
              }
            }
          val length = drainQuoted(tokenStart + 1, escaping = false) - tokenStart
          setNewToken(QuotedString, length, onContents(stateAfter))

        case c if isHoconWhitespace(c) =>
          var idx = tokenStart
          var nl = false
          while (idx < endOffset && isHoconWhitespace(input.charAt(idx))) {
            nl ||= input.charAt(idx) == '\n'
            idx += 1
          }
          val token = if (nl) LineBreakingWhitespace else InlineWhitespace
          setNewToken(token, idx - tokenStart, onWhitespace(stateAfter, nl))

        case _ if continuesUnquotedChars(input, tokenStart) =>
          @tailrec def drainUnquoted(idx: Int): Int =
            if (!continuesUnquotedChars(input, idx)) idx
            else drainUnquoted(idx + 1)
          val length = drainUnquoted(tokenStart) - tokenStart
          setNewToken(UnquotedChars, length, onContents(stateAfter))

        case _ =>
          setNewToken(BadCharacter, 1, stateAfter)
      }
    } else {
      stateBefore = Initial
      stateAfter = Initial
      token = null
    }
  }

  def getTokenEnd: Int = tokenEnd

  def getTokenStart: Int = tokenStart

  def getTokenType: IElementType = {
    if (token == null) {
      advance()
    }
    token
  }

  def getState: Int = stateBefore.raw

  def start(buffer: CharSequence, startOffset: Int, endOffset: Int, initialState: Int): Unit = {
    this.token = null
    this.input = buffer
    this.tokenStart = startOffset
    this.tokenEnd = startOffset
    this.endOffset = endOffset
    this.stateBefore = State(initialState)
  }
}
