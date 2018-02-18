package intellijhocon

import com.intellij.lexer.LexerBase
import scala.util.matching.Regex
import com.intellij.psi.tree.IElementType

object HoconLexer {

  case class State(raw: Int) extends AnyVal

  val Initial = State(0)
  val Value = State(1)
  val RefStarting = State(2)
  val RefStarted = State(3)
  val Reference = State(4)

  val States = Array(Initial, Value, RefStarting, RefStarted, Reference)
}

class HoconLexer extends LexerBase {

  import HoconLexer._
  import HoconTokenType._

  implicit class CharSequenceOps(cs: CharSequence) {
    def startsWith(str: String) =
      cs.length >= str.length && str.contentEquals(cs.subSequence(0, str.length))
  }

  abstract class TokenMatcher(val token: IElementType) {
    def matchToken(seq: CharSequence, state: State): Option[Int]

    def transition(state: State): State
  }

  class LiteralTokenMatcher(str: String,
    token: IElementType,
    condition: State => Boolean = _ => true,
    transitionFun: State => State = identity)
    extends TokenMatcher(token) {

    def matchToken(seq: CharSequence, state: State) =
      if (condition(state) && seq.startsWith(str))
        Some(str.length)
      else None

    def transition(state: State) =
      transitionFun(state)
  }

  class RegexTokenMatcher(regex: Regex,
    token: IElementType,
    condition: State => Boolean = _ => true,
    transitionFun: State => State = identity)
    extends TokenMatcher(token) {

    def matchToken(seq: CharSequence, state: State) =
      if (condition(state))
        regex.findPrefixMatchOf(seq).map(_.end)
      else None

    def transition(state: State) =
      transitionFun(state)
  }

  def forceState(state: State): State => State =
    _ => state

  def always: State => Boolean =
    _ => true

  def onContents(state: State) = state match {
    case Initial => Value
    case RefStarting | RefStarted => Reference
    case _ => state
  }

  def isAnyOf(states: State*): State => Boolean =
    states.contains

  def isNoneOf(states: State*): State => Boolean =
    !states.contains(_)

  val notReference = isAnyOf(Initial, Value)

  val matchers = List(
    WhitespaceMatcher,
    new RegexTokenMatcher( """\$(?=\{)""".r, Dollar, notReference, forceState(RefStarting)),
    new LiteralTokenMatcher("{", RefLBrace, isAnyOf(RefStarting), forceState(RefStarted)),
    new LiteralTokenMatcher("?", QMark, isAnyOf(RefStarted), forceState(Reference)),
    new LiteralTokenMatcher("}", RefRBrace, isAnyOf(RefStarting, RefStarted, Reference), forceState(Value)),
    new LiteralTokenMatcher("{", LBrace, always, forceState(Initial)),
    new LiteralTokenMatcher("}", RBrace, always, forceState(Value)),
    new LiteralTokenMatcher("[", LBracket, always, forceState(Initial)),
    new LiteralTokenMatcher("]", RBracket, always, forceState(Value)),
    new LiteralTokenMatcher(":", Colon, always, forceState(Initial)),
    new LiteralTokenMatcher(",", Comma, always, forceState(Initial)),
    new LiteralTokenMatcher("=", Equals, always, forceState(Initial)),
    new LiteralTokenMatcher("+=", PlusEquals, always, forceState(Initial)),
    new LiteralTokenMatcher(".", Period, always, onContents),
    new LiteralTokenMatcher("\n", NewLine, isNoneOf(Initial), forceState(Initial)),
    new RegexTokenMatcher( """#[^\n]*""".r, HashComment, always, identity),
    new RegexTokenMatcher( """//[^\n]*""".r, DoubleSlashComment, always, identity),
    UnquotedCharsMatcher,
    new RegexTokenMatcher("\"{3}([^\"]+|\"{1,2}[^\"])*(\"{3,}|$)".r, MultilineString, always, onContents),
    QuotedStringMatcher,
    new RegexTokenMatcher(".".r, BadCharacter, always, identity)
  )

  val matchersByToken = matchers.map(m => (m.token, m)).toMap

  val forbidden = """$"{}[]:=,+#`^?!@*&\"""
  val specialWhitespace = "\u00A0\u2007\u202F\uFEFF"

  def isHoconWhitespace(char: Char) = char.isWhitespace || specialWhitespace.contains(char)

  def isCStyleComment(seq: CharSequence, index: Int) =
    seq.subSequence(index, seq.length).startsWith("//")

  def continuesUnquotedChars(seq: CharSequence, index: Int) = index < seq.length && {
    val char = seq.charAt(index)
    char != '.' && !forbidden.contains(char) && !isHoconWhitespace(char) && !isCStyleComment(seq, index)
  }

  object QuotedStringMatcher extends TokenMatcher(QuotedString) {
    def transition(state: State) =
      onContents(state)

    def matchToken(seq: CharSequence, state: State) = if (seq.charAt(0) == '\"') {
      def drain(offset: Int): Int =
        if (offset < seq.length) {
          seq.charAt(offset) match {
            case '\"' => offset + 1
            case '\n' => offset
            case _ => drain(offset + 1)
          }
        } else offset
      Some(drain(1))
    } else None
  }

  object UnquotedCharsMatcher extends TokenMatcher(UnquotedChars) {
    def matchToken(seq: CharSequence, state: State) = {
      var c = 0
      while (continuesUnquotedChars(seq, c)) {
        c += 1
      }
      if (c > 0) Some(c) else None
    }

    def transition(state: State) =
      onContents(state)
  }

  object WhitespaceMatcher extends TokenMatcher(Whitespace) {
    def matchToken(seq: CharSequence, state: State) = {
      var c = 0
      val acceptNewlines = state == Initial
      def char = seq.charAt(c)
      while (c < seq.length && (acceptNewlines || char != '\n') && isHoconWhitespace(char)) {
        c += 1
      }
      if (c > 0) {
        Some(c)
      } else None
    }

    def transition(state: State) = state match {
      case RefStarting | RefStarted => Reference
      case _ => state
    }
  }

  private var input: CharSequence = _
  private var endOffset: Int = _
  private var state: State = Initial

  private var tokenStart: Int = _
  private var tokenEnd: Int = _
  private var token: IElementType = _

  def getBufferEnd = endOffset

  def getBufferSequence = input

  def advance() = {
    tokenStart = tokenEnd
    val seq = input.subSequence(tokenStart, endOffset)
    if (seq.length > 0) {

      val newState = if (token != null) matchersByToken(token).transition(state) else state

      def findMatch(matchers: List[TokenMatcher]): (Int, IElementType) = {
        val head :: tail = matchers
        head.matchToken(seq, newState) match {
          case Some(result) => (result, head.token)
          case _ => findMatch(tail)
        }
      }

      val (length, newToken) = findMatch(matchers)

      tokenEnd = tokenStart + length
      token = newToken
      state = newState
    } else {
      state = Initial
      token = null
    }
  }

  def getTokenEnd = tokenEnd

  def getTokenStart = tokenStart

  def getTokenType = {
    if (token == null) {
      advance()
    }
    token
  }

  def getState = state.raw

  def start(buffer: CharSequence, startOffset: Int, endOffset: Int, initialState: Int) = {
    this.token = null
    this.input = buffer
    this.tokenStart = startOffset
    this.tokenEnd = startOffset
    this.endOffset = endOffset
    this.state = States(initialState)
  }
}
