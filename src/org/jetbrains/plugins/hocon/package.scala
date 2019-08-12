package org.jetbrains.plugins

import java.net.{MalformedURLException, URL}
import java.{lang => jl, util => ju}

import com.intellij.lang.ASTNode
import com.intellij.openapi.util.TextRange
import com.intellij.psi.tree.{IElementType, TokenSet}
import com.intellij.psi.util.PsiTreeUtil
import com.intellij.psi.{PsiDocumentManager, PsiElement, PsiWhiteSpace}
import org.jetbrains.plugins.hocon.lexer.HoconTokenType
import org.jetbrains.plugins.hocon.psi.HoconPsiElement

import scala.collection.GenTraversableOnce
import scala.language.implicitConversions
import scala.reflect.{ClassTag, classTag}

package object hocon {
  def notWhiteSpaceSibling(element: PsiElement)
    (sibling: PsiElement => PsiElement): PsiElement = {
    var result = sibling(element)
    while (isWhiteSpace(result)) {
      result = sibling(result)
    }
    result
  }

  private[this] def isWhiteSpace(element: PsiElement): Boolean = element match {
    case null => false
    case _: PsiWhiteSpace => true
    case _ => element.getNode.getElementType match {
      case HoconTokenType.InlineWhitespace => true
      case _ => false
    }
  }

  implicit def liftSingleToken(token: IElementType): TokenSet = TokenSet.create(token)

  implicit class TokenSetOps(tokenSet: TokenSet) {
    def |(otherTokenSet: TokenSet): TokenSet =
      TokenSet.orSet(tokenSet, otherTokenSet)

    def &(otherTokenSet: TokenSet): TokenSet =
      TokenSet.andSet(tokenSet, otherTokenSet)

    def &^(otherTokenSet: TokenSet): TokenSet =
      TokenSet.andNot(tokenSet, otherTokenSet)

    def unapply(tokenType: IElementType): Boolean =
      tokenSet.contains(tokenType)

    val extractor: TokenSetOps = this
  }

  implicit def token2TokenSetOps(token: IElementType): TokenSetOps = new TokenSetOps(token)

  implicit class CharSequenceOps(private val cs: CharSequence) extends AnyVal {
    def startsWith(str: String): Boolean =
      cs.length >= str.length && str.contentEquals(cs.subSequence(0, str.length))

    def charIterator: Iterator[Char] =
      Iterator.range(0, cs.length).map(cs.charAt)
  }

  implicit class NodeOps(private val node: ASTNode) extends AnyVal {
    def childrenIterator: Iterator[ASTNode] =
      Iterator.iterate(node.getFirstChildNode)(_.getTreeNext).takeWhile(_ != null)

    def children: Seq[ASTNode] =
      childrenIterator.toVector: Seq[ASTNode]

    def hasSingleChild: Boolean =
      node.getFirstChildNode != null && node.getFirstChildNode.getTreeNext == null
  }

  implicit class PsiElementOps(private val elem: PsiElement) extends AnyVal {
    def parentOfType[T <: HoconPsiElement : ClassTag]: Option[T] =
      Option(PsiTreeUtil.getParentOfType(elem, classTag[T].runtimeClass.asInstanceOf[Class[T]]))

    def getNextSibling(reverse: Boolean): PsiElement =
      if (reverse) elem.getPrevSibling else elem.getNextSibling

    def pos: String = {
      val doc = PsiDocumentManager.getInstance(elem.getProject).getDocument(elem.getContainingFile)
      val off = elem.getTextOffset
      val line = doc.getLineNumber(off)
      val column = off - doc.getLineStartOffset(line)
      s"$line:$column"
    }
  }

  implicit class StringOps(private val str: String) extends AnyVal {
    def indent(ind: String): String =
      ind + str.replaceAllLiterally("\n", "\n" + ind)
  }

  implicit class universalOps[T](private val t: T) extends AnyVal {
    def opt: Option[T] = Option(t)

    def setup(code: T => Unit): T = {
      code(t)
      t
    }

    def typedOpt[U: ClassTag]: Option[U] = t match {
      case u: U => Some(u)
      case _ => None
    }

    def debug(msg: T => String): T = {
      println(msg(t))
      t
    }
  }

  implicit class OptionOps[A](private val option: Option[A]) extends AnyVal {
    def collectOnly[T: ClassTag]: Option[T] = option.collect { case t: T => t }
  }

  implicit class collectionOps[A](private val coll: GenTraversableOnce[A]) extends AnyVal {
    def toJList[B >: A]: ju.List[B] = {
      val result = new ju.ArrayList[B]
      coll.foreach(result.add)
      result
    }
  }

  implicit class IteratorOps[A](private val it: Iterator[A]) extends AnyVal {
    def nextOption: Option[A] =
      if (it.hasNext) Option(it.next()) else None

    def collectOnly[T: ClassTag]: Iterator[T] =
      it.collect { case t: T => t }

    def flatCollect[B](f: PartialFunction[A, TraversableOnce[B]]): Iterator[B] =
      it.flatMap(a => f.applyOrElse(a, (_: A) => Iterator.empty))
  }

  private final val quotedCharPattern = "\\\\[\\\\\"/bfnrt]".r
  private final val quotedUnicodePattern = "\\\\u([0-9A-Fa-f]{4})".r

  def unquote(str: String): String = {
    var result = str.stripPrefix("\"").stripSuffix("\"")
    result = quotedCharPattern.replaceAllIn(result, m => m.group(0).charAt(1) match {
      case '\\' => "\\"
      case '/' => "/"
      case '"' => "\""
      case 'b' => "\b"
      case 'f' => "\f"
      case 'n' => "\n"
      case 'r' => "\r"
      case 't' => "\t"
    })
    quotedUnicodePattern.replaceAllIn(result, m => jl.Short.parseShort(m.group(1), 16).toChar.toString)
  }

  def uncaps(str: String): String =
    str.replace('_', ' ').toLowerCase

  object TextRange {
    def unapply(textRange: TextRange): Some[(Int, Int)] =
      Some((textRange.getStartOffset, textRange.getEndOffset))

    def apply(start: Int, end: Int): TextRange =
      com.intellij.openapi.util.TextRange.create(start, end)
  }

  def isValidUrl(str: String): Boolean =
    try {
      new URL(str)
      true
    } catch {
      case _: MalformedURLException => false
    }
}