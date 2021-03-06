package org.jetbrains.plugins

import com.intellij.icons.AllIcons
import com.intellij.lang.ASTNode
import com.intellij.openapi.util.TextRange
import com.intellij.psi.tree.{IElementType, TokenSet}
import com.intellij.psi.util.PsiTreeUtil
import com.intellij.psi.{PsiDocumentManager, PsiElement, PsiWhiteSpace}
import com.intellij.ui.IconManager
import com.intellij.util.text.CharSequenceSubSequence
import org.jetbrains.plugins.hocon.lexer.HoconTokenType

import java.net.{MalformedURLException, URL}
import java.{lang => jl, util => ju}
import scala.annotation.tailrec
import scala.collection.AbstractIterator
import scala.collection.convert.{AsJavaExtensions, AsScalaExtensions}
import scala.language.implicitConversions
import scala.reflect.{ClassTag, classTag}

package object hocon extends AsJavaExtensions with AsScalaExtensions {
  type JList[T] = java.util.List[T]
  type JCollection[T] = java.util.Collection[T]
  type JMap[K, V] = java.util.Map[K, V]

  final val HoconIcon = AllIcons.FileTypes.Config
  final val PropertyIcon = IconManager.getInstance.getIcon("/icons/property.svg", getClass)

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
    /** Like `subSequence` but makes sure a wrapper is created instead of making a copy */
    def subSeqView(start: Int, end: Int = cs.length): CharSequence =
      new CharSequenceSubSequence(cs, start, end)

    def startsWith(str: String): Boolean =
      containsAt(0, str)

    def containsAt(index: Int, str: String): Boolean = {
      val strEndIdx = index + str.length
      cs.length >= strEndIdx && {
        @tailrec def loop(i: Int): Boolean =
          i >= strEndIdx || (str.charAt(i - index) == cs.charAt(i) && loop(i + 1))
        loop(index)
      }
    }

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
    def elementType: IElementType =
      elem.getNode.getElementType

    def parentOfType[T <: PsiElement : ClassTag]: Option[T] =
      Option(PsiTreeUtil.getParentOfType(elem, classTag[T].runtimeClass.asInstanceOf[Class[T]]))

    def getNextSibling(reverse: Boolean): PsiElement =
      if (reverse) elem.getPrevSibling else elem.getNextSibling

    def pos: String = {
      val doc = PsiDocumentManager.getInstance(elem.getProject).getDocument(elem.getContainingFile)
      val off = elem.getTextOffset
      val line = doc.getLineNumber(off)
      val column = off - doc.getLineStartOffset(line)
      s"${elem.getContainingFile.getName}:${line + 1}:$column"
    }

    def depthFirst: Iterator[PsiElement] = new DepthFirstIterator(elem)
  }

  private class DepthFirstIterator(root: PsiElement) extends AbstractIterator[PsiElement] {
    private var _next: PsiElement = root

    def hasNext: Boolean = _next ne null

    def next(): PsiElement =
      if (!hasNext) throw new NoSuchElementException
      else {
        val res = _next
        _next = res.getFirstChild match {
          case null => findNextSibling(res)
          case child => child
        }
        res
      }

    @tailrec private def findNextSibling(cur: PsiElement): PsiElement =
      if (cur eq root) null
      else cur.getNextSibling match {
        case null => findNextSibling(cur.getParent)
        case sibling => sibling
      }
  }

  implicit class StringOps(private val str: String) extends AnyVal {
    def indent(ind: String): String =
      ind + str.replace("\n", "\n" + ind)
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

    def nullOr[T >: Null](f: A => T): T = option.fold(null: T)(f)

    def flatMapIt[T](f: A => Iterator[T]): Iterator[T] = option match {
      case Some(a) => f(a)
      case None => Iterator.empty
    }
  }

  implicit class collectionOps[A](private val coll: IterableOnce[A]) extends AnyVal {
    def toJList[B >: A]: JList[B] = {
      val result = new ju.ArrayList[B]
      coll.iterator.foreach(result.add)
      result
    }
  }

  implicit class IteratorOps[A](private val it: Iterator[A]) extends AnyVal {
    def nextOption: Option[A] =
      if (it.hasNext) Option(it.next()) else None

    def collectOnly[T: ClassTag]: Iterator[T] =
      it.collect { case t: T => t }

    def flatCollect[B](f: PartialFunction[A, IterableOnce[B]]): Iterator[B] =
      it.flatMap(a => f.applyOrElse(a, (_: A) => Iterator.empty))

    def orElse(other: Iterator[A]): Iterator[A] = new AbstractIterator[A] {
      private var chosenIt: Iterator[A] = _

      def hasNext: Boolean =
        if (chosenIt != null) chosenIt.hasNext
        else {
          chosenIt = if (it.hasNext) it else other
          chosenIt.hasNext
        }

      def next(): A = {
        hasNext
        chosenIt.next()
      }
    }
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
