package org.jetbrains.plugins.hocon
package ref

import com.intellij.openapi.util.TextRange
import com.intellij.psi._
import com.intellij.util.ProcessingContext
import org.jetbrains.plugins.hocon.indexing.HoconPathIndex
import org.jetbrains.plugins.hocon.psi.{HKey, HoconPsiElementFactory}

import scala.annotation.tailrec
import scala.collection.mutable

class HoconPropertiesReferenceProvider extends PsiReferenceProvider {
  def getReferencesByElement(element: PsiElement, context: ProcessingContext): Array[PsiReference] = {
    val res = for {
      lit <- element.opt.collectOnly[PsiLiteral]
      strValue <- lit.getValue.opt.collectOnly[String]
      hpath <- HoconPsiElementFactory.createPath(strValue, PsiManager.getInstance(element.getProject)).opt
      strPath <- hpath.fullStringPath
    } yield {
      val text = lit.getText
      val fullRange = ElementManipulators.getValueTextRange(lit)

      def makeRefs(off: Int, keys: List[String]): List[HoconPropertyReference] = keys match {
        case Nil => Nil
        case key :: tail =>
          // Each key may be itself quoted inside the string and we don't know exactly what quoting scheme is used
          // by given string (e.g. unquoted, quoted, multiline...). For this reason we can never assume any
          // representation of given key within the string literal. Therefore, we just split the string contents by
          // dot, taking into account that keys themselves may also contain dots which must be omitted when splitting.
          @tailrec def keyEndOffset(keyStart: Int, keyInnerDots: Int): Int =
            text.indexOf('.', keyStart) match {
              case -1 => fullRange.getEndOffset
              case dotOff if keyInnerDots == 0 => dotOff
              case dotOff => keyEndOffset(dotOff + 1, keyInnerDots - 1)
            }

          val endOffset = keyEndOffset(off, key.count(_ == '.'))
          val subRefs = makeRefs(endOffset + 1, tail)
          val nextRef = subRefs.headOption
          val revIndex = nextRef.fold(0)(_.reverseIndex + 1)
          val keyRange = new TextRange(off, endOffset)
          val ref = new HoconPropertyReference(strPath, revIndex, key, lit, keyRange)
          ref :: subRefs
      }

      makeRefs(fullRange.getStartOffset, strPath).toArray[PsiReference]
    }
    res.getOrElse(PsiReference.EMPTY_ARRAY)
  }
}

class HoconPropertyReference(
  fullPath: List[String],
  val reverseIndex: Int,
  key: String,
  lit: PsiLiteral,
  range: TextRange
) extends PsiPolyVariantReference {
  def getCanonicalText: String = key
  def getElement: PsiElement = lit
  def getRangeInElement: TextRange = range

  def resolve(): PsiElement = multiResolve(false) match {
    case Array(single) => single.getElement
    case _ => null
  }

  def multiResolve(incompleteCode: Boolean): Array[ResolveResult] = {
    val builder = new mutable.ArrayBuilder.ofRef[ResolveResult]
    val scope = IncludedFileReferenceSet.classpathScope(lit.getContainingFile)
    val seen = new mutable.HashSet[HKey]
    HoconPathIndex.processHKeys(fullPath, lit.getProject, scope, _.inFields.lastOption.iterator) { foundKey =>
      foundKey.field.flatMap(_.makeContext).flatMap(_.ancestorField(reverseIndex)).foreach { resField =>
        if (seen.add(resField.hkey)) {
          builder += new PsiElementResolveResult(resField.hkey)
        }
      }
    }
    builder.result()
  }

  def handleElementRename(newElementName: String): PsiElement = null

  def bindToElement(element: PsiElement): PsiElement = null

  def isReferenceTo(element: PsiElement): Boolean = false

  def isSoft: Boolean = true
}
