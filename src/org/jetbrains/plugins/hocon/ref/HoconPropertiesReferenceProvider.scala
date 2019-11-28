package org.jetbrains.plugins.hocon
package ref

import com.intellij.openapi.project.Project
import com.intellij.openapi.util.TextRange
import com.intellij.psi._
import com.intellij.util.ProcessingContext
import org.jetbrains.plugins.hocon.psi.{HFieldKey, HoconPsiElementFactory, HoconPsiFile}
import org.jetbrains.plugins.hocon.semantics.{ResOpts, ToplevelCtx}
import org.jetbrains.plugins.hocon.settings.HoconProjectSettings

import scala.annotation.tailrec
import scala.collection.mutable

class HoconPropertiesReferenceProvider extends PsiReferenceProvider {
  private def isEnabled(project: Project): Boolean =
    HoconProjectSettings.getInstance(project).propertyReferencesOnStrings

  def getReferencesByElement(element: PsiElement, context: ProcessingContext): Array[PsiReference] =
    if (!isEnabled(element.getProject)) PsiReference.EMPTY_ARRAY
    else {
      val res = for {
        lit <- element.opt.collectOnly[PsiLiteralValue]
        strValue <- lit.getValue.opt.collectOnly[String]
        hpath <- HoconPsiElementFactory.createPath(strValue, PsiManager.getInstance(element.getProject)).opt
        strPath <- hpath.fullStringPath
      } yield {
        val text = element.getText
        val fullRange = ElementManipulators.getValueTextRange(element)

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
            val ref = new HoconPropertyReference(strPath, revIndex, key, element, lit, keyRange)
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
  element: PsiElement,
  lit: PsiLiteralValue,
  range: TextRange
) extends PsiReference {
  def getCanonicalText: String = key
  def getElement: PsiElement = element
  def getRangeInElement: TextRange = range

  def createContext: ToplevelCtx = element.getContainingFile match {
    case hf: HoconPsiFile => ToplevelCtx(hf)
    case _ => ToplevelCtx(element, ToplevelCtx.ApplicationResource)
  }

  def resolve(): PsiElement = createContext
    .occurrences(fullPath, ResOpts(reverse = true))
    .nextOption.flatMap(_.ancestorField(reverseIndex))
    .map(_.hkey).orNull

  override def getVariants: Array[AnyRef] = {
    val toplevelCtx = ToplevelCtx(element, ToplevelCtx.ApplicationResource)
    val opts = ResOpts(reverse = true)

    val variantFields = fullPath.dropRight(reverseIndex + 1) match {
      case Nil => toplevelCtx.occurrences(None, opts)
      case prefixPath => toplevelCtx.occurrences(prefixPath, opts).flatMap(_.occurrences(None, opts))
    }
    val seenKeys = new mutable.HashSet[String]
    variantFields.filter(sf => seenKeys.add(sf.key)) // dirty, stateful filter
      .map(sf => new HoconPropertyLookupElement(sf))
      .toArray[AnyRef]
  }

  def handleElementRename(newElementName: String): PsiElement = null

  def bindToElement(element: PsiElement): PsiElement = null

  // important for ReferencesSearch and therefore Find Usages
  def isReferenceTo(element: PsiElement): Boolean = element match {
    case hkey: HFieldKey => hkey.fullStringPath.contains(fullPath.dropRight(reverseIndex))
    case _ => false
  }

  def isSoft: Boolean = true
}
