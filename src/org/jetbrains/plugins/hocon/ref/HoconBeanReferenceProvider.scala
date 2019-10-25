package org.jetbrains.plugins.hocon
package ref

import com.intellij.openapi.util.TextRange
import com.intellij.psi.{PsiElement, PsiReference, PsiReferenceProvider}
import com.intellij.util.ProcessingContext
import org.jetbrains.plugins.hocon.psi.{HArray, HFieldKey, HStringValue, HValuedField}
import org.jetbrains.plugins.hocon.semantics.{ResOpts, ToplevelCtx}

import scala.collection.mutable

object HoconBeanReferenceProvider {
  final val BeanRefIndicators = List("%ref", "%idref", "%parent", "%factory-bean")
}
class HoconBeanReferenceProvider extends PsiReferenceProvider {

  import HoconBeanReferenceProvider._

  def getReferencesByElement(element: PsiElement, context: ProcessingContext): Array[PsiReference] = element match {
    case hs: HStringValue => hs.parent match {
      case vf: HValuedField if vf.keyString.exists(BeanRefIndicators.contains) =>
        Array(new HoconBeanReference(hs))
      case arr: HArray => arr.parent match {
        case vf: HValuedField if vf.keyString.contains("%depends-on") =>
          Array(new HoconBeanReference(hs))
        case _ => PsiReference.EMPTY_ARRAY
      }
      case _ => PsiReference.EMPTY_ARRAY
    }
    case _ => PsiReference.EMPTY_ARRAY
  }
}

class HoconBeanReference(hs: HStringValue) extends PsiReference {
  def getCanonicalText: String = hs.stringValue
  def getElement: PsiElement = hs
  def getRangeInElement: TextRange = new TextRange(0, hs.getTextLength)

  def beanPath = List("beans", hs.stringValue)

  def resolve(): PsiElement = ToplevelCtx(hs.hoconFile)
    .occurrences(beanPath, ResOpts(reverse = true))
    .nextOption.map(_.hkey).orNull

  override def getVariants: Array[AnyRef] = {
    val toplevelCtx = ToplevelCtx(hs.hoconFile)
    val opts = ResOpts(reverse = true)

    val variantFields = toplevelCtx.occurrences(Some("beans"), opts).flatMap(_.occurrences(None, opts))
    val seenKeys = new mutable.HashSet[String]
    variantFields.filter(sf => seenKeys.add(sf.key)) // dirty, stateful filter
      .map(sf => new HoconPropertyLookupElement(sf))
      .toArray[AnyRef]
  }

  def handleElementRename(newElementName: String): PsiElement = null

  def bindToElement(element: PsiElement): PsiElement = null

  // important for ReferencesSearch and therefore Find Usages
  def isReferenceTo(element: PsiElement): Boolean = element match {
    case hkey: HFieldKey => hkey.fullStringPath.contains(beanPath)
    case _ => false
  }

  def isSoft: Boolean = true
}
