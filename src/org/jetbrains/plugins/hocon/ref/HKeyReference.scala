package org.jetbrains.plugins.hocon
package ref

import com.intellij.codeInsight.lookup.LookupElement
import com.intellij.openapi.util.TextRange
import com.intellij.psi.{ElementManipulators, PsiElement, PsiReference}
import org.jetbrains.plugins.hocon.psi._

import scala.collection.mutable

/**
 * @author ghik
 */
class HKeyReference(key: HKey) extends PsiReference {
  def getCanonicalText: String = key.stringValue

  def getElement: PsiElement = key

  def isReferenceTo(element: PsiElement): Boolean =
    element == resolve()

  def bindToElement(element: PsiElement): PsiElement = null

  def handleElementRename(newElementName: String): PsiElement =
    ElementManipulators.getManipulator(key).handleContentChange(key, newElementName)

  def isSoft = true

  def getRangeInElement: TextRange = ElementManipulators.getValueTextRange(key)

  // There's no single definition of a key that we could point to, just resolve to itself and leave the
  // job to each particular action, e.g. HoconGotoDeclarationHandler
  def resolve(): PsiElement = key

  // completion
  override def getVariants: Array[AnyRef] = {
    val file = key.getContainingFile
    val toplevelCtx = ToplevelCtx(file, ToplevelCtx.referenceFilesFor(file))

    val variantFields: Iterator[ResolvedField] = key.parent match {
      case path: HPath => path.prefix match {
        case Some(prefixPath) =>
          prefixPath.allValidKeys.map { path =>
            val strPath = path.map(_.stringValue)
            toplevelCtx.occurrences(strPath, reverse = true).flatMap(_.subOccurrences(None, reverse = true))
          }.getOrElse(Iterator.empty)
        case None =>
          toplevelCtx.occurrences(None, reverse = true)
      }
      case field: HKeyedField => field.prefixingField match {
        case Some(prefixField) => prefixField.fullValidContainingPath.iterator.flatMap {
          case (entries, path) =>
            val strPath = path.map(_.stringValue)
            val prefixOccurrences =
              if (entries.isToplevel) toplevelCtx.occurrences(strPath, reverse = true)
              else entries.occurrences(strPath, reverse = true, toplevelCtx)
            prefixOccurrences.flatMap(_.subOccurrences(None, reverse = true))
        }
        case None =>
          val entries = field.enclosingEntries
          if (entries.isToplevel) toplevelCtx.occurrences(None, reverse = true)
          else entries.occurrences(None, reverse = true, toplevelCtx)
      }
    }

    val seenKeys = new mutable.HashSet[String]
    variantFields.filter(sf => seenKeys.add(sf.key)) // dirty, stateful filter
      .map(sf => new HoconFieldLookupElement(sf))
      .toArray[AnyRef]
  }
}

class HoconFieldLookupElement(field: ResolvedField) extends LookupElement {
  def getLookupString: String = field.field.key.fold("")(_.getText)

  override def getObject: ResolvedField = field

  override def getPsiElement: PsiElement = field.field
}
