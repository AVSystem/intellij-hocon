package org.jetbrains.plugins.hocon
package ref

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
    ElementManipulators.handleContentChange(key, newElementName)

  def isSoft = true

  def getRangeInElement: TextRange = ElementManipulators.getValueTextRange(key)

  def resolve(): PsiElement =
    key.resolved.map(_.hkey).orNull

  // completion
  override def getVariants: Array[AnyRef] = {
    val file = key.hoconFile
    val toplevelCtx = ToplevelCtx(file)
    val opts = ResOpts(reverse = true)

    val variantFields: Iterator[ResolvedField] = key.parent match {
      case path: HPath => path.prefix match {
        case Some(prefixPath) =>
          prefixPath.allKeys.flatMapIt { path =>
            val strPath = path.map(_.stringValue)
            toplevelCtx.occurrences(strPath, opts).flatMap(_.subOccurrences(None, opts))
          }
        case None =>
          toplevelCtx.occurrences(None, opts)
      }
      case field: HKeyedField =>
        val entries = field.outermostEntries
        field.prefixingField match {
          case Some(prefixField) => prefixField.fullStringPath.iterator.flatMap { strPath =>
            val prefixOccurrences =
              if (entries.isToplevel) toplevelCtx.occurrences(strPath, opts)
              else entries.occurrences(strPath, opts, toplevelCtx)
            prefixOccurrences.flatMap(_.subOccurrences(None, opts))
          }
          case None =>
            if (entries.isToplevel) toplevelCtx.occurrences(None, opts)
            else entries.occurrences(None, opts, toplevelCtx)
        }
    }

    val seenKeys = new mutable.HashSet[String]
    variantFields.filter(sf => seenKeys.add(sf.key)) // dirty, stateful filter
      .map(sf => new HoconPropertyLookupElement(sf))
      .toArray[AnyRef]
  }
}
