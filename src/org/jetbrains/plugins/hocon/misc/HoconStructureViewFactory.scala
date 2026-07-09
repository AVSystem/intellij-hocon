package org.jetbrains.plugins.hocon
package misc

import psi.*

import com.intellij.icons.AllIcons
import com.intellij.ide.structureView.impl.common.PsiTreeElementBase
import com.intellij.ide.structureView.*
import com.intellij.ide.util.treeView.smartTree.{SortableTreeElement, Sorter}
import com.intellij.lang.PsiStructureViewFactory
import com.intellij.openapi.editor.Editor
import com.intellij.psi.{PsiElement, PsiFile}

import javax.swing.Icon

class HoconStructureViewFactory extends PsiStructureViewFactory {
  override def getStructureViewBuilder(psiFile: PsiFile): StructureViewBuilder = psiFile match {
    case hoconFile: HoconPsiFile =>
      new TreeBasedStructureViewBuilder {
        override def createStructureViewModel(editor: Editor): StructureViewModel =
          new HoconStructureViewModel(hoconFile, editor)
      }
    case _ => null
  }
}

class HoconStructureViewModel(psiFile: HoconPsiFile, editor: Editor)
  extends StructureViewModelBase(psiFile, editor, new HoconStructureViewElement(psiFile))
    with StructureViewModel.ElementInfoProvider {

  override def getSorters: Array[Sorter] = Array(Sorter.ALPHA_SORTER)

  // Classes whose elements may be selected when syncing the tree with the editor caret.
  override def getSuitableClasses: Array[Class[?]] =
    Array(classOf[HObjectField], classOf[HInclude])

  override def isAlwaysShowsPlus(element: StructureViewTreeElement): Boolean = false

  override def isAlwaysLeaf(element: StructureViewTreeElement): Boolean = element.getValue match {
    case _: HInclude => true
    case _ => false
  }
}

final class HoconStructureViewElement(element: PsiElement)
  extends PsiTreeElementBase[PsiElement](element) with SortableTreeElement {

  import HoconStructureViewElement.*

  override def getChildrenBase: JList[StructureViewTreeElement] =
    childEntries(element).map(e => new HoconStructureViewElement(e): StructureViewTreeElement).toJList

  override def getPresentableText: String = element match {
    case file: HoconPsiFile => file.getName
    case of: HObjectField => pathText(of)
    case incl: HInclude => includeText(incl)
    case _ => element.getText
  }

  override def getLocationString: String = element match {
    case of: HObjectField if getChildrenBase.isEmpty =>
      valueOf(of).map(preview).orNull
    case _ => null
  }

  override def getIcon(open: Boolean): Icon = element match {
    case _: HoconPsiFile => HoconIcon
    case _: HInclude => AllIcons.Nodes.Include
    case _ => PropertyIcon
  }

  override def getAlphaSortKey: String =
    Option(getPresentableText).getOrElse("")
}

object HoconStructureViewElement {

  /** Child entries shown under a given element: top-level entries for the file, or the entries of an object-valued
    * field. Includes are listed but not expanded.
    */
  private def childEntries(element: PsiElement): Iterator[HObjectEntry] = element match {
    case file: HoconPsiFile =>
      file.toplevelEntries.iterator.flatMap(_.entries(reverse = false))
    case of: HObjectField =>
      valueOf(of).iterator.flatMap(objectEntries)
    case _ =>
      Iterator.empty
  }

  /** Object entries directly contained in a value - looking through concatenations (e.g. `a = {...} {...}`). */
  private def objectEntries(value: HValue): Iterator[HObjectEntry] = value match {
    case obj: HObject => obj.entries.entries(reverse = false)
    case conc: HConcatenation => conc.findChildren[HValue].flatMap(objectEntries)
    case _ => Iterator.empty
  }

  /** The value finally assigned by an object field, looking through prefixed paths (e.g. `a.b.c = value`). */
  private def valueOf(of: HObjectField): Option[HValue] =
    of.keyedField.fieldsInPathForward.toSeq.lastOption.collectOnly[HValuedField].flatMap(_.value)

  /** Dotted key path of a field, e.g. `foo.bar.baz` for `foo.bar.baz = ...`. */
  private def pathText(of: HObjectField): String =
    of.keyedField.fieldsInPathForward.map(_.keyString.getOrElse("?")).mkString(".")

  private def includeText(incl: HInclude): String =
    incl.included.target.map(t => s"include ${t.stringValue}").getOrElse("include")

  /** A short, single-line preview of a leaf value to display next to the key. */
  private def preview(value: HValue): String = {
    val text = value match {
      case _: HArray => "[…]"
      case other => other.getText
    }
    val singleLine = text.replaceAll("\\s+", " ").trim
    if (singleLine.length > 60) singleLine.take(59) + "…" else singleLine
  }
}
