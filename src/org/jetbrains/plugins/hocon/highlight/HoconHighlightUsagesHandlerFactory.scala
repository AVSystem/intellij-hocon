package org.jetbrains.plugins.hocon
package highlight

import java.{util => ju}

import com.intellij.codeInsight.highlighting.{HighlightUsagesHandlerBase, HighlightUsagesHandlerFactoryBase}
import com.intellij.openapi.editor.Editor
import com.intellij.psi.{PsiElement, PsiFile}
import com.intellij.util.Consumer
import org.jetbrains.plugins.hocon.psi._

import scala.annotation.tailrec
import scala.collection.JavaConverters.asScalaIteratorConverter

class HoconHighlightUsagesHandlerFactory extends HighlightUsagesHandlerFactoryBase {
  def createHighlightUsagesHandler(editor: Editor, file: PsiFile, target: PsiElement): HoconHighlightKeyUsagesHandler =
    target.parentOfType[HKey].map(new HoconHighlightKeyUsagesHandler(editor, file, _)).orNull
}

class HoconHighlightKeyUsagesHandler(editor: Editor, psiFile: PsiFile, hkey: HKey)
  extends HighlightUsagesHandlerBase[HKey](editor, psiFile) {

  def computeUsages(targets: JList[HKey]): Unit = {
    def findPaths(el: PsiElement): Iterator[HPath] = el match {
      case path: HPath => Iterator(path)
      case hoconFile: HoconPsiFile => hoconFile.toplevelEntries.flatMapIt(findPaths)
      case _: HInclude | _: HLiteralValue => Iterator.empty
      case hoconElement: HoconPsiElement => hoconElement.nonWhitespaceChildren.flatMap(findPaths)
      case _ => Iterator.empty
    }

    lazy val allPathsInFile = findPaths(psiFile).map(_.startingKeys).toList

    val foundKeys = targets.iterator.asScala.flatMap(_.fullContainingPath).flatMap {
      case (enclosingEntries, allKeys) =>
        val strPath = allKeys.map(_.stringValue)
        val resCtx = ToplevelCtx(enclosingEntries.hoconFile, Vector.empty)
        val opts = ResOpts(reverse = false, resolveIncludes = false, resolveSubstitutions = false)
        val fromFields = enclosingEntries.occurrences(strPath, opts, resCtx).flatMap(_.field.key)

        @tailrec def fromPath(keys: List[String], pathKeys: List[HKey]): Option[HKey] =
          (keys, pathKeys) match {
            case (key :: Nil, pathKey :: _) if key == pathKey.stringValue =>
              Some(pathKey)
            case (key :: rest, pathKey :: pathRest) if key == pathKey.stringValue =>
              fromPath(rest, pathRest)
            case _ => None
          }

        def fromPaths =
          if (enclosingEntries.isToplevel)
            allPathsInFile.iterator.flatMap(pathKeys => fromPath(strPath, pathKeys))
          else
            Iterator.empty

        fromFields ++ fromPaths
    }
    foundKeys.foreach { key =>
      val usages = key.parent match {
        case _: HKeyedField => myWriteUsages
        case _: HPath => myReadUsages
      }
      usages.add(key.getTextRange)
    }
  }

  def getTargets: JList[HKey] = ju.Collections.singletonList(hkey)

  def selectTargets(targets: JList[HKey], selectionConsumer: Consumer[JList[HKey]]): Unit =
    selectionConsumer.consume(targets)
}
