package org.jetbrains.plugins.hocon.highlight

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
    Iterator.iterate(target)(_.getParent).takeWhile {
      case null | _: PsiFile => false
      case _ => true
    }.collectFirst {
      case hkey: HKey => new HoconHighlightKeyUsagesHandler(editor, file, hkey)
    }.orNull
}

class HoconHighlightKeyUsagesHandler(editor: Editor, psiFile: PsiFile, hkey: HKey)
  extends HighlightUsagesHandlerBase[HKey](editor, psiFile) {

  def computeUsages(targets: ju.List[HKey]): Unit = {
    def findPaths(el: PsiElement): Iterator[HPath] = el match {
      case path: HPath => Iterator(path)
      case hoconFile: HoconPsiFile => findPaths(hoconFile.toplevelEntries)
      case _: HInclude | _: HLiteralValue => Iterator.empty
      case hoconElement: HoconPsiElement => hoconElement.nonWhitespaceChildren.flatMap(findPaths)
      case _ => Iterator.empty
    }

    lazy val allValidPathsInFile = findPaths(psiFile).map(_.startingValidKeys).toList

    val foundKeys = targets.iterator.asScala.flatMap(_.fullValidContainingPath).flatMap {
      case (enclosingEntries, allKeys) =>
        val fromFields = enclosingEntries.occurrences(allKeys, reverse = false).flatMap(_.key)

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
            allValidPathsInFile.iterator.flatMap(pathKeys => fromPath(allKeys, pathKeys))
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

    // don't highlight if there is only one occurrence
    if (myReadUsages.size + myWriteUsages.size == 1) {
      myReadUsages.clear()
      myWriteUsages.clear()
    }
  }

  def getTargets: ju.List[HKey] = ju.Collections.singletonList(hkey)

  def selectTargets(targets: ju.List[HKey], selectionConsumer: Consumer[ju.List[HKey]]): Unit =
    selectionConsumer.consume(targets)
}
