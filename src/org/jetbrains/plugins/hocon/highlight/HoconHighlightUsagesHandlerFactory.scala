package org.jetbrains.plugins.hocon
package highlight

import java.{util => ju}

import com.intellij.codeInsight.highlighting.{HighlightUsagesHandlerBase, HighlightUsagesHandlerFactoryBase}
import com.intellij.openapi.editor.Editor
import com.intellij.psi.{PsiElement, PsiFile}
import com.intellij.util.Consumer
import org.jetbrains.plugins.hocon.navigation.HoconUsageSearcher
import org.jetbrains.plugins.hocon.psi._

import scala.collection.JavaConverters.asScalaIteratorConverter

class HoconHighlightUsagesHandlerFactory extends HighlightUsagesHandlerFactoryBase {
  def createHighlightUsagesHandler(editor: Editor, file: PsiFile, target: PsiElement): HoconHighlightKeyUsagesHandler =
    target.parentOfType[HKey].map(new HoconHighlightKeyUsagesHandler(editor, _)).orNull
}

class HoconHighlightKeyUsagesHandler(editor: Editor, hkey: HKey)
  extends HighlightUsagesHandlerBase[HKey](editor, hkey.hoconFile) {

  def computeUsages(targets: JList[HKey]): Unit = for {
    target <- targets.iterator.asScala
    foundKey <- HoconUsageSearcher.localUsagesOf(target)
    usageList = if (foundKey.inField) myWriteUsages else myReadUsages
  } {
    usageList.add(foundKey.getTextRange)
  }

  def getTargets: JList[HKey] = ju.Collections.singletonList(hkey)

  def selectTargets(targets: JList[HKey], selectionConsumer: Consumer[JList[HKey]]): Unit =
    selectionConsumer.consume(targets)
}
