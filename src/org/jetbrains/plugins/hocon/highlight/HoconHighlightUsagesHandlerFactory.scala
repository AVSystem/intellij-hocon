package org.jetbrains.plugins.hocon
package highlight

import navigation.HoconFindUsagesHandler
import psi._

import com.intellij.codeInsight.highlighting.{HighlightUsagesHandlerBase, HighlightUsagesHandlerFactoryBase}
import com.intellij.openapi.editor.Editor
import com.intellij.psi.{PsiElement, PsiFile}
import com.intellij.util.Consumer

import java.{util => ju}

class HoconHighlightUsagesHandlerFactory extends HighlightUsagesHandlerFactoryBase {
  def createHighlightUsagesHandler(editor: Editor, file: PsiFile, target: PsiElement): HoconHighlightKeyUsagesHandler =
    target.parentOfType[HKey].map(new HoconHighlightKeyUsagesHandler(editor, _)).orNull
}

class HoconHighlightKeyUsagesHandler(editor: Editor, hkey: HKey)
  extends HighlightUsagesHandlerBase[HKey](editor, hkey.hoconFile) {

  def computeUsages(targets: JList[_ <: HKey]): Unit = for {
    target <- targets.iterator.asScala
    foundKey <- HoconFindUsagesHandler.localUsagesOf(target)
    usageList = if (foundKey.inField) myWriteUsages else myReadUsages
  } {
    usageList.add(foundKey.getTextRange)
  }

  def getTargets: JList[HKey] = ju.Collections.singletonList(hkey)

  def selectTargets(targets: JList[_ <: HKey], selectionConsumer: Consumer[_ >: JList[_ <: HKey]]): Unit =
    selectionConsumer.consume(targets)
}
