package org.jetbrains.plugins.hocon
package formatting

import com.intellij.formatting.{FormattingContext, FormattingModelBuilder}
import com.intellij.lang.ASTNode
import com.intellij.openapi.util.TextRange
import com.intellij.psi.PsiFile
import com.intellij.psi.formatter.{FormattingDocumentModelImpl, PsiBasedFormattingModel}

class HoconFormattingModelBuilder extends FormattingModelBuilder {
  override def createModel(context: FormattingContext): PsiBasedFormattingModel = {
    val element = context.getPsiElement
    val containingFile = element.getContainingFile
    val block = new HoconBlock(new HoconFormatter(context.getCodeStyleSettings), element.getNode, null, null, null)
    new PsiBasedFormattingModel(containingFile, block, FormattingDocumentModelImpl.createOn(containingFile))
  }

  override def getRangeAffectingIndent(file: PsiFile, offset: Int, elementAtOffset: ASTNode): TextRange =
    elementAtOffset.getTextRange
}
