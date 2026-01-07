package org.jetbrains.plugins.hocon
package editor

import com.intellij.lang.CodeDocumentationAwareCommenter
import com.intellij.psi.PsiComment
import com.intellij.psi.tree.IElementType
import org.jetbrains.plugins.hocon.lexer.HoconTokenType

class HoconCommenter extends CodeDocumentationAwareCommenter {
  def getLineCommentPrefix = "//"

  def getLineCommentTokenType: IElementType = HoconTokenType.DoubleSlashComment

  def getBlockCommentSuffix: String | Null = null

  def getBlockCommentPrefix: String | Null = null

  def getCommentedBlockCommentPrefix: String | Null = null

  def getCommentedBlockCommentSuffix: String | Null = null

  def getDocumentationCommentLinePrefix: String | Null = null

  def getBlockCommentTokenType: IElementType | Null = null

  def getDocumentationCommentTokenType: IElementType | Null = null

  def isDocumentationComment(element: PsiComment): Boolean = false

  def getDocumentationCommentSuffix: String | Null = null

  def getDocumentationCommentPrefix: String | Null = null
}
