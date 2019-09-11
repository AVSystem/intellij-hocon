package org.jetbrains.plugins.hocon
package manipulators

import com.intellij.openapi.util.TextRange
import com.intellij.openapi.util.text.StringUtil
import com.intellij.psi.{AbstractElementManipulator, PsiManager}
import org.jetbrains.plugins.hocon.lexer.HoconLexer
import org.jetbrains.plugins.hocon.psi.{HFieldKey, HKey, HSubstitutionKey, HoconPsiElementFactory}

/**
 * @author ghik
 */
class HKeyManipulator extends AbstractElementManipulator[HKey] {

  import org.jetbrains.plugins.hocon.lexer.HoconTokenType._

  def handleContentChange(key: HKey, range: TextRange, newContent: String): HKey = {
    val psiManager = PsiManager.getInstance(key.getProject)
    val allStringTypes = key.keyParts.map(_.stringType).toSet

    lazy val escapedContent =
      StringUtil.escapeStringCharacters(newContent)

    lazy val needsQuoting =
      newContent.isEmpty || newContent.startsWith(" ") || newContent.endsWith(" ") ||
        escapedContent != newContent || newContent.exists(HoconLexer.KeyForbiddenChars.contains(_))

    val quotedEscapedContent =
      if (allStringTypes.contains(MultilineString))
        "\"\"\"" + newContent + "\"\"\""
      else if (allStringTypes.contains(QuotedString) || needsQuoting)
        "\"" + escapedContent + "\""
      else
        newContent

    val newKey = key match {
      case _: HFieldKey => HoconPsiElementFactory.createFieldKey(quotedEscapedContent, psiManager)
      case _: HSubstitutionKey => HoconPsiElementFactory.createSubstitutionKey(quotedEscapedContent, psiManager)
    }
    key.replace(newKey)
    newKey
  }
}
