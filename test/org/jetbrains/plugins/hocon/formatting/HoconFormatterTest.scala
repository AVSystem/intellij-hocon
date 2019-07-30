package org.jetbrains.plugins.hocon
package formatting

import com.intellij.application.options.CodeStyle
import com.intellij.openapi.util.JDOMUtil
import com.intellij.psi.codeStyle.CodeStyleManager
import org.jetbrains.plugins.hocon.CommonUtil.TextRange
import org.junit.runner.RunWith
import org.junit.runners.AllTests

@RunWith(classOf[AllTests])
class HoconFormatterTest extends HoconFileSetTestCase("formatter") {

  override protected def transform(data: Seq[String]): String = {
    val Seq(settingsXml, input) = data

    val settings = CodeStyle.getSettings(myProject)
    settings.readExternal(JDOMUtil.load(settingsXml))

    val psiFile = createPseudoPhysicalHoconFile(input)

    inWriteCommandAction {
      val TextRange(start, end) = psiFile.getTextRange
      CodeStyleManager.getInstance(myProject).reformatText(psiFile, start, end)
    }

    psiFile.getText
  }
}

object HoconFormatterTest extends TestSuiteCompanion[HoconFormatterTest]
