package org.jetbrains.plugins.hocon

import com.intellij.openapi.fileTypes.FileTypeManager
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiFileFactory
import com.intellij.util.LocalTimeCounter
<<<<<<< HEAD:test/org/jetbrains/plugins/hocon/HoconTestUtils.scala
<<<<<<< HEAD:test/org/jetbrains/plugins/hocon/HoconTestUtils.scala
import org.jetbrains.plugins.hocon.psi.HoconPsiFile
=======
>>>>>>> ecf1b15... HOCON: reformat code, organize imports:intellij-hocon/src/test/scala/intellijhocon/HoconTestUtils.scala
=======
import psi.HoconPsiFile
>>>>>>> 1fd4c19... HOCON: Java class reference detection on string literals:intellij-hocon/src/test/scala/intellijhocon/HoconTestUtils.scala

import scala.language.implicitConversions

object HoconTestUtils {
  def createPseudoPhysicalHoconFile(project: Project, text: String) = {
    val tempFile = project.getBaseDir + "temp.conf"
    val fileType = FileTypeManager.getInstance.getFileTypeByFileName(tempFile)
    PsiFileFactory.getInstance(project).createFileFromText(
      tempFile, fileType, text, LocalTimeCounter.currentTime(), true).asInstanceOf[HoconPsiFile]
  }

  implicit def asRunnable(code: => Any): Runnable =
    new Runnable {
      def run() = code
    }
}
