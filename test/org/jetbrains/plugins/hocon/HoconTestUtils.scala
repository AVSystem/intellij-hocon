package org.jetbrains.plugins.hocon

import java.io.File

import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.util.Computable

import scala.annotation.tailrec

trait HoconTestUtils {
  def testdataPath: String = HoconTestUtils.TestdataPath

  def inWriteAction[T](body: => T): T =
    ApplicationManager.getApplication match {
      case application if application.isWriteAccessAllowed => body
      case application =>
        val computable: Computable[T] = () => body
        application.runWriteAction(computable)
    }

}
object HoconTestUtils {
  final lazy val TestdataPath = {
    @tailrec def find(dir: File): File =
      if (dir == null)
        throw new RuntimeException("testdata dir not found")
      else {
        val td = new File(dir, "testdata")
        if (td.exists()) td
        else find(dir.getParentFile)
      }
    find(new File(".")).getAbsolutePath
  }
}
