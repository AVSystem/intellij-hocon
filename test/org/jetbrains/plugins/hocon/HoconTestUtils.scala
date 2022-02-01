package org.jetbrains.plugins.hocon

import java.io.File
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.Computable
import com.intellij.openapi.vfs.{LocalFileSystem, VirtualFile}
import com.intellij.psi.{PsiFile, PsiManager}
import org.jetbrains.plugins.hocon.psi.HoconPsiFile

import scala.annotation.{nowarn, tailrec}

trait HoconTestUtils {
  def testdataPath: String = HoconTestUtils.TestdataPath

  def rootPath: String

  def pathOf(file: PsiFile): String =
    file.getVirtualFile.getPath.stripPrefix(contentRoot.getPath).stripPrefix("/")

  lazy val contentRoot: VirtualFile = {
    val lfs = LocalFileSystem.getInstance()
    lfs.refresh(false)
    lfs.findFileByPath(rootPath).opt
      .getOrElse(throw new IllegalArgumentException(s"root path not found: $rootPath"))
  }

  def findVirtualFile(path: String): VirtualFile =
    contentRoot.findFileByRelativePath(path).opt
      .getOrElse(throw new IllegalArgumentException(s"file not found: $path"))

  def findFile(path: String, project: Project): PsiFile =
    PsiManager.getInstance(project).findFile(findVirtualFile(path))

  def findHoconFile(path: String, project: Project): HoconPsiFile =
    findFile(path, project) match {
      case file: HoconPsiFile => file
      case _ => throw new IllegalArgumentException(s"not a HOCON file: $path")
    }

  def inWriteAction[T](body: => T): T =
    ApplicationManager.getApplication match {
      case application if application.isWriteAccessAllowed => body
      case application =>
        @nowarn("msg=deprecated")
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
