package org.jetbrains.plugins.hocon
package ref

import com.intellij.openapi.extensions.ExtensionPointName
import com.intellij.openapi.project.Project
import com.intellij.openapi.roots.PackageIndex
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.psi.search.GlobalSearchScope
import com.intellij.psi.{PsiFileSystemItem, PsiManager}

object PackageDirsEnumerator {
  final val EpName: ExtensionPointName[PackageDirsEnumerator] =
    ExtensionPointName.create("org.jetbrains.plugins.hocon.packageDirsEnumerator")
}
abstract class PackageDirsEnumerator {
  def packageNameByDirectory(project: Project, dir: VirtualFile): Option[String]
  def classpathPackageDirs(project: Project, scope: GlobalSearchScope, pkgName: String): List[PsiFileSystemItem]
}

final class JavaPackageDirsEnumerator extends PackageDirsEnumerator {
  def classpathPackageDirs(project: Project, scope: GlobalSearchScope, pkgName: String): List[PsiFileSystemItem] = {
    val psiManager = PsiManager.getInstance(project)
    PackageIndex.getInstance(project).getDirectoriesByPackageName(pkgName, false).iterator
      .filter(scope.contains).flatMap(dir => Option(psiManager.findDirectory(dir)))
      .toList
  }

  def packageNameByDirectory(project: Project, dir: VirtualFile): Option[String] =
    PackageIndex.getInstance(project).getPackageNameByDirectory(dir).opt
}
