package org.jetbrains.plugins.hocon
package ref

import com.intellij.openapi.module.ModuleManager
import com.intellij.openapi.roots.ModuleRootManager
import com.intellij.openapi.util.TextRange
import com.intellij.psi.*
import com.intellij.util.ProcessingContext
import org.jetbrains.jps.model.java.JavaResourceRootType
import org.jetbrains.plugins.hocon.psi.HStringValue
import org.jetbrains.plugins.hocon.settings.HoconProjectSettings

class HoconFilePathReferenceProvider extends PsiReferenceProvider {
  def getReferencesByElement(element: PsiElement, context: ProcessingContext): Array[PsiReference] =
    element match {
      case hstr: HStringValue =>
        val project = element.getProject
        val settings = HoconProjectSettings.getInstance(project)
        val extensions = settings.fileNavigationExtensionsList
        if (extensions.isEmpty) PsiReference.EMPTY_ARRAY
        else {
          val filePath = hstr.stringValue
          if (!extensions.exists(ext => filePath.endsWith("." + ext))) PsiReference.EMPTY_ARRAY
          else {
            val range = ElementManipulators.getValueTextRange(element)
            Array(new HoconFilePathReference(filePath, element, range, settings))
          }
        }
      case _ => PsiReference.EMPTY_ARRAY
    }
}

class HoconFilePathReference(
  filePath: String,
  element: PsiElement,
  range: TextRange,
  settings: HoconProjectSettings,
) extends PsiReferenceBase[PsiElement](element, range) {

  def resolve(): PsiElement = {
    val project = element.getProject
    val searchRoots = settings.fileNavigationSearchRootsList
    val psiManager = PsiManager.getInstance(project)
    val modules = ModuleManager.getInstance(project).getModules

    val result = modules.iterator.flatMap { module =>
      val mrm = ModuleRootManager.getInstance(module)

      val baseDirs = if (searchRoots.nonEmpty) {
        // Use explicitly configured roots (relative paths from content roots)
        mrm.getContentRoots.iterator.flatMap { contentRoot =>
          searchRoots.iterator.flatMap { searchRoot =>
            val dir = if (searchRoot.isEmpty) contentRoot else contentRoot.findFileByRelativePath(searchRoot)
            Option(dir).iterator
          }
        }
      } else {
        // Default: use resource root resolved from the project model (Compile / resourceDirectory)
        mrm.getContentEntries.iterator.flatMap { entry =>
          entry.getSourceFolders.iterator
            .filter(_.getRootType == JavaResourceRootType.RESOURCE)
            .flatMap(sf => Option(sf.getFile).iterator)
        }
      }

      baseDirs
        .flatMap(dir => Option(dir.findFileByRelativePath(filePath)))
        .flatMap(vf => Option(psiManager.findFile(vf)))
    }

    result.nextOption().orNull
  }

  override def getVariants: Array[AnyRef] = Array.empty

  override def isSoft: Boolean = true
}
