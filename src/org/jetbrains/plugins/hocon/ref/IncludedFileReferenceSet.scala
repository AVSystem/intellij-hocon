package org.jetbrains.plugins.hocon
package ref

import java.{util => ju}

import com.intellij.openapi.project.Project
import com.intellij.openapi.roots._
import com.intellij.openapi.util.{Condition, TextRange}
import com.intellij.psi._
import com.intellij.psi.impl.source.resolve.reference.impl.providers.{FileReference, FileReferenceSet}
import com.intellij.psi.search.GlobalSearchScope
import org.jetbrains.plugins.hocon.HoconConstants._
import org.jetbrains.plugins.hocon.ref.IncludedFileReferenceSet._

/**
 * FileReferenceSet subclass that tries to simulate how Typesafe Config handles includes with its
 * default includer implementation, <tt>com.typesafe.config.impl.SimpleIncluder</tt> -
 * as much as this is possible without access to actual runtime.
 * <p/>
 * This implementation will only try to resolve includes with <tt>classpath(...)</tt>, <tt>file(...)</tt>
 * or no qualifier - that is, <tt>url(...)</tt> is not supported since it can only be understood at runtime.
 * Also, for heuristic includes (no qualifier) in source, resource or library files, it is assumed that including file
 * was loaded from classpath resource and thus, included path will be interpreted as classpath resource relative to
 * current file. If including file is neither in sources or library, heuristic include will only be resolved as
 * if the including file was loaded with <tt>file(..)</tt> qualifier. Module content root is assumed as the working
 * directory for resolving absolute file includes.
 * <p/>
 * Files to include will be searched for in classpath of including file's containing module or - when including file
 * is in a library - joined classpath of all modules that directly depend on that library. Test sources and dependencies
 * are searched only when including file is also a test source or lies in a test dependency.
 * <p/>
 * Just like Typesafe Config, this implementation will try to guess extension of included resource to be either
 * <tt>.conf</tt>, <tt>.json</tt> or <tt>.properties</tt>. It is impossible to include a file with any other extension.
 * This constraint is also reflected by appropriate completion filter.
 * <p/>
 * If a reference resolves to multiple files, they will be sorted so that .conf files come first, .json files after
 * them and .properties files at the end. This reflects the order in which Typesafe Config merges those files.
 */
object IncludedFileReferenceSet {
  case class DefaultContexts(
    scope: GlobalSearchScope,
    contexts: ju.Collection[PsiFileSystemItem]
  )

  def classpathScope(context: PsiFile): GlobalSearchScope = {
    val proj = context.getProject
    val vfile = context.getOriginalFile.getVirtualFile
    if (vfile == null) return GlobalSearchScope.EMPTY_SCOPE

    val parent = vfile.getParent
    if (parent == null) return GlobalSearchScope.EMPTY_SCOPE

    val pfi = ProjectRootManager.getInstance(proj).getFileIndex

    // If including file is in a module source, add classpath of that module
    // If including file is in a library, add classpath of all modules which depend directly on that library
    // in order to include all possible dependencies of that library
    val allScopes = pfi.getOrderEntriesForFile(parent).iterator.asScala.collect {
      case msoe: ModuleSourceOrderEntry =>
        msoe.getOwnerModule.getModuleRuntimeScope(pfi.isInTestSourceContent(parent))
      case loe: LibraryOrderEntry =>
        loe.getOwnerModule.getModuleRuntimeScope(loe.getScope == DependencyScope.TEST)
    }

    def orderEntryScope = allScopes.reduceOption(_ union _)
    def moduleScope = pfi.getModuleForFile(parent).opt.map(_.getModuleRuntimeScope(false))

    orderEntryScope orElse moduleScope getOrElse GlobalSearchScope.EMPTY_SCOPE
  }

  def classpathPackageDirs(project: Project, scope: GlobalSearchScope, pkgName: String): List[PsiFileSystemItem] = {
    val psiManager = PsiManager.getInstance(project)
    PackageIndex.getInstance(project).getDirectoriesByPackageName(pkgName, false).iterator
      .filter(scope.contains).flatMap(dir => Option(psiManager.findDirectory(dir)))
      .toList
  }
}

class IncludedFileReferenceSet(
  text: String,
  element: PsiElement,
  forcedAbsolute: Boolean,
  fromClasspath: Boolean,
  scope: GlobalSearchScope
) extends FileReferenceSet(text, element, 1, null, true) {

  setEmptyPathAllowed(false)

  override def isAbsolutePathReference: Boolean =
    forcedAbsolute || super.isAbsolutePathReference

  override def couldBeConvertedTo(relative: Boolean): Boolean =
    if (relative) !forcedAbsolute
    else fromClasspath

  override def createFileReference(range: TextRange, index: Int, text: String): FileReference =
    new IncludedFileReference(this, range, index, text)

  override def getReferenceCompletionFilter: Condition[PsiFileSystemItem] =
    (item: PsiFileSystemItem) => item.isDirectory ||
      item.getName.endsWith(ConfExt) || item.getName.endsWith(JsonExt) || item.getName.endsWith(PropsExt)

  // code mostly based on similar bits in `FileReferenceSet` and `PsiFileReferenceHelper`
  override def computeDefaultContexts: ju.Collection[PsiFileSystemItem] = {
    val empty = ju.Collections.emptyList[PsiFileSystemItem]

    def single(fsi: PsiFileSystemItem) = ju.Collections.singletonList(fsi)

    val cf = getContainingFile
    if (cf == null) return empty

    val containingFile = Option(cf.getContext).map(_.getContainingFile).getOrElse(cf)

    val proj = containingFile.getProject
    val vfile = containingFile.getOriginalFile.getVirtualFile
    if (vfile == null) return empty

    val parent = vfile.getParent
    if (parent == null) return empty

    if (fromClasspath) {
      val pfi = ProjectRootManager.getInstance(proj).getFileIndex
      val pkgName =
        if (isAbsolutePathReference) ""
        else pfi.getPackageNameByDirectory(parent)

      if (pkgName != null) classpathPackageDirs(proj, scope, pkgName).toJList else empty
    } else if (isAbsolutePathReference) {
      FileReferenceSet.getAbsoluteTopLevelDirLocations(containingFile)
    } else {
      Option(PsiManager.getInstance(proj).findDirectory(parent)).map(single).getOrElse(empty)
    }
  }
}

object IncludedFileReference {
  final val ResolveResultOrdering = Ordering.by { rr: ResolveResult =>
    rr.getElement match {
      case file: PsiFile =>
        val name = file.getName
        if (name.endsWith(ConfExt)) 0
        else if (name.endsWith(JsonExt)) 1
        else if (name.endsWith(PropsExt)) 2
        else 3
      case _ =>
        3
    }
  }
}

class IncludedFileReference(refSet: FileReferenceSet, range: TextRange, index: Int, text: String)
  extends FileReference(refSet, range, index, text) {

  private def lacksExtension(text: String) =
    isLast && text.nonEmpty && text != "." && text != ".." && text != "/" &&
      !text.endsWith(ConfExt) && !text.endsWith(JsonExt) && !text.endsWith(PropsExt)

  override def innerResolve(caseSensitive: Boolean, containingFile: PsiFile): Array[ResolveResult] = {
    val result = super.innerResolve(caseSensitive, containingFile)

    // Sort the files so that .conf files come first, .json files after then and .properties files at the end
    // This is to mimic Typesafe Config which merges included files in exactly that order.
    ju.Arrays.sort(result, IncludedFileReference.ResolveResultOrdering)
    result
  }

  override def innerResolveInContext(
    text: String,
    context: PsiFileSystemItem,
    result: ju.Collection[ResolveResult],
    caseSensitive: Boolean
  ): Unit =
    if (lacksExtension(text)) {
      def resolveWithExt(ext: String): Unit =
        super.innerResolveInContext(text + ext, context, result, caseSensitive)

      resolveWithExt(ConfExt)
      resolveWithExt(JsonExt)
      resolveWithExt(PropsExt)
    } else
      super.innerResolveInContext(text, context, result, caseSensitive)

  override def getFileNameToCreate: String =
    if (lacksExtension(getCanonicalText))
      super.getFileNameToCreate + ConfExt
    else
      super.getFileNameToCreate
}
