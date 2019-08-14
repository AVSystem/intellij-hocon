package org.jetbrains.plugins.hocon
package psi

import com.intellij.psi.search.FilenameIndex
import org.jetbrains.plugins.hocon.ref.IncludedFileReferenceSet
import org.jetbrains.plugins.hocon.ref.IncludedFileReferenceSet.DefaultContexts

import scala.annotation.tailrec

case class ResOpts(
  reverse: Boolean,
  mergeableOnly: Boolean = false
)

sealed abstract class ResolutionCtx {
  val toplevelCtx: ToplevelCtx = this match {
    case tc: ToplevelCtx => tc
    case rf: ResolvedField => rf.parentCtx.toplevelCtx
    case ic: IncludeCtx => ic.parentCtx.toplevelCtx
    case sc: SubstitutedCtx => sc.localCtx.toplevelCtx
  }

  @tailrec final def isAlreadyIn(file: HoconPsiFile): Boolean = this match {
    case toplevelCtx: ToplevelCtx =>
      toplevelCtx.file == file
    case resolvedField: ResolvedField =>
      resolvedField.parentCtx.isAlreadyIn(file)
    case atInclude: IncludeCtx =>
      atInclude.allFiles.contains(file) || atInclude.parentCtx.isAlreadyIn(file)
    case substitutedCtx: SubstitutedCtx =>
      substitutedCtx.localCtx.isAlreadyIn(file)
  }

  lazy val path: List[String] = {
    @tailrec def mkPath(suffix: List[String], ctx: ResolutionCtx): List[String] = ctx match {
      case _: ToplevelCtx => suffix
      case ic: IncludeCtx => mkPath(suffix, ic.parentCtx)
      case sc: SubstitutedCtx => mkPath(suffix, sc.backtracedCtx)
      case rf: ResolvedField => mkPath(rf.key :: suffix, rf.parentCtx)
    }
    mkPath(Nil, this)
  }

  /**
   * Occurrences of given key (or all) adjacent to this resolution context (outside of it, before or after).
   */
  def adjacentOccurrences(key: Option[String], opts: ResOpts): Iterator[ResolvedField] = this match {
    case _: ToplevelCtx =>
      Iterator.empty

    case ic: IncludeCtx =>
      ic.moreFileContexts(opts.reverse).flatMap(_.occurrences(key, opts)) ++ (ic.include match {
        case Some(inc) =>
          inc.adjacentEntriesOccurrences(key, opts, ic.parentCtx) ++
            ic.parentCtx.adjacentOccurrences(key, opts)
        case None if !opts.reverse =>
          // proceeding from last auto-included file into the contents of toplevel file itself
          ic.toplevelCtx.file.toplevelEntries.occurrences(key, opts, ic.toplevelCtx)
        case _ => Iterator.empty
      })

    case sc: SubstitutedCtx =>
      sc.substitution.adjacentConcatOccurrences(key, opts, sc.backtracedCtx) ++
        sc.backtracedCtx.adjacentOccurrences(key, opts)

    case rf: ResolvedField =>
      rf.moreOccurrences(opts).flatMap(_.subOccurrences(key, opts))
  }
}

case class OpenSubstitution(ctx: ResolutionCtx, subst: HSubstitution)

case class ToplevelCtx(
  file: HoconPsiFile,
  referenceFiles: Vector[HoconPsiFile],
  directOnly: Boolean = false,
  // indicates that we are resolving a substitution and points to it
  forSubst: Option[OpenSubstitution] = None
) extends ResolutionCtx {

  def referenceIncludeCtxs(reverse: Boolean): Iterator[IncludeCtx] =
    if (directOnly) Iterator.empty
    else IncludeCtx.allContexts(None, referenceFiles, reverse, this)

  def occurrences(subkey: Option[String], opts: ResOpts): Iterator[ResolvedField] = {
    def autoIncluded =
      referenceIncludeCtxs(opts.reverse).flatMap(_.occurrences(subkey, opts))
    def fromActualContents =
      file.toplevelEntries.occurrences(subkey, opts, this)
    if (opts.reverse) fromActualContents ++ autoIncluded
    else autoIncluded ++ fromActualContents
  }

  def occurrences(path: List[String], opts: ResOpts): Iterator[ResolvedField] = path match {
    case Nil => Iterator.empty
    case head :: tail =>
      val occurrencesOfFirst = occurrences(Some(head), opts)
      tail.foldLeft(occurrencesOfFirst) {
        case (occ, key) => occ.flatMap(_.subOccurrences(Some(key), opts))
      }
  }
}

object ToplevelCtx {
  final val ReferenceFile = "reference.conf" //TODO: configurable in project settings

  def referenceFilesFor(file: HoconPsiFile): Vector[HoconPsiFile] = {
    val DefaultContexts(scope, contexts) = IncludedFileReferenceSet.classpathDefaultContexts(file, "")
    val res = FilenameIndex.getFilesByName(file.getProject, ReferenceFile, scope)
      .iterator.collectOnly[HoconPsiFile].filter(f => contexts.contains(f.getParent))
      .toVector
    // return empty if the file itself is a reference file
    if (!res.contains(file)) res else Vector.empty
  }
}

case class ResolvedField(
  key: String,
  field: HKeyedField,
  parentCtx: ResolutionCtx,
) extends ResolutionCtx {

  def firstSubOccurrence(subkey: Option[String], opts: ResOpts): Option[ResolvedField] =
    subOccurrences(subkey, opts).nextOption

  def subOccurrences(subkey: Option[String], opts: ResOpts): Iterator[ResolvedField] = field match {
    case pf: HPrefixedField =>
      pf.subField.occurrences(subkey, opts, this)
    case vf: HValuedField =>
      vf.subScopeValue.flatMapIt(_.occurrences(subkey, opts, this))
  }

  def prefixField: Option[ResolvedField] = {
    @tailrec def loop(parentCtx: ResolutionCtx): Option[ResolvedField] = parentCtx match {
      case rf: ResolvedField => Some(rf)
      case sc: SubstitutedCtx => loop(sc.backtracedCtx)
      case ic: IncludeCtx => loop(ic.parentCtx)
      case _: ToplevelCtx => None
    }
    loop(parentCtx)
  }

  def moreOccurrences(opts: ResOpts): Iterator[ResolvedField] =
    field.adjacentEntriesOccurrences(key.opt, opts, parentCtx) ++
      parentCtx.adjacentOccurrences(key.opt, opts)

  def nextOccurrence(opts: ResOpts): Option[ResolvedField] =
    moreOccurrences(opts).nextOption
}

case class IncludeCtx(
  include: Option[HInclude], // None means auto-included file, e.g. reference.conf
  allFiles: Vector[HoconPsiFile],
  fileIdx: Int,
  parentCtx: ResolutionCtx
) extends ResolutionCtx {
  def file: HoconPsiFile = allFiles(fileIdx)

  def occurrences(key: Option[String], opts: ResOpts): Iterator[ResolvedField] =
    if (parentCtx.isAlreadyIn(file)) Iterator.empty
    else file.toplevelEntries.occurrences(key, opts, this)

  def firstOccurrence(key: Option[String], opts: ResOpts): Option[ResolvedField] =
    occurrences(key, opts).nextOption

  def moreFileContexts(reverse: Boolean): Iterator[IncludeCtx] = {
    val idxIt = if (reverse) Iterator.range(fileIdx - 1, -1, -1) else Iterator.range(fileIdx + 1, allFiles.size)
    idxIt.map(i => copy(fileIdx = i))
  }
}
object IncludeCtx {
  def allContexts(
    include: Option[HInclude],
    files: Vector[HoconPsiFile],
    reverse: Boolean,
    parentCtx: ResolutionCtx
  ): Iterator[IncludeCtx] =
    if (files.isEmpty) Iterator.empty else {
      val idxIt = if (reverse) files.indices.reverseIterator else files.indices.iterator
      idxIt.map(idx => IncludeCtx(include, files, idx, parentCtx))
    }
}

case class SubstitutedCtx(
  localCtx: ResolutionCtx, // context of local parent, without backtracing substitutions
  backtracedCtx: ResolutionCtx, // context of where the substitution itself is located
  substitution: HSubstitution
) extends ResolutionCtx
