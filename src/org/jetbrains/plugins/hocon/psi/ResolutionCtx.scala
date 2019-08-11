package org.jetbrains.plugins.hocon
package psi

import scala.annotation.tailrec

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
}

case class OpenSubstitution(ctx: ResolutionCtx, subst: HSubstitution)

case class ToplevelCtx(
  file: HoconPsiFile,
  entries: HObjectEntries,
  directOnly: Boolean = false,
  // indicates that we are resolving a substitution and points to it
  forSubst: Option[OpenSubstitution] = None
) extends ResolutionCtx

case class ResolvedField(
  key: String,
  field: HKeyedField,
  parentCtx: ResolutionCtx,
) extends ResolutionCtx {
  def subOccurrences(key: String, reverse: Boolean): Iterator[ResolvedField] = field match {
    case pf: HPrefixedField =>
      val subField = pf.subField
      if (subField.hasKeyValue(key))
        Iterator(ResolvedField(key, subField, this))
      else Iterator.empty
    case vf: HValuedField =>
      vf.subScopeValue.fold[Iterator[ResolvedField]](Iterator.empty)(_.occurrences(key, reverse, this))
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

  def moreOccurrences(reverse: Boolean): Iterator[ResolvedField] = {
    def moreFrom(
      entry: HEntriesLike, parentCtx: ResolutionCtx, acc: Iterator[ResolvedField]
    ): Iterator[ResolvedField] = parentCtx match {
      case sc: SubstitutedCtx =>
        val insideSubstitution = moreFrom(field, sc.localCtx, acc)
        sc.backtracedCtx match {
          case parentField: ResolvedField =>
            fromParentField(parentField, withinConcat(sc.substitution, parentField, insideSubstitution))
          case _ =>
            insideSubstitution
        }

      case ic: IncludeCtx =>
        val locals = withinEntries(field, parentCtx, acc)
        val otherFilesCtxs = if (reverse) ic.prevFileContexts else ic.nextFileContexts
        val fromOtherFiles = otherFilesCtxs.flatMap(_.occurrences(key, reverse))
        moreFrom(ic.include, ic.parentCtx, locals ++ fromOtherFiles)

      case parentField: ResolvedField =>
        val locals = withinEntries(field, parentCtx, acc)
        val localsWithConcat = field.parentOfType[HObjectField].flatMap(_.containingObject)
          .fold(locals)(obj => withinConcat(obj, parentCtx, locals))
        fromParentField(parentField, localsWithConcat)

      case _: ToplevelCtx =>
        withinEntries(field, parentCtx, acc)
    }

    def fromParentField(parentField: ResolvedField, acc: Iterator[ResolvedField]): Iterator[ResolvedField] =
      acc ++ parentField.moreOccurrences(reverse).flatMap(_.subOccurrences(key, reverse))

    def withinEntries(entry: HEntriesLike, parentCtx: ResolutionCtx, acc: Iterator[ResolvedField]): Iterator[ResolvedField] =
      acc ++ entry.moreEntries(reverse).flatMap(_.occurrences(key, reverse, parentCtx))

    def withinConcat(value: HValue, parentCtx: ResolutionCtx, acc: Iterator[ResolvedField]): Iterator[ResolvedField] =
      acc ++ value.moreConcatenated(reverse).flatMap(_.occurrences(key, reverse, parentCtx))

    moreFrom(field, parentCtx, Iterator.empty)
  }
}

case class IncludeCtx(
  include: HInclude,
  allFiles: Vector[HoconPsiFile],
  fileIdx: Int,
  parentCtx: ResolutionCtx
) extends ResolutionCtx {
  def file: HoconPsiFile = allFiles(fileIdx)

  def occurrences(key: String, reverse: Boolean): Iterator[ResolvedField] =
    file.toplevelEntries.occurrences(key, reverse, this)

  def prevFileContexts: Iterator[IncludeCtx] =
    Iterator.range(fileIdx - 1, -1, -1).map(prevIdx => copy(fileIdx = prevIdx))

  def nextFileContexts: Iterator[IncludeCtx] =
    Iterator.range(fileIdx + 1, allFiles.size).map(nextIdx => copy(fileIdx = nextIdx))
}

case class SubstitutedCtx(
  localCtx: ResolutionCtx, // context of local parent, without backtracing substitutions
  backtracedCtx: ResolutionCtx, // context of where the substitution itself is located
  substitution: HSubstitution
) extends ResolutionCtx
