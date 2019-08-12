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

  def firstSubOccurrence(key: String, reverse: Boolean): Option[ResolvedField] =
    subOccurrences(key, reverse).nextOption

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

  def moreOccurrences(reverse: Boolean): Iterator[ResolvedField] =
    Iterator.iterate(nextOccurrence(reverse).orNull)(_.nextOccurrence(reverse).orNull)
      .takeWhile(_ != null)

  def nextOccurrence(reverse: Boolean): Option[ResolvedField] = {
    def nextFrom(entry: HEntriesLike, parentCtx: ResolutionCtx): Option[ResolvedField] = parentCtx match {
      case sc: SubstitutedCtx =>
        nextFrom(field, sc.localCtx) orElse
          sc.backtracedCtx.opt.collectOnly[ResolvedField].flatMap { parentField: ResolvedField =>
            withinConcat(sc.substitution, parentField) orElse fromParentField(parentField)
          }

      case ic: IncludeCtx =>
        withinEntries(field, parentCtx) orElse
          ic.moreFileContexts(reverse).flatMap(_.firstOccurrence(key, reverse)).nextOption orElse
          nextFrom(ic.include, ic.parentCtx)

      case parentField: ResolvedField =>
        withinEntries(field, parentCtx) orElse
          field.parentOfType[HObjectField].flatMap(_.containingObject).flatMap(withinConcat(_, parentCtx)) orElse
          fromParentField(parentField)

      case _: ToplevelCtx =>
        withinEntries(field, parentCtx)
    }

    def fromParentField(parentField: ResolvedField): Option[ResolvedField] =
      parentField.moreOccurrences(reverse).flatMap(_.firstSubOccurrence(key, reverse)).nextOption

    def withinEntries(entry: HEntriesLike, parentCtx: ResolutionCtx): Option[ResolvedField] =
      entry.moreEntries(reverse).flatMap(_.firstOccurrence(key, reverse, parentCtx)).nextOption

    def withinConcat(value: HValue, parentCtx: ResolutionCtx): Option[ResolvedField] =
      value.moreConcatenated(reverse).flatMap(_.firstOccurrence(key, reverse, parentCtx)).nextOption

    nextFrom(field, parentCtx)
  }
}

case class IncludeCtx(
  include: HInclude,
  allFiles: Vector[HoconPsiFile],
  fileIdx: Int,
  parentCtx: ResolutionCtx
) extends ResolutionCtx {
  def file: HoconPsiFile = allFiles(fileIdx)

  def firstOccurrence(key: String, reverse: Boolean): Option[ResolvedField] =
    file.toplevelEntries.firstOccurrence(key, reverse, this)

  def moreFileContexts(reverse: Boolean): Iterator[IncludeCtx] = {
    val idxIt = if (reverse) Iterator.range(fileIdx - 1, -1, -1) else Iterator.range(fileIdx + 1, allFiles.size)
    idxIt.map(i => copy(fileIdx = i))
  }
}

case class SubstitutedCtx(
  localCtx: ResolutionCtx, // context of local parent, without backtracing substitutions
  backtracedCtx: ResolutionCtx, // context of where the substitution itself is located
  substitution: HSubstitution
) extends ResolutionCtx
