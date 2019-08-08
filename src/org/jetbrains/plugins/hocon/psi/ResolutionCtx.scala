package org.jetbrains.plugins.hocon
package psi

import scala.annotation.tailrec

sealed trait ResolutionCtx {
  def toplevelCtx: ToplevelCtx

  @tailrec final def asField: Option[ResolvedField] = this match {
    case rf: ResolvedField => Some(rf)
    case ic: IncludeCtx => ic.parentCtx.asField
    case _: ToplevelCtx => None
  }

  @tailrec final def isAlreadyIn(file: HoconPsiFile): Boolean = this match {
    case toplevelCtx: ToplevelCtx =>
      toplevelCtx.file == file
    case resolvedField: ResolvedField =>
      resolvedField.parentCtx.isAlreadyIn(file)
    case atInclude: IncludeCtx =>
      atInclude.allIncludeFiles.contains(file) || atInclude.parentCtx.isAlreadyIn(file)
  }
}

case class AtSubstitution(ctx: ResolutionCtx, subst: HSubstitution)

case class ToplevelCtx(
  file: HoconPsiFile,
  directOnly: Boolean = false,
  // indicates that we are resolving a substitution and points to it
  forSubst: Option[AtSubstitution] = None
) extends ResolutionCtx {
  def toplevelCtx: ToplevelCtx = this
}

case class ResolvedField(
  field: HKeyedField,
  parentCtx: ResolutionCtx,
  // indicates that this field is a result of a substitution resolution and points to that substitution
  forSubst: Option[AtSubstitution] = None
) extends ResolutionCtx {
  val toplevelCtx: ToplevelCtx = parentCtx.toplevelCtx

  def subOccurrences(key: String, reverse: Boolean): Iterator[ResolvedField] = field match {
    case pf: HPrefixedField =>
      val subField = pf.subField
      if (subField.hasKeyValue(key))
        Iterator(ResolvedField(subField, this))
      else Iterator.empty
    case vf: HValuedField =>
      vf.subScopeValue.fold[Iterator[ResolvedField]](Iterator.empty)(_.occurrences(key, reverse, this))
  }

  def previousField: Option[ResolvedField] =
    forSubst.flatMap(_.ctx.asField) orElse parentCtx.asField
}

case class IncludeCtx(
  include: HInclude,
  file: HoconPsiFile,
  allIncludeFiles: List[HoconPsiFile], // all files included by this HInclude, for cycle detection
  parentCtx: ResolutionCtx
) extends ResolutionCtx {
  val toplevelCtx: ToplevelCtx = parentCtx.toplevelCtx
}
