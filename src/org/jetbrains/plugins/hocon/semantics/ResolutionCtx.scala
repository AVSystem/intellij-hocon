package org.jetbrains.plugins.hocon
package semantics

import com.intellij.openapi.util.TextRange
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.psi.PsiElement
import com.intellij.psi.search.{FilenameIndex, GlobalSearchScope}
import org.jetbrains.plugins.hocon.psi._
import org.jetbrains.plugins.hocon.ref.IncludedFileReferenceSet
import org.jetbrains.plugins.hocon.semantics.SubstitutionKind.SelfReferential

import scala.annotation.tailrec

case class ResOpts(
  reverse: Boolean,
  resolveIncludes: Boolean = true,
  resolveSubstitutions: Boolean = true
)

sealed abstract class SubstitutionKind
object SubstitutionKind {
  case class Full(path: List[String], fallback: Option[SubstitutionKind]) extends SubstitutionKind
  case class SelfReferential(path: List[String], selfReferenced: ResolvedField) extends SubstitutionKind
  case object Circular extends SubstitutionKind
  case object Invalid extends SubstitutionKind
}

case class SubstitutionCtx(
  ctx: ResolutionCtx,
  subst: HSubstitution,
  subsKind: SubstitutionKind
)

sealed abstract class ResolutionCtx {
  val toplevelCtx: ToplevelCtx = this match {
    case tc: ToplevelCtx => tc
    case rf: ResolvedField => rf.parentCtx.toplevelCtx
    case ic: IncludeCtx => ic.parentCtx.toplevelCtx
    case ac: ArrayCtx => ac.parentCtx.toplevelCtx
  }

  val depth: Int = this match {
    case _: ToplevelCtx => 0
    case rf: ResolvedField => rf.parentCtx.depth + 1
    case ic: IncludeCtx => ic.parentCtx.depth
    case ac: ArrayCtx => ac.parentCtx.depth
  }

  val inArray: Boolean = this match {
    case _: ToplevelCtx => false
    case rf: ResolvedField => rf.parentCtx.inArray
    case ic: IncludeCtx => ic.parentCtx.inArray
    case _: ArrayCtx => true
  }

  val lastInclude: Option[IncludeCtx] = this match {
    case _: ToplevelCtx => None
    case rf: ResolvedField => rf.parentCtx.lastInclude
    case ic: IncludeCtx => Some(ic)
    case ac: ArrayCtx => ac.parentCtx.lastInclude
  }

  private def textRangeTuple(file: HoconPsiFile, tr: TextRange): (VirtualFile, Int, Int) =
    (file.getVirtualFile, tr.getStartOffset, tr.getEndOffset)

  private lazy val localTextRange = this match {
    case _: ToplevelCtx => (null, 0, 0)
    case rf: ResolvedField => textRangeTuple(rf.field.hoconFile, rf.field.getTextRange)
    case ic: IncludeCtx =>
      ic.source match {
        case IncludeSource.Element(inc) => textRangeTuple(inc.hoconFile, inc.getTextRange)
        case _ => (ic.file.getVirtualFile, 0, ic.file.getTextLength)
      }
    case ac: ArrayCtx => textRangeTuple(ac.value.hoconFile, ac.value.getTextRange)
  }

  def localStartOffset: Int = localTextRange._2
  def localEndOffset: Int = localTextRange._3

  // ignores substitutions!
  final def sameAs(other: ResolutionCtx): Boolean =
    (this eq other) ||
      depth == other.depth && localTextRange == other.localTextRange && ((lastInclude, other.lastInclude) match {
        case (Some(inc1), Some(inc2)) => inc1.parentCtx.sameAs(inc2.parentCtx)
        case (None, None) => true
        case _ => false
      })

  // https://github.com/lightbend/config/blob/master/HOCON.md#include-semantics-substitution
  def fixupSubstitutionPath(path: List[String]): List[String] = {
    @tailrec def loop(ctx: ResolutionCtx, suffix: List[String]): List[String] = ctx match {
      case _: ToplevelCtx => suffix
      case rf: ResolvedField => loop(rf.parentCtx, rf.key :: suffix)
      case ic: IncludeCtx => loop(ic.parentCtx, suffix)
      case _: ArrayCtx => path
    }
    lastInclude.filterNot(_.inArray).map(loop(_, path)).getOrElse(path)
  }

  @tailrec final def isAlreadyIn(file: HoconPsiFile): Boolean = lastInclude match {
    case Some(ic) => ic.allFiles.contains(file) || ic.parentCtx.isAlreadyIn(file)
    case None => false
  }

  private def pathsInResolution(
    suffix: List[ResolvedField], suffixStack: List[List[ResolvedField]]
  ): List[List[ResolvedField]] = this match {
    case tc: ToplevelCtx =>
      val fromOpenSubstitutions = tc.subsCtx.fold(Nil: List[List[ResolvedField]]) { sc =>
        val (newSuffix, newSuffixStack) = suffixStack match {
          case h :: t => (h, t)
          case Nil => (Nil, Nil)
        }
        sc.ctx.pathsInResolution(newSuffix, newSuffixStack)
      }

      if (suffix.nonEmpty)
        suffix :: fromOpenSubstitutions
      else fromOpenSubstitutions

    case rf: ResolvedField =>
      // tricky! if this is a substitution result, push current suffix to suffix stack,
      // it will be handled ultimately by ToplevelCtx case
      val newSuffixStack = rf.subsCtx.fold(suffixStack)(_ => suffix :: suffixStack)
      rf.parentCtx.pathsInResolution(rf :: suffix, newSuffixStack)

    case ic: IncludeCtx =>
      ic.parentCtx.pathsInResolution(suffix, suffixStack)

    case ac: ArrayCtx =>
      ac.parentCtx.pathsInResolution
  }

  /**
   * All currently "open" paths - used for detecting self-referential and circular substitutions.
   */
  lazy val pathsInResolution: List[List[ResolvedField]] =
    pathsInResolution(Nil, Nil)

  /**
   * Occurrences of given key (or all) adjacent to this resolution context (outside of it, before or after).
   * Example: sibling fields of an `include`.
   */
  def adjacentOccurrences(key: Option[String], opts: ResOpts): Iterator[ResolvedField] = this match {
    case _: ToplevelCtx =>
      Iterator.empty

    case rf: ResolvedField => rf.subsCtx match {
      case Some(sc) =>
        rf.moreOccurrences(opts, withBacktraced = false).flatMap(_.subOccurrences(key, opts)) ++
          sc.subst.adjacentConcatOccurrences(key, opts, sc.ctx) ++
          sc.ctx.adjacentOccurrences(key, opts)

      case None =>
        rf.moreOccurrences(opts).flatMap(_.subOccurrences(key, opts))
    }

    case ic: IncludeCtx =>
      ic.moreFileContexts(opts.reverse).flatMap(_.occurrences(key, opts)) ++ (ic.source match {
        case IncludeSource.Element(inc) =>
          inc.adjacentEntriesOccurrences(key, opts, ic.parentCtx) ++
            ic.parentCtx.adjacentOccurrences(key, opts)
        case _ =>
          Iterator.empty
      })

    case _: ArrayCtx =>
      Iterator.empty
  }

  def trace: String = {
    def loop(ic: Option[IncludeCtx], suffix: String): String =
      ic.fold(suffix) { incCtx =>
        incCtx.source match {
          case IncludeSource.Element(inc) =>
            loop(incCtx.parentCtx.lastInclude, s"${inc.pos}->$suffix")
          case _ => suffix
        }
      }
    loop(lastInclude, this match {
      case tc: ToplevelCtx => s"<toplevel:${tc.context.pos}>"
      case ic: IncludeCtx => ic.file.getName
      case rf: ResolvedField => rf.field.pos
      case ac: ArrayCtx => ac.value.pos
    })
  }
}

case class ToplevelCtx(
  context: PsiElement,
  scope: GlobalSearchScope,
  files: Vector[HoconPsiFile],
  // indicates that we are resolving a substitution and points to it
  subsCtx: Option[SubstitutionCtx] = None
) extends ResolutionCtx {

  def toplevelIncludes(reverse: Boolean): Iterator[IncludeCtx] =
    IncludeCtx.allContexts(IncludeSource.ToplevelFile(this), files, reverse, this)

  def selfReferenced: Option[ResolvedField] =
    subsCtx.map(_.subsKind).collectOnly[SelfReferential].map(_.selfReferenced)

  def occurrences(subkey: Option[String], opts: ResOpts): Iterator[ResolvedField] =
    toplevelIncludes(opts.reverse).flatMap(_.occurrences(subkey, opts))

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
  //TODO: configurable in project settings
  final val ReferenceResource = "reference.conf"
  final val ApplicationResource = "application.conf"

  def resolveResource(scope: GlobalSearchScope, resource: String): Vector[HoconPsiFile] = {
    val rootDirs = IncludedFileReferenceSet.classpathPackageDirs(scope, "")
    FilenameIndex.getFilesByName(scope.getProject, resource, scope)
      .iterator.collectOnly[HoconPsiFile].filter(f => rootDirs.contains(f.getParent))
      .toVector.sortBy(_.getVirtualFile.getPath)
  }

  def apply(file: HoconPsiFile): ToplevelCtx = {
    val scope = IncludedFileReferenceSet.classpathScope(file)
    val referenceFiles = resolveResource(scope, ReferenceResource)
    val files = if (referenceFiles.contains(file)) referenceFiles else referenceFiles :+ file
    ToplevelCtx(file, scope, files)
  }

  def apply(context: PsiElement, resource: String): ToplevelCtx = {
    val scope = IncludedFileReferenceSet.classpathScope(context.getContainingFile)
    val referenceFiles =
      if (resource == ReferenceResource) Vector.empty
      else resolveResource(scope, ReferenceResource)
    ToplevelCtx(context, scope, referenceFiles ++ resolveResource(scope, resource))
  }
}

case class ResolvedField(
  key: String,
  field: HKeyedField,
  parentCtx: ResolutionCtx,
  subsCtx: Option[SubstitutionCtx] = None // indicates that this field is a result of substitution resolution
) extends ResolutionCtx {

  def hkey: HFieldKey =
    field.key.getOrElse(throw new IllegalStateException("ResolvedField must have a key"))

  def backtracedField: Option[ResolvedField] =
    subsCtx.map(_.ctx).collectOnly[ResolvedField]

  def fullyBacktraced: ResolvedField =
    backtracedField.fold(this)(_.fullyBacktraced)

  def firstSubOccurrence(subkey: Option[String], opts: ResOpts): Option[ResolvedField] =
    subOccurrences(subkey, opts).nextOption

  def subOccurrences(subkey: Option[String], opts: ResOpts): Iterator[ResolvedField] = field match {
    case pf: HPrefixedField =>
      pf.subField.occurrences(subkey, opts, this)
    case vf: HValuedField =>
      vf.subScopeValue.flatMapIt(_.occurrences(subkey, opts, this))
  }

  def subOccurrences(path: List[String], opts: ResOpts): Iterator[ResolvedField] =
    path.foldLeft(Iterator(this)) {
      case (it, key) => it.flatMap(_.subOccurrences(key.opt, opts))
    }

  val prefixField: Option[ResolvedField] = {
    @tailrec def loop(parentCtx: ResolutionCtx): Option[ResolvedField] =
      parentCtx match {
        case rf: ResolvedField => Some(rf)
        case ic: IncludeCtx => loop(ic.parentCtx)
        case _: ToplevelCtx | _: ArrayCtx => None
      }
    loop(parentCtx)
  }

  def backtracedPrefixField: Option[ResolvedField] =
    fullyBacktraced.prefixField

  @tailrec final def ancestorField(levels: Int, backtraced: Boolean = true): Option[ResolvedField] =
    if (levels == 0) Some(this)
    else {
      val prefix = if (backtraced) backtracedPrefixField else prefixField
      prefix match {
        case Some(pf) => pf.ancestorField(levels - 1)
        case None => None
      }
    }

  @tailrec private def path(suffix: List[String], backtraced: Boolean): List[String] = {
    val self = if (backtraced) fullyBacktraced else this
    val newSuffix = self.key :: suffix
    self.prefixField match {
      case None => newSuffix
      case Some(pf) => pf.path(newSuffix, backtraced)
    }
  }

  lazy val path: List[String] = path(Nil, backtraced = false)
  lazy val backtracedPath: List[String] = path(Nil, backtraced = true)

  lazy val includeChain: List[IncludeCtx] = {
    @tailrec def loop(rf: ResolutionCtx, suffix: List[IncludeCtx]): List[IncludeCtx] = rf.lastInclude match {
      case Some(ic) => loop(ic.parentCtx, ic :: suffix)
      case None => suffix
    }
    loop(this, Nil)
  }

  @tailrec final def samePathAs(other: ResolvedField): Boolean =
    sameAs(other) ||
      depth == other.depth && key == other.key && ((prefixField, other.prefixField) match {
        case (Some(pf), Some(opf)) => pf.samePathAs(opf)
        case (None, None) => true
        case _ => false
      })

  /**
   * Checks whether a [[ResolvedField]] occurs before another [[ResolvedField]].
   * Includes are taken into account, i.e. you can imagine comparing positions in a document with all the included
   * files inlined.
   */
  def isBefore(other: ResolvedField): Boolean = {
    def comparePos(lhs: ResolutionCtx, rhs: ResolutionCtx): Int =
      if (lhs.localStartOffset < rhs.localStartOffset && lhs.localEndOffset <= rhs.localStartOffset) -1
      else if (rhs.localStartOffset < lhs.localStartOffset && rhs.localEndOffset <= lhs.localStartOffset) 1
      else 0

    @tailrec def compare(thisIc: List[IncludeCtx], otherIc: List[IncludeCtx]): Int = (thisIc, otherIc) match {
      case (Nil, Nil) => comparePos(this, other)
      case (lhead :: _, Nil) => comparePos(lhead, other)
      case (Nil, rhead :: _) => comparePos(this, rhead)
      case (lhead :: ltail, rhead :: rtail) =>
        comparePos(lhead, rhead) match {
          case 0 => Integer.compare(lhead.fileIdx, rhead.fileIdx) match {
            case 0 => compare(ltail, rtail)
            case res => res
          }
          case res => res
        }
    }

    compare(includeChain, other.includeChain) < 0
  }

  // Quite naive implementation but probably the most "correct".
  // I have no idea what _actually_ should I do here since typesafe-config implementation itself is completely
  // unreliable in this area and the "spec" is vague.
  // https://github.com/lightbend/config/blob/master/HOCON.md#self-referential-substitutions
  // Also, this is probably not the most efficient way to filter out these fields.
  def isRemovedBySelfReference: Boolean =
    toplevelCtx.selfReferenced.exists { selfReferenced =>
      @tailrec def loop(rf: ResolvedField): Boolean =
        if (rf.depth > selfReferenced.depth) rf.prefixField match {
          case Some(pf) => loop(pf)
          case None => false
        } else if (rf.depth == selfReferenced.depth) {
          rf.samePathAs(selfReferenced) && !rf.isBefore(selfReferenced)
        } else false
      loop(this)
    }

  def moreOccurrences(opts: ResOpts, withBacktraced: Boolean = true): Iterator[ResolvedField] = {
    val unbacktraced =
      field.adjacentEntriesOccurrences(key.opt, opts, parentCtx) ++
        parentCtx.adjacentOccurrences(key.opt, opts)

    val backtraced =
      if (withBacktraced) backtracedField.flatMapIt(_.moreOccurrences(opts))
      else Iterator.empty

    val res = unbacktraced ++ backtraced
    subsCtx.fold(res)(_ => res.map(_.copy(subsCtx = subsCtx)))
  }

  def nextOccurrence(opts: ResOpts): Option[ResolvedField] =
    moreOccurrences(opts).nextOption

  def resolveValue: Option[ConfigValue] = field match {
    case _: HPrefixedField =>
      Some(ObjectValue)
    case vf: HValuedField if vf.isArrayAppend =>
      Some(ArrayValue)
    case vf: HValuedField =>
      vf.value.flatMap(_.resolveValue(this))
  }
}

sealed trait IncludeSource
object IncludeSource {
  case class Element(inc: HInclude) extends IncludeSource
  case class ToplevelFile(toplevelCtx: ToplevelCtx) extends IncludeSource
}

case class IncludeCtx(
  source: IncludeSource, // None means auto-included file, e.g. reference.conf
  allFiles: Vector[HoconPsiFile],
  fileIdx: Int,
  parentCtx: ResolutionCtx
) extends ResolutionCtx {
  def file: HoconPsiFile = allFiles(fileIdx)

  def occurrences(key: Option[String], opts: ResOpts): Iterator[ResolvedField] =
    if (parentCtx.isAlreadyIn(file)) Iterator.empty
    else file.toplevelEntries.flatMapIt(_.occurrences(key, opts, this))

  def firstOccurrence(key: Option[String], opts: ResOpts): Option[ResolvedField] =
    occurrences(key, opts).nextOption

  def moreFileContexts(reverse: Boolean): Iterator[IncludeCtx] = {
    val idxIt = if (reverse) Iterator.range(fileIdx - 1, -1, -1) else Iterator.range(fileIdx + 1, allFiles.size)
    idxIt.map(i => copy(fileIdx = i))
  }
}
object IncludeCtx {
  def allContexts(
    source: IncludeSource,
    files: Vector[HoconPsiFile],
    reverse: Boolean,
    parentCtx: ResolutionCtx
  ): Iterator[IncludeCtx] =
    if (files.isEmpty) Iterator.empty else {
      val idxIt = if (reverse) files.indices.reverseIterator else files.indices.iterator
      idxIt.map(idx => IncludeCtx(source, files, idx, parentCtx))
    }
}

case class ArrayCtx(
  parentCtx: ResolutionCtx,
  arrayAppend: Boolean,
  value: HValue, // either the array itself or element appended with +=
) extends ResolutionCtx
