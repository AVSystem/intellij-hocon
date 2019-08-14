package org.jetbrains.plugins.hocon
package psi

import java.{lang => jl}

import com.intellij.extapi.psi.ASTWrapperPsiElement
import com.intellij.lang.ASTNode
import com.intellij.openapi.fileTypes.FileType
import com.intellij.openapi.roots.ProjectRootManager
import com.intellij.openapi.util.text.StringUtil
import com.intellij.psi._
import com.intellij.psi.impl.source.PsiFileImpl
import com.intellij.psi.impl.source.resolve.reference.impl.providers.FileReference
import com.intellij.psi.tree.IElementType
import org.jetbrains.plugins.hocon.HoconConstants._
import org.jetbrains.plugins.hocon.lang.HoconFileType
import org.jetbrains.plugins.hocon.lexer.{HoconTokenSets, HoconTokenType}
import org.jetbrains.plugins.hocon.parser.HoconElementType
import org.jetbrains.plugins.hocon.parser.HoconElementType.HoconFileElementType
import org.jetbrains.plugins.hocon.ref.{HKeyReference, IncludedFileReferenceSet}

import scala.annotation.tailrec
import scala.reflect.{ClassTag, classTag}

sealed trait HoconPsiParent extends PsiElement

sealed trait HObjectEntriesParent extends HoconPsiParent
sealed trait HKeyedFieldParent extends HoconPsiElement with HoconPsiParent
sealed trait HKeyParent extends HoconPsiElement with HoconPsiParent
sealed trait HPathParent extends HoconPsiElement with HoconPsiParent
sealed trait HValueParent extends HoconPsiParent

class HoconPsiFile(provider: FileViewProvider)
  extends PsiFileImpl(HoconFileElementType, HoconFileElementType, provider)
    with HObjectEntriesParent with HValueParent {

  def accept(visitor: PsiElementVisitor): Unit =
    visitor.visitFile(this)

  def getFileType: FileType =
    HoconFileType

  def toplevelEntries: HObjectEntries = {
    @tailrec
    def entriesInner(child: PsiElement): HObjectEntries = child match {
      case obj: HObject => obj.entries
      case ets: HObjectEntries => ets
      case comment: PsiComment =>
        val sibling = notWhiteSpaceSibling(comment)(_.getNextSibling)
        entriesInner(sibling)
    }
    entriesInner(getFirstChild)
  }

  def toplevelObject: Option[HObject] = getFirstChild match {
    case obj: HObject => Some(obj)
    case _ => None
  }

  def elementsAt(offset: Int): Iterator[PsiElement] = {
    val leaf = findElementAt(offset)
    Iterator.iterate(leaf)(_.getParent).takeWhile(e => e != null && (e ne this))
  }
}

sealed abstract class HoconPsiElement(ast: ASTNode) extends ASTWrapperPsiElement(ast) with HoconPsiParent {
  type Parent <: HoconPsiParent

  def hoconFile: HoconPsiFile =
    super.getContainingFile.asInstanceOf[HoconPsiFile]

  def parent: Parent =
    getParent.asInstanceOf[Parent]

  def parents: Iterator[HoconPsiElement] =
    Iterator.iterate(this)(_.getParent match {
      case he: HoconPsiElement => he
      case _ => null
    }).takeWhile(_ != null)

  def elementType: IElementType =
    getNode.getElementType

  def getChild[T >: Null : ClassTag]: T =
    findChildByClass(classTag[T].runtimeClass.asInstanceOf[Class[T]])

  def findChild[T >: Null : ClassTag](reverse: Boolean): Option[T] =
    allChildren(reverse).collectFirst({ case t: T => t })

  def findChild[T >: Null : ClassTag]: Option[T] =
    findChild[T](reverse = false)

  def findLastChild[T >: Null : ClassTag]: Option[T] =
    findChild[T](reverse = true)

  def allChildren(reverse: Boolean): Iterator[PsiElement] =
    Iterator.iterate(if (reverse) getLastChild else getFirstChild)(
      c => if (reverse) c.getPrevSibling else c.getNextSibling
    ).takeWhile(_ != null)

  def nextSibling[T: ClassTag](reverse: Boolean): Option[T] =
    moreSiblings(reverse).collectFirst { case t: T => t }

  def moreSiblings(reverse: Boolean): Iterator[PsiElement] =
    Iterator.iterate(getNextSibling(reverse))(_.getNextSibling(reverse)).takeWhile(_ != null)

  def nonWhitespaceChildren: Iterator[PsiElement] =
    allChildren(reverse = false).filterNot(ch => ch.getNode.getElementType == TokenType.WHITE_SPACE)

  def nonWhitespaceOrCommentChildren: Iterator[PsiElement] =
    allChildren(reverse = false).filterNot(ch =>
      (HoconTokenSets.Comment | TokenType.WHITE_SPACE).contains(ch.getNode.getElementType))

  def findChildren[T <: HoconPsiElement : ClassTag]: Iterator[T] =
    findChildren[T](reverse = false)

  def findChildren[T <: HoconPsiElement : ClassTag](reverse: Boolean): Iterator[T] =
    allChildren(reverse).collectOnly[T]
}

final class HObjectEntries(ast: ASTNode) extends HoconPsiElement(ast) with HEntriesLike {
  type Parent = HObjectEntriesParent

  def containingObject: Option[HObject] =
    Option(parent).collect { case obj: HObject => obj }

  def isToplevel: Boolean = parent match {
    case _: HoconPsiFile => true
    case obj: HObject => obj.isToplevel
  }

  def prefixingField: Option[HValuedField] = parent match {
    case _: HoconPsiFile => None
    case obj: HObject => obj.prefixingField
  }

  def entries(reverse: Boolean): Iterator[HObjectEntry] =
    findChildren[HObjectEntry](reverse)

  def moreEntries(reverse: Boolean): Iterator[HEntriesLike] = Iterator.empty

  def occurrences(key: Option[String], reverse: Boolean, resCtx: ResolutionCtx): Iterator[ResolvedField] =
    entries(reverse).flatMap(_.occurrences(key, reverse, resCtx))

  def firstOccurrence(path: List[String], reverse: Boolean, resCtx: ResolutionCtx): Option[ResolvedField] =
    occurrences(path, reverse, resCtx).nextOption

  def occurrences(path: List[String], reverse: Boolean, resCtx: ResolutionCtx): Iterator[ResolvedField] = path match {
    case Nil => Iterator.empty
    case firstKey :: restOfKeys =>
      val occurrencesOfFirst = occurrences(Some(firstKey), reverse, resCtx)
      restOfKeys.foldLeft(occurrencesOfFirst) { (occ, key) =>
        occ.flatMap(_.subOccurrences(Some(key), reverse))
      }
  }
}

sealed trait HObjectEntry extends HoconPsiElement with HEntriesLike {
  type Parent = HObjectEntries

  def containingObject: Option[HObject] = parent.containingObject

  override def moreEntries(reverse: Boolean): Iterator[HObjectEntry] =
    moreSiblings(reverse).collectOnly[HObjectEntry]

  override def nextEntry(reverse: Boolean): Option[HObjectEntry] =
    nextSibling[HObjectEntry](reverse)

  def nextEntry: Option[HObjectEntry] = nextEntry(reverse = false)

  def prevEntry: Option[HObjectEntry] = nextEntry(reverse = true)

  def firstOccurrence(key: Option[String], reverse: Boolean, resCtx: ResolutionCtx): Option[ResolvedField]
}

final class HObjectField(ast: ASTNode) extends HoconPsiElement(ast) with HObjectEntry with HKeyedFieldParent {
  def docComments: Iterator[PsiComment] = nonWhitespaceChildren
    .takeWhile(_.getNode.getElementType == HoconTokenType.HashComment)
    .map(ch => ch.asInstanceOf[PsiComment])

  def keyedField: HKeyedField = getChild[HKeyedField]

  def occurrences(key: Option[String], reverse: Boolean, resCtx: ResolutionCtx): Iterator[ResolvedField] =
    keyedField.occurrences(key, reverse, resCtx)

  // there may be bound comments and text offset should be at the beginning of path
  override def getTextOffset: Int = keyedField.getTextOffset
}

sealed trait HEntriesLike extends HoconPsiElement {
  def moreEntries(reverse: Boolean): Iterator[HEntriesLike]

  def containingObject: Option[HObject]

  def nextEntry(reverse: Boolean): Option[HEntriesLike] =
    moreEntries(reverse).nextOption

  def firstOccurrence(key: Option[String], reverse: Boolean, resCtx: ResolutionCtx): Option[ResolvedField] =
    occurrences(key, reverse, resCtx).nextOption

  def occurrences(key: Option[String], reverse: Boolean, resCtx: ResolutionCtx): Iterator[ResolvedField]

  /**
   * Occurrences of given key (or all) within entries adjacent to these in the same containing object and objects
   * concatenated with it.
   */
  def adjacentEntriesOccurrences(key: Option[String], reverse: Boolean, resCtx: ResolutionCtx): Iterator[ResolvedField] = {
    def fromReferenceOrConcat = resCtx match {
      case tc: ToplevelCtx if reverse =>
        // going into the contents of auto-included files, e.g. reference.conf
        tc.referenceIncludeCtxs(reverse = true).flatMap(_.occurrences(key, reverse))
      case _ =>
        containingObject.flatMapIt(_.adjacentConcatOccurrences(key, reverse, resCtx))
    }
    moreEntries(reverse).flatMap(_.occurrences(key, reverse, resCtx)) ++ fromReferenceOrConcat
  }
}

sealed trait HKeyedField extends HEntriesLike with HKeyedFieldParent with HKeyParent {
  type Parent = HKeyedFieldParent

  def containingObject: Option[HObject] = parent match {
    case of: HObjectField => of.containingObject
    case _: HKeyedField => None
  }

  def key: Option[HKey] = findChild[HKey]

  def validKey: Option[HKey] = key.filter(_.isValidKey)

  def validKeyString: Option[String] = validKey.map(_.stringValue)

  def hasKeyValue(key: String): Boolean =
    validKeyString.contains(key)

  def sameKeyAs(other: HKeyedField): Boolean =
    (validKey zip other.validKey).exists {
      case (k1, k2) => k1.stringValue == k2.stringValue
    }

  def moreEntries(reverse: Boolean): Iterator[HEntriesLike] = parent match {
    case of: HObjectField => of.moreEntries(reverse)
    case _: HKeyedField => Iterator.empty
  }

  def occurrences(key: Option[String], reverse: Boolean, resCtx: ResolutionCtx): Iterator[ResolvedField] =
    validKeyString.filter(selfKey => key.isEmpty || key.contains(selfKey))
      .map(selfKey => ResolvedField(selfKey, this, resCtx))
      .iterator

  def prefixingField: Option[HKeyedField] = parent match {
    case keyedField: HKeyedField => Some(keyedField)
    case objectField: HObjectField => objectField.containingObject.flatMap(_.prefixingField)
  }

  /**
   * Goes up the tree in order to determine full path under which this keyed field is defined.
   * Stops when encounters file-toplevel entries or an array (including array-append field).
   *
   * @return iterator of all encountered keyed fields (in bottom-up order, i.e. starting with itself)
   */
  def prefixingFields: Iterator[HKeyedField] =
    Iterator.iterate(this)(_.prefixingField.orNull).takeWhile(_ != null)

  /**
   * Returns all keys on containing path, assuming they are all valid keys. `None` is returned if not all keys
   * on containing path are valid. The list is ordered top-down, i.e. `this` key is the last element.
   * "Containing path" starts at the smallest enclosing array element or at the file toplevel entries if there are
   * no arrays on the way to this field.
   */
  def fullValidContainingPath: Option[(HObjectEntries, List[HKey])] = {
    @tailrec def loop(currentField: HKeyedField, acc: List[HKey]): Option[(HObjectEntries, List[HKey])] =
      currentField.validKey match {
        case Some(key) => currentField.prefixingField match {
          case Some(parentField) => loop(parentField, key :: acc)
          case None => Some((currentField.enclosingEntries, key :: acc))
        }
        case None => None
      }
    loop(this, Nil)
  }

  def enclosingObjectField: HObjectField = parent match {
    case keyedParent: HKeyedField => keyedParent.enclosingObjectField
    case objectField: HObjectField => objectField
  }

  def enclosingEntries: HObjectEntries =
    enclosingObjectField.parent

  def fieldsInPathForward: Iterator[HKeyedField] = this match {
    case pf: HPrefixedField => Iterator(pf) ++ pf.subField.fieldsInPathForward
    case vf: HValuedField => Iterator(vf)
  }

  private def toplevelContext: ToplevelCtx = {
    val file = hoconFile
    ToplevelCtx(file, ToplevelCtx.referenceFilesFor(file))
  }

  def makeContext: Option[ResolvedField] =
    validKeyString.map { key =>
      val parentCtx = prefixingField.flatMap(_.makeContext).getOrElse(toplevelContext)
      ResolvedField(key, this, parentCtx)
    }
}

final class HPrefixedField(ast: ASTNode) extends HoconPsiElement(ast) with HKeyedField {
  def subField: HKeyedField = getChild[HKeyedField]
}

final class HValuedField(ast: ASTNode) extends HoconPsiElement(ast) with HKeyedField with HValueParent {
  def value: Option[HValue] = findChild[HValue]

  def subScopeValue: Option[HValue] =
    if (isArrayAppend) None else value

  def isArrayAppend: Boolean =
    separator.contains(HoconTokenType.PlusEquals)

  def separator: Option[HoconTokenType] = Option(findChildByType[PsiElement](HoconTokenSets.KeyValueSeparator))
    .map(_.getNode.getElementType.asInstanceOf[HoconTokenType])
}

final class HInclude(ast: ASTNode) extends HoconPsiElement(ast) with HObjectEntry with HEntriesLike {
  def included: HIncluded = getChild[HIncluded]

  // there may be bound comments and text offset should be on 'include' keyword
  override def getTextOffset: Int =
    allChildren(reverse = false).find(_.getNode.getElementType == HoconTokenType.UnquotedChars)
      .map(_.getTextOffset).getOrElse(super.getTextOffset)

  def occurrences(key: Option[String], reverse: Boolean, resCtx: ResolutionCtx): Iterator[ResolvedField] =
    if (resCtx.toplevelCtx.directOnly) Iterator.empty
    else included.qualified.flatMap(_.fileReferenceSet).fold[Iterator[ResolvedField]](Iterator.empty) { refset =>
      //TODO: search also .json and .properties files
      val allFiles = refset.getLastReference.opt.fold(Vector.empty[HoconPsiFile]) { ref =>
        ref.multiResolve(false).iterator.map(_.getElement).collectOnly[HoconPsiFile].toVector
      }
      IncludeCtx.allContexts(Some(this), allFiles, reverse, resCtx)
        .flatMap(_.occurrences(key, reverse))
    }
}

final class HIncluded(ast: ASTNode) extends HoconPsiElement(ast) {
  type Parent = HInclude

  def required: Boolean =
    getFirstChild.getNode.getElementType == HoconTokenType.UnquotedChars &&
      getFirstChild.getText == HoconConstants.RequiredModifer

  def qualified: Option[HQualifiedIncluded] = findChild[HQualifiedIncluded]

  def target: Option[HIncludeTarget] = qualified.flatMap(_.target)
}

final class HQualifiedIncluded(ast: ASTNode) extends HoconPsiElement(ast) {
  type Parent = HIncluded

  def qualifier: Option[String] = getFirstChild.getNode.getElementType match {
    case HoconTokenType.UnquotedChars =>
      Some(getFirstChild.getText)
    case _ => None
  }

  def target: Option[HIncludeTarget] = findChild[HIncludeTarget]

  def fileReferenceSet: Option[IncludedFileReferenceSet] =
    for {
      hs <- target
      vf <- Option(getContainingFile.getOriginalFile.getVirtualFile)
      rs <- {
        val strVal = hs.stringValue

        val (absolute, forcedAbsolute, fromClasspath) = qualifier match {
          case Some(ClasspathModifier) => (true, true, true)
          case None if !isValidUrl(strVal) =>
            val pfi = ProjectRootManager.getInstance(getProject).getFileIndex
            val fromClasspath = pfi.isInSource(vf) || pfi.isInLibraryClasses(vf)
            (strVal.trim.startsWith("/"), false, fromClasspath)
          case _ => (true, true, false)
        }

        // Include resolution is enabled for:
        // - classpath() includes anywhere
        // - unqualified includes in classpath (source or library) files
        // - relative unqualified includes in non-classpath files
        if (!absolute || fromClasspath)
          Some(new IncludedFileReferenceSet(strVal, hs, forcedAbsolute, fromClasspath))
        else
          None
      }
    } yield rs
}

final class HKey(ast: ASTNode) extends HoconPsiElement(ast) {
  type Parent = HKeyParent

  def fullValidContainingPath: Option[(HObjectEntries, List[HKey])] = parent match {
    case path: HPath => path.allValidKeys.map(keys => (hoconFile.toplevelEntries, keys))
    case keyedField: HKeyedField => keyedField.fullValidContainingPath
  }

  def fullValidContainingPathString: Option[String] = fullValidContainingPath.map {
    case (_, path) => path.iterator.map(_.getText).mkString(".")
  }

  def enclosingEntries: HObjectEntries = parent match {
    case _: HPath => hoconFile.toplevelEntries
    case keyedField: HKeyedField => keyedField.enclosingEntries
  }

  def stringValue: String = allChildren(reverse = false).collect {
    case keyPart: HKeyPart => keyPart.stringValue
    case other => other.getText
  }.mkString

  def resolved: Option[ResolvedField] = parent match {
    case path: HPath => path.resolve()
    case kf: HKeyedField => kf.makeContext
  }

  def keyParts: Iterator[HKeyPart] = findChildren[HKeyPart]

  def isValidKey: Boolean = findChild[PsiErrorElement].isEmpty

  override def getReference = new HKeyReference(this)
}

final class HPath(ast: ASTNode) extends HoconPsiElement(ast) with HKeyParent with HPathParent {
  type Parent = HPathParent

  @tailrec def substitution: HSubstitution = parent match {
    case path: HPath => path.substitution
    case sub: HSubstitution => sub
  }

  def allPaths: List[HPath] = {
    @tailrec def allPathsIn(path: HPath, acc: List[HPath]): List[HPath] = path.prefix match {
      case Some(prePath) => allPathsIn(prePath, path :: acc)
      case None => path :: acc
    }
    allPathsIn(this, Nil)
  }

  /**
   * Some(all keys in this path) or None if there's an invalid key in path.
   */
  def allValidKeys: Option[List[HKey]] = {
    def allKeysIn(path: HPath, acc: List[HKey]): Option[List[HKey]] =
      path.validKey.flatMap { key =>
        path.prefix.map(prePath => allKeysIn(prePath, key :: acc)).getOrElse(Some(key :: acc))
      }
    allKeysIn(this, Nil)
  }

  /**
   * If all keys are valid - all keys of this path.
   * If some keys are invalid - all valid keys from left to right until some invalid key is encountered
   * (i.e. longest valid prefix path)
   */
  def startingValidKeys: List[HKey] =
    allPaths.iterator.takeWhile(_.validKey.nonEmpty).flatMap(_.validKey).toList

  def startingPath: HPath = prefix.map(_.startingPath).getOrElse(this)

  def prefix: Option[HPath] = findChild[HPath]

  def validKey: Option[HKey] = findChild[HKey].filter(_.isValidKey)

  def resolve(): Option[ResolvedField] = {
    val subst = substitution
    val file = hoconFile
    val resCtx = ToplevelCtx(file, ToplevelCtx.referenceFilesFor(file))
    val resField = subst.resolve(reverse = true, resCtx).nextOption

    @tailrec def gotoPrefix(rfOpt: Option[ResolvedField], subpath: HPath): Option[ResolvedField] =
      if (subpath eq this) rfOpt
      else (rfOpt, subpath.prefix) match {
        case (Some(rf), Some(ppath)) => gotoPrefix(rf.prefixField, ppath)
        case _ => None
      }

    subst.path.flatMap(fullPath => gotoPrefix(resField, fullPath))
  }
}

sealed trait HValue extends HoconPsiElement {
  type Parent = HValueParent

  def prefixingField: Option[HValuedField] = parent match {
    case vf: HValuedField if vf.isArrayAppend => None
    case vf: HValuedField => Some(vf)
    case _: HArray => None
    case concat: HConcatenation => concat.prefixingField
    case _: HoconPsiFile => None
  }

  def concatParent: Option[HConcatenation] =
    Option(parent).collect { case hc: HConcatenation => hc }

  def moreConcatenated(reverse: Boolean): Iterator[HValue] =
    concatParent.flatMapIt(_ => moreSiblings(reverse).collectOnly[HValue])

  def firstOccurrence(key: Option[String], reverse: Boolean, resCtx: ResolutionCtx): Option[ResolvedField] =
    occurrences(key, reverse, resCtx).nextOption

  def occurrences(key: Option[String], reverse: Boolean, resCtx: ResolutionCtx): Iterator[ResolvedField] = this match {
    case obj: HObject =>
      obj.entries.occurrences(key, reverse, resCtx)
    case conc: HConcatenation =>
      conc.findChildren[HValue](reverse).flatMap(_.occurrences(key, reverse, resCtx))
    case subst: HSubstitution =>
      subst.resolve(reverse, resCtx).flatMap(_.subOccurrences(key, reverse))
        .map(rf => rf.copy(parentCtx = SubstitutedCtx(rf.parentCtx, resCtx, subst)))
    case _ =>
      Iterator.empty
  }

  def adjacentConcatOccurrences(key: Option[String], reverse: Boolean, parentCtx: ResolutionCtx): Iterator[ResolvedField] =
    moreConcatenated(reverse).flatMap(_.occurrences(key, reverse, parentCtx))
}

final class HObject(ast: ASTNode) extends HoconPsiElement(ast) with HValue with HObjectEntriesParent {
  def entries: HObjectEntries = getChild[HObjectEntries]

  def isToplevel: Boolean = parent match {
    case _: HoconPsiFile => true
    case _ => false
  }
}

final class HArray(ast: ASTNode) extends HoconPsiElement(ast) with HValue with HValueParent

final class HSubstitution(ast: ASTNode) extends HoconPsiElement(ast) with HValue with HPathParent {
  def path: Option[HPath] = findChild[HPath]

  def resolve(reverse: Boolean, resCtx: ResolutionCtx): Iterator[ResolvedField] =
    if (resCtx.toplevelCtx.directOnly) Iterator.empty
    else path.flatMap(_.allValidKeys).fold[Iterator[ResolvedField]](Iterator.empty) { keys =>
      //TODO: self-referential substitutions!
      val toplevelCtx = resCtx.toplevelCtx
      val newCtx = toplevelCtx.copy(forSubst = Some(OpenSubstitution(resCtx, this)))
      newCtx.occurrences(keys.map(_.stringValue), reverse)
    }
}

final class HConcatenation(ast: ASTNode) extends HoconPsiElement(ast) with HValue with HValueParent

sealed trait HLiteralValue extends HValue with PsiLiteralValue

final class HNull(ast: ASTNode) extends HoconPsiElement(ast) with HLiteralValue {
  def getValue: Object = null
}

final class HBoolean(ast: ASTNode) extends HoconPsiElement(ast) with HLiteralValue {
  def getValue: Object = jl.Boolean.valueOf(booleanValue)

  def booleanValue: Boolean = getText.toBoolean
}

final class HNumber(ast: ASTNode) extends HoconPsiElement(ast) with HLiteralValue {
  def getValue: Object = numberValue

  def numberValue: jl.Number =
    if (getText.exists(HNumber.DecimalIndicators.contains))
      jl.Double.parseDouble(getText)
    else
      jl.Long.parseLong(getText)
}

object HNumber {
  private final val DecimalIndicators = Set('.', 'e', 'E')
}

final class HUnquotedString(ast: ASTNode) extends HoconPsiElement(ast)

sealed trait HString extends HoconPsiElement with PsiLiteralValue with ContributedReferenceHost {
  def stringType: IElementType = getFirstChild.getNode.getElementType

  def getValue: Object = stringValue

  def unquote: String = stringType match {
    case HoconTokenType.QuotedString =>
      getText.substring(1, getText.length - (if (isClosed) 1 else 0))
    case HoconTokenType.MultilineString =>
      getText.substring(3, getText.length - (if (isClosed) 3 else 0))
    case HoconElementType.UnquotedString =>
      getText
  }

  def stringValue: String = stringType match {
    case HoconTokenType.QuotedString =>
      StringUtil.unescapeStringCharacters(unquote)
    case _ =>
      unquote
  }

  def isClosed: Boolean = stringType match {
    case HoconTokenType.QuotedString =>
      HoconConstants.ProperlyClosedQuotedString.pattern.matcher(getText).matches
    case HoconTokenType.MultilineString =>
      getText.endsWith("\"\"\"")
    case _ =>
      true
  }

  override def getReferences: Array[PsiReference] =
    PsiReferenceService.getService.getContributedReferences(this)
}

final class HStringValue(ast: ASTNode) extends HoconPsiElement(ast) with HString with HLiteralValue

final class HKeyPart(ast: ASTNode) extends HoconPsiElement(ast) with HString {
  type Parent = HKey
}

final class HIncludeTarget(ast: ASTNode) extends HoconPsiElement(ast) with HString {
  type Parent = HQualifiedIncluded

  def getFileReferences: Array[FileReference] =
    parent.fileReferenceSet.map(_.getAllReferences).getOrElse(FileReference.EMPTY)
}
