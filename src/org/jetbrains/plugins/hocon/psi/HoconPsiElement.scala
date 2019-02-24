package org.jetbrains.plugins.hocon.psi

import java.{lang => jl}

import com.intellij.extapi.psi.ASTWrapperPsiElement
import com.intellij.lang.ASTNode
import com.intellij.openapi.roots.ProjectRootManager
import com.intellij.openapi.util.text.StringUtil
import com.intellij.psi._
import com.intellij.psi.impl.source.resolve.reference.impl.providers.FileReference
import com.intellij.psi.tree.IElementType
import org.jetbrains.plugins.hocon.CommonUtil._
import org.jetbrains.plugins.hocon.HoconConstants
import org.jetbrains.plugins.hocon.HoconConstants._
import org.jetbrains.plugins.hocon.lexer.{HoconTokenSets, HoconTokenType}
import org.jetbrains.plugins.hocon.parser.HoconElementType
import org.jetbrains.plugins.hocon.ref.{HKeySelfReference, IncludedFileReferenceSet}

import scala.annotation.tailrec
import scala.reflect.{ClassTag, classTag}

sealed abstract class HoconPsiElement(ast: ASTNode) extends ASTWrapperPsiElement(ast) {
  type Parent <: PsiElement

  override def getContainingFile: HoconPsiFile =
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

  def findChild[T >: Null : ClassTag] =
    Option(getChild[T])

  def findLastChild[T >: Null : ClassTag]: Option[PsiElement with T] =
    allChildrenReverse.collectFirst({ case t: T => t })

  def allChildren: Iterator[PsiElement] =
    allChildren(reverse = false)

  def allChildrenReverse: Iterator[PsiElement] =
    allChildren(reverse = true)

  def allChildren(reverse: Boolean): Iterator[PsiElement] =
    Iterator.iterate(if (reverse) getLastChild else getFirstChild)(
      c => if (reverse) c.getPrevSibling else c.getNextSibling
    ).takeWhile(_ != null)

  def prevSibling =
    Option(getPrevSibling)

  def prevSiblings: Iterator[PsiElement] =
    Iterator.iterate(getPrevSibling)(_.getPrevSibling).takeWhile(_ != null)

  def nextSibling =
    Option(getNextSibling)

  def nextSiblings: Iterator[PsiElement] =
    Iterator.iterate(getNextSibling)(_.getNextSibling).takeWhile(_ != null)

  def nonWhitespaceChildren: Iterator[PsiElement] =
    allChildren.filterNot(ch => ch.getNode.getElementType == TokenType.WHITE_SPACE)

  def nonWhitespaceOrCommentChildren: Iterator[PsiElement] =
    allChildren.filterNot(ch =>
      (HoconTokenSets.Comment | TokenType.WHITE_SPACE).contains(ch.getNode.getElementType))

  def findChildren[T <: HoconPsiElement : ClassTag]: Iterator[T] =
    findChildren[T](reverse = false)

  def findChildrenReverse[T <: HoconPsiElement : ClassTag]: Iterator[T] =
    findChildren[T](reverse = true)

  def findChildren[T <: HoconPsiElement : ClassTag](reverse: Boolean): Iterator[T] =
    allChildren(reverse).collectOnly[T]
}

sealed trait HInnerElement extends HoconPsiElement {
  type Parent <: HoconPsiElement
}

final class HObjectEntries(ast: ASTNode) extends HoconPsiElement(ast) {
  def forParent[T](forFile: HoconPsiFile => T, forObject: HObject => T): T = parent match {
    case file: HoconPsiFile => forFile(file)
    case obj: HObject => forObject(obj)
  }

  def objectParent: Option[HObject] =
    Option(parent).collect { case obj: HObject => obj }

  def isToplevel: Boolean = forParent(_ => true, _.isToplevel)

  def prefixingField: Option[HValuedField] = forParent(_ => None, obj => obj.prefixingField)

  def entries: Iterator[HObjectEntry] = findChildren[HObjectEntry]

  def occurrences(key: String, reverse: Boolean): Iterator[HObjectField] =
    findChildren[HObjectField](reverse).filter(_.keyedField.hasKeyValue(key))
}

sealed trait HObjectEntry extends HoconPsiElement with HInnerElement {
  type Parent = HObjectEntries

  def containingObject: Option[HObject] = parent.objectParent

  def previousEntry: Option[HObjectEntry] = prevSiblings.collectFirst({ case e: HObjectEntry => e })

  def nextEntry: Option[HObjectEntry] = nextSiblings.collectFirst({ case e: HObjectEntry => e })
}

final class HObjectField(ast: ASTNode) extends HoconPsiElement(ast) with HObjectEntry {
  def docComments: Iterator[PsiComment] = nonWhitespaceChildren
    .takeWhile(_.getNode.getElementType == HoconTokenType.HashComment)
    .map(ch => ch.asInstanceOf[PsiComment])

  def keyedField: HKeyedField = getChild[HKeyedField]

  def endingValue: Option[HValue] = keyedField.endingField.endingValue

  def prevFields: Iterator[HObjectField] =
    prevSiblings.collectOnly[HObjectField]

  def nextFields: Iterator[HObjectField] =
    nextSiblings.collectOnly[HObjectField]

  // there may be bound comments and text offset should be at the beginning of path
  override def getTextOffset: Int = keyedField.getTextOffset
}

sealed trait HKeyedField extends HoconPsiElement with HInnerElement {
  def forParent[T](forKeyedParent: HKeyedField => T, forObjectField: HObjectField => T): T = parent match {
    case kf: HKeyedField => forKeyedParent(kf)
    case of: HObjectField => forObjectField(of)
  }

  def key: Option[HKey] = findChild[HKey]

  def validKey: Option[HKey] = key.filter(_.isValidKey)

  def hasKeyValue(key: String): Boolean =
    validKey.exists(_.stringValue == key)

  def sameKeyAs(other: HKeyedField): Boolean =
    (validKey zip other.validKey).exists {
      case (k1, k2) => k1.stringValue == k2.stringValue
    }

  def subOccurrences(key: String, reverse: Boolean): Iterator[HKeyedField] = this match {
    case pf: HPrefixedField =>
      Iterator(pf.subField).filter(_.hasKeyValue(key))
    case vf: HValuedField =>
      vf.subScopeValue.map(_.occurrences(key, reverse).map(_.keyedField)).getOrElse(Iterator.empty)
  }

  def moreOccurrences(reverse: Boolean): Iterator[HKeyedField] =
    validKey.map(_.stringValue).fold[Iterator[HKeyedField]](Iterator.empty) { key =>
      def byParentField(parentField: HKeyedField): Iterator[HKeyedField] =
        parentField.moreOccurrences(reverse).flatMap(_.subOccurrences(key, reverse))

      forParent(
        keyedField => byParentField(keyedField),
        objectField => {
          def withinEntries = (if (reverse) objectField.prevFields else objectField.nextFields)
            .map(_.keyedField).filter(_.hasKeyValue(key))
          def fromConcatenated = objectField.containingObject
            .map(_.moreConcatenated(reverse).flatMap(_.occurrences(key, reverse)).map(_.keyedField))
            .getOrElse(Iterator.empty)
          def fromEnclosing =
            objectField.containingObject.flatMap(_.prefixingField)
              .map(byParentField).getOrElse(Iterator.empty)

          withinEntries ++ fromConcatenated ++ fromEnclosing
        }
      )
    }

  def prefixingField: Option[HKeyedField] = forParent(
    keyedField => Some(keyedField),
    objectField => objectField.containingObject.flatMap(_.prefixingField)
  )

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
  def fullValidContainingPath: Option[List[HKey]] = {
    @tailrec def loop(currentField: HKeyedField, acc: List[HKey]): Option[List[HKey]] =
      currentField.validKey match {
        case Some(key) => currentField.prefixingField match {
          case Some(parentField) => loop(parentField, key :: acc)
          case None => Some(key :: acc)
        }
        case None => None
      }
    loop(this, Nil)
  }

  def enclosingObjectField: HObjectField =
    forParent(keyedParent => keyedParent.enclosingObjectField, objectField => objectField)

  def enclosingEntries: HObjectEntries =
    enclosingObjectField.parent

  def fieldsInPathForward: Iterator[HKeyedField] = this match {
    case pf: HPrefixedField => Iterator(pf) ++ pf.subField.fieldsInPathForward
    case vf: HValuedField => Iterator(vf)
  }

  def startingField: HKeyedField =
    forParent(_.startingField, _ => this)

  def endingField: HValuedField = this match {
    case pf: HPrefixedField => pf.subField.endingField
    case vf: HValuedField => vf
  }

  def endingValue: Option[HValue] = endingField.value
}

final class HPrefixedField(ast: ASTNode) extends HoconPsiElement(ast) with HKeyedField {
  def subField: HKeyedField = getChild[HKeyedField]
}

final class HValuedField(ast: ASTNode) extends HoconPsiElement(ast) with HKeyedField {
  def value: Option[HValue] = findChild[HValue]

  def subScopeValue: Option[HValue] =
    if (isArrayAppend) None else value

  def isArrayAppend: Boolean =
    separator.contains(HoconTokenType.PlusEquals)

  def separator: Option[HoconTokenType] = Option(findChildByType[PsiElement](HoconTokenSets.KeyValueSeparator))
    .map(_.getNode.getElementType.asInstanceOf[HoconTokenType])
}

final class HInclude(ast: ASTNode) extends HoconPsiElement(ast) with HObjectEntry {
  def included: HQualifiedIncluded = getChild[HQualifiedIncluded]

  // there may be bound comments and text offset should be on 'include' keyword
  override def getTextOffset: Int =
    allChildren.find(_.getNode.getElementType == HoconTokenType.UnquotedChars)
      .map(_.getTextOffset).getOrElse(super.getTextOffset)
}

final class HIncluded(ast: ASTNode) extends HoconPsiElement(ast) with HInnerElement {
  type Parent = HInclude

  def required: Boolean =
    getFirstChild.getNode.getElementType == HoconTokenType.UnquotedChars &&
      getFirstChild.getText == HoconConstants.RequiredModifer

  def qualified: Option[HQualifiedIncluded] = findChild[HQualifiedIncluded]

  def target: Option[HIncludeTarget] = qualified.flatMap(_.target)
}

final class HQualifiedIncluded(ast: ASTNode) extends HoconPsiElement(ast) with HInnerElement {
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

final class HKey(ast: ASTNode) extends HoconPsiElement(ast) with HInnerElement {
  def forParent[T](forPath: HPath => T, forKeyedField: HKeyedField => T): T = parent match {
    case path: HPath => forPath(path)
    case keyedField: HKeyedField => forKeyedField(keyedField)
  }

  def fullValidContainingPath: Option[List[HKey]] =
    forParent(
      path => path.allValidKeys,
      keyedEntry => keyedEntry.fullValidContainingPath
    )

  def enclosingEntries: HObjectEntries =
    forParent(
      _ => getContainingFile.toplevelEntries,
      keyedField => keyedField.enclosingEntries
    )

  def stringValue: String = allChildren.collect {
    case keyPart: HKeyPart => keyPart.stringValue
    case other => other.getText
  }.mkString

  def keyParts: Iterator[HKeyPart] = findChildren[HKeyPart]

  def isValidKey: Boolean = findChild[PsiErrorElement].isEmpty

  override def getReference = new HKeySelfReference(this)
}

final class HPath(ast: ASTNode) extends HoconPsiElement(ast) with HInnerElement {
  def forParent[T](forPath: HPath => T, forSubstitution: HSubstitution => T): T = parent match {
    case path: HPath => forPath(path)
    case subst: HSubstitution => forSubstitution(subst)
  }

  def allPaths: List[HPath] = {
    def allPathsIn(path: HPath, acc: List[HPath]): List[HPath] =
      path.prefix.map(prePath => allPathsIn(prePath, path :: acc)).getOrElse(path :: acc)

    allPathsIn(this, Nil)
  }

  /**
    * Some(all keys in this path) or None if there's an invalid key in path.
    */
  def allValidKeys: Option[List[HKey]] = {
    def allKeysIn(path: HPath, acc: List[HKey]): Option[List[HKey]] =
      path.validKey.flatMap(key => path.prefix
        .map(prePath => allKeysIn(prePath, key :: acc))
        .getOrElse(Some(key :: acc)))

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
}

sealed trait HValue extends HoconPsiElement {
  def forParent[T](
    forValuedField: HValuedField => T,
    forArray: HArray => T,
    forConcatenation: HConcatenation => T,
    forFile: HoconPsiFile => T // only possible for toplevel HObject
  ): T = parent match {
    case vf: HValuedField => forValuedField(vf)
    case arr: HArray => forArray(arr)
    case conc: HConcatenation => forConcatenation(conc)
    case file: HoconPsiFile => forFile(file)
  }

  def prefixingField: Option[HValuedField] = forParent(
    vf => if (vf.isArrayAppend) None else Some(vf),
    _ => None,
    concat => concat.prefixingField,
    _ => None
  )

  def concatParent: Option[HConcatenation] =
    Option(parent).collect { case hc: HConcatenation => hc }

  def moreConcatenated(reverse: Boolean): Iterator[HValue] =
    concatParent.map(_ => (if (reverse) prevSiblings else nextSiblings).collectOnly[HValue]).getOrElse(Iterator.empty)

  def occurrences(key: String, reverse: Boolean): Iterator[HObjectField] = this match {
    case obj: HObject => obj.entries.occurrences(key, reverse)
    case conc: HConcatenation => conc.findChildren[HValue](reverse).flatMap(_.occurrences(key, reverse))
    case _ => Iterator.empty
  }
}

final class HObject(ast: ASTNode) extends HoconPsiElement(ast) with HValue {
  def entries: HObjectEntries = getChild[HObjectEntries]

  def isToplevel: Boolean = parent match {
    case _: HoconPsiFile => true
    case _ => false
  }
}

final class HArray(ast: ASTNode) extends HoconPsiElement(ast) with HValue

final class HSubstitution(ast: ASTNode) extends HoconPsiElement(ast) with HValue {
  def path: Option[HPath] = findChild[HPath]
}

final class HConcatenation(ast: ASTNode) extends HoconPsiElement(ast) with HValue

sealed trait HLiteralValue extends HValue with PsiLiteral

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

sealed trait HString extends HInnerElement with PsiLiteral with ContributedReferenceHost {
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
