package intellijhocon.formatting

import com.intellij.psi.codeStyle.CodeStyleSettings
import com.intellij.formatting.{Spacing, Indent}
import com.intellij.lang.ASTNode
import intellijhocon.parser.{HoconElementSets, HoconElementType}
import intellijhocon.Util
import intellijhocon.lexer.{HoconTokenSets, HoconTokenType}
import intellijhocon.lang.HoconLanguage
import intellijhocon.codestyle.HoconCustomCodeStyleSettings
import com.intellij.psi.tree.IElementType

class HoconFormatter(settings: CodeStyleSettings) {

  import HoconElementSets._
  import HoconElementType._
  import HoconTokenType._
  import HoconTokenSets._
  import Util._

  val commonSettings = settings.getCommonSettings(HoconLanguage)
  val customSettings = settings.getCustomSettings(classOf[HoconCustomCodeStyleSettings])

  val MaxBlankLines = 2 //TODO extract to code style settings

  private def beforeCommentOnNewLineSpacing(commentToken: IElementType) = commentToken match {
    case HashComment if customSettings.HASH_COMMENTS_AT_FIRST_COLUMN =>
      Spacing.createKeepingFirstColumnSpacing(0, 0, true, MaxBlankLines)
    case DoubleSlashComment if customSettings.DOUBLE_SLASH_COMMENTS_AT_FIRST_COLUMN =>
      Spacing.createKeepingFirstColumnSpacing(0, 0, true, MaxBlankLines)
    case _ =>
      Spacing.createSpacing(0, 0, 0, true, MaxBlankLines)
  }

  def getFirstSpacing(parent: ASTNode, firstChild: ASTNode) =
    if (Comment.contains(firstChild.getElementType))
      beforeCommentOnNewLineSpacing(firstChild.getElementType)
    else
      Spacing.createSpacing(0, 0, 0, true, MaxBlankLines)

  def getSpacing(parent: ASTNode, leftChild: ASTNode, rightChild: ASTNode) = {

    def dependentLFSpacing(shouldBeSpace: Boolean) = {
      val spaces = if (shouldBeSpace) 1 else 0
      Spacing.createDependentLFSpacing(spaces, spaces, parent.getTextRange, commonSettings.KEEP_LINE_BREAKS, MaxBlankLines)
    }

    def normalSpacing(shouldBeSpace: Boolean) = {
      val spaces = if (shouldBeSpace) 1 else 0
      Spacing.createSpacing(spaces, spaces, 0, commonSettings.KEEP_LINE_BREAKS, MaxBlankLines)
    }

    val lineBreakEnsuringSpacing =
      Spacing.createSpacing(0, 0, 1, commonSettings.KEEP_LINE_BREAKS, MaxBlankLines)

    val isLineBreakBetween = parent.getText.subSequence(
      leftChild.getTextRange.getEndOffset - parent.getTextRange.getStartOffset,
      rightChild.getTextRange.getStartOffset - parent.getTextRange.getStartOffset)
      .charIterator.contains('\n')

    def standardSpacing = (leftChild.getElementType, rightChild.getElementType) match {
      case (LBrace, RBrace) =>
        normalSpacing(commonSettings.SPACE_WITHIN_BRACES)

      case (LBrace, Include | BareObjectField) =>
        if (customSettings.OBJECTS_NEW_LINE_AFTER_LBRACE)
          dependentLFSpacing(commonSettings.SPACE_WITHIN_BRACES)
        else
          normalSpacing(commonSettings.SPACE_WITHIN_BRACES)

      case (Include | BareObjectField, Include | BareObjectField) =>
        lineBreakEnsuringSpacing

      case (Include | BareObjectField, Comma) =>
        normalSpacing(commonSettings.SPACE_BEFORE_COMMA)

      case (Comma, BareObjectField | Include) =>
        normalSpacing(commonSettings.SPACE_AFTER_COMMA)

      case (BareObjectField | Include | Comma, RBrace) =>
        if (customSettings.OBJECTS_RBRACE_ON_NEXT_LINE)
          dependentLFSpacing(commonSettings.SPACE_WITHIN_BRACES)
        else
          normalSpacing(commonSettings.SPACE_WITHIN_BRACES)

      case (LBracket, RBracket) =>
        normalSpacing(commonSettings.SPACE_WITHIN_BRACKETS)

      case (LBracket, Value) =>
        if (customSettings.LISTS_NEW_LINE_AFTER_LBRACKET)
          dependentLFSpacing(commonSettings.SPACE_WITHIN_BRACKETS)
        else
          normalSpacing(commonSettings.SPACE_WITHIN_BRACKETS)

      case (Value, Value) =>
        lineBreakEnsuringSpacing

      case (Value, Comma) =>
        normalSpacing(commonSettings.SPACE_BEFORE_COMMA)

      case (Comma, Value) =>
        normalSpacing(commonSettings.SPACE_AFTER_COMMA)

      case (Value | Comma, RBracket) =>
        if (customSettings.LISTS_RBRACKET_ON_NEXT_LINE)
          dependentLFSpacing(commonSettings.SPACE_WITHIN_BRACKETS)
        else
          normalSpacing(commonSettings.SPACE_WITHIN_BRACKETS)

      case (FieldPath, Object) =>
        normalSpacing(customSettings.SPACE_BEFORE_LBRACE_AFTER_PATH)

      case (FieldPath, Colon) =>
        if (customSettings.OBJECT_FIELDS_COLON_ON_NEXT_LINE)
          dependentLFSpacing(customSettings.SPACE_BEFORE_COLON)
        else
          normalSpacing(customSettings.SPACE_BEFORE_COLON)

      case (FieldPath, Equals | PlusEquals) =>
        if (customSettings.OBJECT_FIELDS_ASSIGNMENT_ON_NEXT_LINE)
          dependentLFSpacing(customSettings.SPACE_BEFORE_ASSIGNMENT)
        else
          normalSpacing(customSettings.SPACE_BEFORE_ASSIGNMENT)

      case (Colon, Value) =>
        normalSpacing(customSettings.SPACE_AFTER_COLON)

      case (Equals | PlusEquals, Value) =>
        normalSpacing(customSettings.SPACE_AFTER_ASSIGNMENT)

      case (Dollar, RefLBrace) | (RefLBrace, QMark) =>
        Spacing.getReadOnlySpacing

      case (RefLBrace, ReferencePath | RefRBrace) =>
        normalSpacing(customSettings.SPACES_WITHIN_REFERENCE_BRACES)

      case (QMark, ReferencePath) =>
        normalSpacing(customSettings.SPACE_AFTER_QMARK)

      case (ReferencePath, RefRBrace) =>
        normalSpacing(customSettings.SPACES_WITHIN_REFERENCE_BRACES)

      case (UnquotedChars, QuotedString) | (QuotedString, UnquotedChars) if parent.getElementType == Included =>
        normalSpacing(commonSettings.SPACE_WITHIN_METHOD_CALL_PARENTHESES)

      case _ =>
        null

    }

    if (Comment.contains(rightChild.getElementType)) {
      if (isLineBreakBetween)
        beforeCommentOnNewLineSpacing(rightChild.getElementType)
      else
        null
    } else if (Comment.contains(leftChild.getElementType))
      Spacing.createSafeSpacing(true, MaxBlankLines)
    else if (parent.getElementType == Value)
      Spacing.getReadOnlySpacing
    else
      standardSpacing
  }

  def getIndent(parent: ASTNode, child: ASTNode) =
    (parent.getElementType, child.getElementType) match {
      case (Object, Include | BareObjectField | Comma | Comment.extractor()) |
           (Array, Value | Comma | Comment.extractor()) =>
        Indent.getNormalIndent
      case (Include, Included) |
           (BareObjectField, PathValueSeparator.extractor() | Value) =>
        Indent.getContinuationIndent
      case _ =>
        Indent.getNoneIndent

    }

  def getChildIndent(parent: ASTNode) = parent.getElementType match {
    case Object | Array => Indent.getNormalIndent
    case Include | BareObjectField => Indent.getContinuationIndent
    case _ => Indent.getNoneIndent
  }

  def getChildren(node: ASTNode): Iterator[ASTNode] = node.getElementType match {
    case ForcedLeafBlock.extractor() =>
      Iterator.empty
    case HoconFileElementType | Object =>
      node.childrenIterator.flatMap(child => child.getElementType match {
        case ObjectEntries => getChildren(child)
        case _ => Iterator(child)
      })
    case ObjectEntries =>
      node.childrenIterator.flatMap(child => child.getElementType match {
        case ObjectField => getChildren(child)
        case _ => Iterator(child)
      })
    case _ => node.childrenIterator
  }
}
