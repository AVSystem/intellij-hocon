package org.jetbrains.plugins.hocon
package parser

import lang.HoconLanguage

import com.intellij.psi.tree.{IElementType, IFileElementType}

enum HoconElementType(debugName: String) extends IElementType(debugName, HoconLanguage) {

  /** Object, i.e. object entries inside braces.
    *
    * {{{
    *   {
    *     include file("stuff")
    *     some.path = value
    *   }
    * }}}
    */
  case Object extends HoconElementType("OBJECT")

  /** Contents of HOCON file or object, contains includes and object fields.
    *
    * {{{
    *   include file("stuff")
    *   some.path = value
    * }}}
    */
  case ObjectEntries extends HoconElementType("OBJECT_ENTRIES")

  /** `include` clause
    *
    * {{{
    *   include file("stuff")
    * }}}
    */
  case Include extends HoconElementType("INCLUDE")

  /** Thing that comes after `include` keyword, including possible `required` modifier
    *
    * {{{
    *   required(file("stuff"))
    * }}}
    */
  case Included extends HoconElementType("INCLUDED")

  /** Thing that comes after `include` keyword but without the enclosing `required` modifier, if any.
    *
    * {{{
    *   file("stuff")
    * }}}
    */
  case QualifiedIncluded extends HoconElementType("QUALIFIED_INCLUDED")

  /** Keyed field (i.e. prefixed field or valued field) along with documentation comments.
    *
    * {{{
    *   # This doc comment is contained in an object field.
    *   # After docs comes a prefixed field or valued field (in this example - prefixed field)
    *   prefix.key = value
    * }}}
    *
    * Even if there are no doc comments, keyed field is always enclosed inside object field.
    */
  case ObjectField extends HoconElementType("OBJECT_FIELD")

  /** A path-value field in which path contains more than one key:
    *
    * {{{
    *   prefix.key = value
    * }}}
    *
    * Prefixed field divides itself into first key (`prefix` in above example) and rest of the prefixed field which may
    * be another prefixed field or valued field (`key = value` in above example, which is a valued field).
    */
  case PrefixedField extends HoconElementType("PREFIXED_FIELD")

  /** A key-value association (NOT path-value):
    *
    * {{{
    *    key = value
    * }}}
    */
  case ValuedField extends HoconElementType("VALUED_FIELD")

  /** Path inside substitution. Divides into prefix path and last key.
    */
  case Path extends HoconElementType("PATH")

  /** Key inside field (prefixed field or valued field).
    */
  case FieldKey extends HoconElementType("FIELD_KEY")

  /** Key inside substitution path.
    */
  case SubstitutionKey extends HoconElementType("SUBSTITUTION_KEY")

  /** HOCON array, i.e. brackets with sequence of values inside.
    */
  case Array extends HoconElementType("ARRAY")

  /** HOCON substitution, i.e. path enclosed in `${}` (with optional `?` sign)
    */
  case Substitution extends HoconElementType("SUBSTITUTION")

  /** Concatenation of two or more HOCON values.
    */
  case Concatenation extends HoconElementType("CONCATENATION")

  /** Unquoted string - a concatenation of whitespace, unquoted chars, parens and periods. This element type exists
    * primarily so that String element always has exactly one child (unquoted, quoted or multiline string). Unquoted
    * string occurs as a child of String or Key.
    */
  case UnquotedString extends HoconElementType("UNQUOTED_STRING")

  /** Encapsulates either an unquoted, quoted or multiline string - in value context.
    */
  case StringValue extends HoconElementType("STRING_VALUE")

  /** Quoted string in `include` clause context.
    */
  case IncludeTarget extends HoconElementType("INCLUDE_TARGET")

  /** Encapsulates either an unquoted, quoted or multiline string - in key context.
    */
  case KeyPart extends HoconElementType("KEY_PART")

  /** Literal numeric value.
    */
  case Number extends HoconElementType("NUMBER")

  /** Literal `null` value.
    */
  case Null extends HoconElementType("NULL")

  /** Literal boolean value.
    */
  case Boolean extends HoconElementType("BOOLEAN")

}

object HoconElementType {
  val HoconFileElementType = new IFileElementType("HOCON_FILE", HoconLanguage)
}
