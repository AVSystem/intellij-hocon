package org.jetbrains.plugins.hocon

import com.intellij.json.JsonFileType
import org.jetbrains.plugins.hocon.lang.HoconFileType

object HoconConstants {
  final val Null = "null"
  final val True = "true"
  final val False = "false"
  final val Include = "include"

  final val UrlModifier = "url"
  final val FileModifier = "file"
  final val ClasspathModifier = "classpath"
  final val RequiredModifer = "required"
  final val IncludeLocationModifiers = Set(UrlModifier, FileModifier, ClasspathModifier)
  final val IncludeModifiers = IncludeLocationModifiers + RequiredModifer

  final val IntegerPattern = """-?(0|[1-9][0-9]*)""".r
  final val DecimalPartPattern = """([0-9]+)((e|E)(\+|-)?[0-9]+)?""".r
  final val ProperlyClosedQuotedString = ".*[^\\\\](\\\\\\\\)*\"".r
  final val MultilineStringEnd = "\"{3,}".r

  final val ConfExt = "." + HoconFileType.DefaultExtension
  final val JsonExt = "." + JsonFileType.DEFAULT_EXTENSION
  final val PropsExt = ".properties"
}
