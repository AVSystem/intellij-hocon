package org.jetbrains.plugins.hocon
package lang

import com.github.ghik.silencer.silent
import com.intellij.openapi.fileTypes.{FileTypeConsumer, FileTypeFactory}

@silent("deprecated")
class HoconFileTypeFactory extends FileTypeFactory {
  def createFileTypes(consumer: FileTypeConsumer): Unit =
    consumer.consume(HoconFileType, HoconFileType.DefaultExtension)
}
