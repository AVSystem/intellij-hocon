package intellijhocon

import com.intellij.openapi.fileTypes.{FileTypeConsumer, FileTypeFactory}

class HoconFileTypeFactory extends FileTypeFactory {
  def createFileTypes(consumer: FileTypeConsumer) =
    consumer.consume(HoconFileType, HoconFileType.DefaultExtension)
}
