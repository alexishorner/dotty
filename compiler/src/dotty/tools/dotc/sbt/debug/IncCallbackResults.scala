package dotty.tools.dotc.sbt.debug

import scala.collection.immutable.Queue

import xsbti.api.ClassLike
import dotty.tools.dotc.util.SourceFile
import dotty.tools.io.FileExtension

object IncCallbackResults:
  trait IncResultEntry

  case class StartSource(sourceFile: SourceFile | Null) extends IncResultEntry
  case class Api(sourceFile: SourceFile | Null, classApi: ClassLike | Null) extends IncResultEntry
  case class MainClass(sourceFile: SourceFile | Null, className: String | Null) extends IncResultEntry
  case object NoResult extends IncResultEntry
end IncCallbackResults
