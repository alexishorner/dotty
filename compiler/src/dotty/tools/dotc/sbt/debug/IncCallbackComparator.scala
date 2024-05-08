package dotty.tools.dotc.sbt.debug
import scala.collection.immutable.Queue
import IncCallbackResults.*
import dotty.tools.dotc.sbt.interfaces
import scala.annotation.constructorOnly
import dotty.tools.dotc.sbt.DefaultShowAPI

class IncCallbackComparator:
  val apiCallback = new RecordingIncCallback
  val apiTastyCallback = new RecordingIncCallback

  def diffAndPropagate(cb: interfaces.IncrementalCallback | Null): Either[IncDiff, Unit] =
    val sourceFiles = apiCallback.results.keySet ++ apiTastyCallback.results.keys
    val merged = sourceFiles.map(sourceFile => {
      val apiResult = apiCallback.results(sourceFile).map(_.toString)
      val apiTastyResult = apiTastyCallback.results(sourceFile).map(_.toString)
      val diff = apiResult.zipAll(apiTastyResult, NoResult.toString, NoResult.toString)
                          .map((a, b) => if a == b then Identical(a) else Different(a, b))
      (sourceFile, diff)
    }).toMap
    
    val diff = IncDiff.groupIdentical(merged)
    if diff.hasDifferences then
      Left(diff)
    else
      Right(
        if cb != null then
          for (source, results) <- apiTastyCallback.results do
            for result <- results do
              result match
                case StartSource(sourceFile) => cb.startSource(sourceFile)
                case Api(sourceFile, classApi) => cb.api(sourceFile, classApi)
                case MainClass(sourceFile, className) => cb.mainClass(sourceFile, className)
        else ())

  sealed trait IncDiffEntry
  case class Different(expected: String, actual: String) extends IncDiffEntry:
    override def toString: String = s"Expected:\n$expected\nActual:\n$actual"
  end Different
  case class Identical(result: String) extends IncDiffEntry:
    override def toString: String = s"Identical value:\n$result"

  class IncDiff(private val groupedEntries: Map[StartSource, Queue[Queue[IncDiffEntry]]]):
    def hasDifferences: Boolean = groupedEntries.values.exists(groups => groups.exists(group => group.exists { case Different(_, _) => true; case _ => false }))
    def fullTrace: String = traceImpl {
      case (head: Identical) +: Queue() => s"${head}\n"
      case group => group.mkString("\n")
    }
    def collapsedTrace: String = traceImpl {
      case (head: Identical) +: Queue() => s"${head}\n"
      case group @ (head: Identical) +: tail => s"<${group.size} identical entries>\n"
      case group => group.mkString("\n")
    }
    def traceImpl(groupCollapser: Queue[IncDiffEntry] => String): String =
      val sb = new StringBuilder
      groupedEntries.foreach((source, groups) =>
        sb ++= s"Source: ${source.sourceFile}\n"
        groups.foreach(group => sb ++= groupCollapser(group))
        sb ++= "\n"
      )
      sb.toString

  end IncDiff

  object IncDiff:
    def groupIdentical(entries: Map[StartSource, Queue[IncDiffEntry]]) =
      def groupEntries(diff: Queue[IncDiffEntry]) =
        diff.tail.foldLeft(Queue(diff.take(1))) {
          case (acc @ (group @ groupHead +: _) +: accTail, entry) => 
              if (entry == groupHead) then accTail :+ (group :+ entry)
              else acc :+ (Queue(entry))
          case _ => throw new IllegalStateException("Impossible")
        }
      end groupEntries

      val groupedEntries = entries.view.mapValues(groupEntries).toMap
      new IncDiff(groupedEntries)
  end IncDiff
end IncCallbackComparator
