package tastyquery

import scala.language.unsafeNulls

import Symbols.*
import Flags.*

object PrivateAccess:
  private[tastyquery] class PrivateMethodCaller(x: AnyRef, methodName: String) {
    def apply(_args: Any*): Any = {
      val args = _args.map(_.asInstanceOf[AnyRef])
      def _parents: LazyList[Class[_]] = LazyList(x.getClass) #::: _parents.map(_.getSuperclass)
      val parents = _parents.takeWhile(_ != null).toList
      val methods = parents.flatMap(_.getDeclaredMethods)
      val method = methods.find(_.getName.split("\\$\\$") contains methodName).getOrElse(
      throw new IllegalArgumentException("Method " + methodName + s" not found, available: ${methods.map(_.getName).mkString(", ")}"))

      method.setAccessible(true)
      method.invoke(x, args : _*)
    }
  }

  private[tastyquery] class PrivateMethodExposer(x: AnyRef) {
    def apply(method: scala.Symbol): PrivateMethodCaller = new PrivateMethodCaller(x, method.name)
  }

  private [tastyquery] def p(x: AnyRef): PrivateMethodExposer = new PrivateMethodExposer(x)
end PrivateAccess

object Bridge:
  export Flags.*
  import PrivateAccess.*

  private val myFlags = new collection.mutable.HashMap[Symbol, FlagSet]

  extension (symbol: Symbol)
    private def computeFlags: FlagSet =
      p(symbol)(scala.Symbol("flags"))().asInstanceOf[FlagSet]

    private def getFlags: FlagSet =
      myFlags.getOrElseUpdate(symbol, computeFlags)
    
    // @deprecated("Last resort, define custom extension methods instead when possible")
    inline def is(flag: Flag): Boolean = getFlags.is(flag)
  end extension
end Bridge
  