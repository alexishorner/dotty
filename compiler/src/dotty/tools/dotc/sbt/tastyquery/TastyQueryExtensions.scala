package tastyquery

import Contexts.*
import Symbols.*
import Types.*
import Names.*
import Modifiers.*

import Bridge.*

object CommonNames:
  val main: TermName = termName("main")
end CommonNames

object Extensions: 
  extension (name: Name)(using Context)
    def toTermName: TermName = name match
      case name: TypeName => name.toTermName
      case name: TermName => name
    def toTypeName: TypeName = name match
      case name: TypeName => name
      case name: TermName => name.toTypeName
  end extension

  extension (sym: Symbol)(using Context)
    def fullName: String =
      val b = new StringBuilder
      def build(sym: Symbol): Unit = {
        if (sym.owner != null)
          build(sym.owner)
          b append "."
        b append sym.name 
      }
      build(sym)
      b.toString
    end fullName

    def thisType: Prefix = sym match
      case sym: ClassSymbol => sym.thisType
      case _ => NoPrefix
    end thisType

    /** The chain of owners of this denotation, starting with the denoting symbol itself */
    def ownersIterator: Iterator[Symbol] = new Iterator[Symbol] {
      private var current: Symbol | Null = sym
      def hasNext = current != null
      def next: Symbol = {
        val result = current.nn
        current = result.owner
        result
      }
    }

    /** Is this symbol the root class or its companion object? */
    def isRoot: Boolean =
      (sym.owner == null) && sym.name.toTermName == Names.nme.RootName || sym.name == Names.nme.UserLandRootPackageName

    /** Is this symbol the empty package class or its companion object? */
    def isEmptyPackage: Boolean =
      val owner = sym.owner
      sym.name.toTermName == Names.nme.EmptyPackageName && owner != null && owner.isRoot

    /** Is this symbol the empty package class or its companion object? */
    def isEffectiveRoot: Boolean = sym.isRoot || sym.isEmptyPackage

    def isTopLevelClass: Boolean =
      true // TODO implement
    end isTopLevelClass

    def hack_isOverride: Boolean =
      sym.is(Override)
    end hack_isOverride

    /* Public equivalent of `Symbol.isStatic`. We need another name, because the extension cannot have the same name as a method. */
    def hack_isStatic: Boolean =
      sym.isStatic
    end hack_isStatic

    private inline def predicateAs[T <: Symbol](inline p: T => Boolean): Boolean = (sym: @unchecked) match
        case sym: T => p(sym)
        case _ => false
    end predicateAs

    // `ClassSymbol` predicates
    def isTrait: Boolean =
      predicateAs[ClassSymbol](_.isTrait)
    end isTrait

    def isAbstractClass: Boolean =
      predicateAs[ClassSymbol](_.isAbstractClass)
    end isAbstractClass
    
    private inline def hasOpenLevel(inline level: OpenLevel): Boolean =
      predicateAs[ClassSymbol](_.openLevel == level)
    end hasOpenLevel

    def isFinal: Boolean =
      hasOpenLevel(OpenLevel.Final)
    end isFinal
    
    def isSealed: Boolean =
      hasOpenLevel(OpenLevel.Sealed)
    end isSealed

    // `TermSymbol` predicates

    def isAbstractOverride: Boolean =
      predicateAs[TermSymbol](_.isAbstractOverride)
    end isAbstractOverride

    def isAbstractMember: Boolean =
      predicateAs[TermSymbol](_.isAbstractMember)
    end isAbstractMember

    def isGivenOrUsing: Boolean =
      predicateAs[TermSymbol](_.isGivenOrUsing)
    end isGivenOrUsing

    def isImplicit: Boolean =
      predicateAs[TermSymbol](_.isImplicit)
    end isImplicit

    private inline def hasKind(kind: TermSymbolKind): Boolean =
      predicateAs[TermSymbol](_.kind == kind)
    end hasKind

    def isLazyVal: Boolean =
      hasKind(TermSymbolKind.LazyVal)
    end isLazyVal

    def isMacro: Boolean =
      predicateAs[TermSymbol](_.isMacro)
    end isMacro

  end extension

  extension(sym: ClassSymbol)(using Context)
    def typeRef: TypeRef =
      TypeRef(sym.owner.thisType, sym)
    end typeRef

    def sealedDescendants: List[ClassSymbol | TermSymbol] =
      sym :: sym.sealedChildren // TODO reimplement like in dotty
    end sealedDescendants

    def hasMainMethod: Boolean =
      sym.getMember(CommonNames.main) match
        case Some(main) =>
          main.isMainMethod && (sym.isModuleClass || sym.isStatic)
        case None => false
      end match
    end hasMainMethod

    def hack_isDerivedValueClass: Boolean =
      sym.isDerivedValueClass
    end hack_isDerivedValueClass
  end extension

  extension(sym: TermOrTypeSymbol)(using Context)
    def isConstructor: Boolean =
      sym.name == Names.nme.Constructor // FIXME why do we need `Names` prefix ?
    end isConstructor
  end extension

  extension(sym: TermSymbol)(using Context)
    def termRef: TermRef =
      TermRef(sym.owner.thisType, sym)
    end termRef

    def isMainMethod: Boolean =
      (sym.name == CommonNames.main) && (sym.declaredType match {
        case MethodTypeExtractor(_, List(el), restpe) =>
          (el isSameType defn.StringType) && (restpe isTypeRefOf defn.UnitClass)
        case _ => false
      })
    end isMainMethod
  end extension

  extension (tpe: TypeOrMethodic)(using Context)
    def isDirectRef[U <: TypeOrMethodic](tpe2: U): Boolean = false

    def isTypeRefOf(cls: ClassSymbol): Boolean = tpe match
      case TypeRef.OfClass(tpeCls) => tpeCls == cls
      case _                       => false
    end isTypeRefOf
  end extension

  private object MethodTypeExtractor:
    def unapply(mt: MethodType): Some[(List[TermName], List[Type], TypeOrMethodic)] =
      Some((mt.paramNames, mt.paramInfos, mt.resultType))
    end unapply
end Extensions
