package dotty.tools.dotc
package sbt

import scala.language.unsafeNulls

import tastyquery.Classpaths.*
import tastyquery.Contexts.*
import tastyquery.Symbols.*
import tastyquery.Names.*
import tastyquery.Trees.*
import tastyquery.Types.*
import tastyquery.Annotations.*
import tastyquery.Constants.*
import tastyquery.SourceLanguage
import tastyquery.Modifiers.*
import tastyquery.SourcePosition
import tastyquery.Exceptions.UnknownClasspathEntry

import tastyquery.Extensions.*

import xsbti.api as api
import dotty.tools.dotc.util.SourceFile

import scala.collection.mutable
import scala.util.hashing.MurmurHash3
import tastyquery.Modifiers.Variance

import core.Decorators.*
import dotty.tools.io.FileWriters.ReadOnlyContext
import dotty.tools.io.FileWriters.ctx as dottyCtx
import dotty.tools.dotc.core.StdNames.str

import java.util.concurrent.atomic.AtomicBoolean
import dotty.tools.dotc.printing.Formatting.ShownDef.Shown.runCtxShow
import dotty.tools.dotc.typer.ErrorReporting.err


class ExtractAPITasty:
  val hasErrors: AtomicBoolean = AtomicBoolean() // TODO store errors to report them later

  var mySourceAndClasses: List[(SourceFile, Seq[api.ClassLike])] = Nil
  
  private def listDecls(symbol: Symbol)(using Context): List[Symbol] = // TODO remove
    val decls = symbol match
      case symbol: DeclaringSymbol =>
        symbol.declarations.flatMap(listDecls)
      case _ => Nil

    symbol :: decls

  def runOn(entry: ClasspathEntry, cp: Classpath, relativePathToDottySource: Map[String, SourceFile],
            cb: interfaces.IncrementalCallback | Null)(using ReadOnlyContext): Unit =
    def withIncCallback(op: interfaces.IncrementalCallback => Unit): Unit =
      if cb != null then
        op(cb)
       
    given Context = Context.initialize(cp)

    val nonLocalClassSymbols = new mutable.HashMap[SourceFile, mutable.HashSet[ClassSymbol]]

    val symbols =
      try // Workaround, because tastyquery does not add entries with no packages to its lookup
        ctx.findSymbolsByClasspathEntry(entry).toList
      catch case e: UnknownClasspathEntry =>
        Nil

    val paths = symbols.map(_.tree.get.pos.sourceFile.path).toList

    val symbolsBySource = symbols.groupMap(symbol => {
      relativePathToDottySource(symbol.tree.get.pos.sourceFile.path)
    })(identity)
    
    
    for (source, symbols) <- symbolsBySource do
      withIncCallback: cb => 
        cb.startSource(source)

      val apiTraverser = new ExtractAPITastyCollector(source, nonLocalClassSymbols)
      val classes = apiTraverser.apiSource(symbols)
      val mainClasses = apiTraverser.mainClasses

      mySourceAndClasses ::= (source, classes)

      withIncCallback: cb =>
        classes.foreach(cb.api(source, _))
        mainClasses.foreach(cb.mainClass(source, _))
    
      if apiTraverser.hasErrors then
        hasErrors.set(true)

    withIncCallback(cb =>
      recordNonLocalClasses(nonLocalClassSymbols, cb)
      cb.apiPhaseCompleted()
    )
  end runOn

  private def recordNonLocalClasses(nonLocalClassSymbols: mutable.HashMap[SourceFile, mutable.HashSet[ClassSymbol]], cb: interfaces.IncrementalCallback)(using Context)(using ReadOnlyContext): Unit =
    for (sourceFile, clss) <- nonLocalClassSymbols do
      if sourceFile.exists then
        clss.foreach(recordNonLocalClass(_, sourceFile, cb))
    // ctx.run.nn.asyncTasty.foreach(_.signalAPIComplete())

  private def recordNonLocalClass(cls: ClassSymbol, sourceFile: SourceFile, cb: interfaces.IncrementalCallback)(using Context)(using ReadOnlyContext): Unit =
    def registerProductNames(fullClassName: String, binaryClassName: String) =
      import dotty.tools.io.JarArchive
      val pathToClassFile = s"${binaryClassName.replace('.', java.io.File.separatorChar)}.class"

      val classFile = {
        dottyCtx.settings.outputDir match {
          case jar: JarArchive =>
            // important detail here, even on Windows, Zinc expects the separator within the jar
            // to be the system default, (even if in the actual jar file the entry always uses '/').
            // see https://github.com/sbt/zinc/blob/dcddc1f9cfe542d738582c43f4840e17c053ce81/internal/compiler-bridge/src/main/scala/xsbt/JarUtils.scala#L47
            new java.io.File(s"$jar!$pathToClassFile")
          case outputDir =>
            new java.io.File(outputDir.file, pathToClassFile)
        }
      }

      cb.generatedNonLocalClass(sourceFile, classFile.toPath(), binaryClassName, fullClassName)
    end registerProductNames

    // val fullClassName = atPhase(sbtExtractDependenciesPhase) {
    //   ExtractDependencies.classNameAsString(cls)
    // }
    val fullClassName = cls.fullNameNoModuleClassSuffix
    // val binaryClassName = cls.binaryClassName
    val binaryClassName = cls.fullName // TODO
    registerProductNames(fullClassName, binaryClassName)

    // Register the names of top-level module symbols that emit two class files
    val isTopLevelUniqueModule =
      cls.owner.hack_isPackageClass && cls.isModuleClass && cls.companionClass.isEmpty // TODO
    if isTopLevelUniqueModule then
      registerProductNames(fullClassName, binaryClassName.stripSuffix(str.MODULE_SUFFIX))
  end recordNonLocalClass







end ExtractAPITasty


private class ExtractAPITastyCollector(source: SourceFile, nonLocalClassSymbols: mutable.HashMap[SourceFile, mutable.HashSet[ClassSymbol]])(using Context)(using ReadOnlyContext) extends ThunkHolder:
  import xsbti.api

  var hasErrors: Boolean = false

  private def log(msg: String, pos: SourcePosition = SourcePosition.NoPosition): Unit =
    dottyCtx.reporter.log(msg)

  private def debugLog(msg: => String, pos: SourcePosition = SourcePosition.NoPosition): Unit =
    if (dottyCtx.settings.debug) log(msg)

  private def warn(msg: String, pos: SourcePosition = SourcePosition.NoPosition): Unit =
    dottyCtx.reporter.warning(msg.toMessage)

  private def error(msg: String, pos: SourcePosition = SourcePosition.NoPosition): Unit =
    dottyCtx.reporter.error(msg.toMessage)
    hasErrors = true

  /** Report an internal error in incremental compilation. */
  private def internalError(msg: => String, pos: SourcePosition = SourcePosition.NoPosition): Unit = // TODO move method
    error(s"Internal error in the incremental compiler while compiling ${source}: $msg", pos)

  /** This cache is necessary for correctness, see the comment about inherited
   *  members in `apiClassStructure`
   */
  private val classLikeCache = new mutable.HashMap[ClassSymbol, api.ClassLikeDef]
  /** This cache is optional, it avoids recomputing representations */
  private val typeCache = new mutable.HashMap[TypeMappable, api.Type]
  /** This cache is necessary to avoid unstable name hashing when `typeCache` is present,
   *  see the comment in the `RefinedType` case in `computeType`
   *  The cache key is (api of RefinedType#parent, api of RefinedType#refinedInfo).
   */
  private val refinedTypeCache = new mutable.HashMap[(api.Type, api.Definition), api.Structure]

  /** This cache is necessary to avoid infinite loops when hashing an inline "Body" annotation.
   *  Its values are transitively seen inline references within a call chain starting from a single "origin" inline
   *  definition. Avoid hashing an inline "Body" annotation if its associated definition is already in the cache.
   *  Precondition: the cache is empty whenever we hash a new "origin" inline "Body" annotation.
   */
  private val seenInlineCache = mutable.HashSet.empty[Symbol]

  private val allNonLocalClassesInSrc = new mutable.HashSet[xsbti.api.ClassLike]
  private val _mainClasses = new mutable.HashSet[String]

  private object Constants {
    val emptyStringArray = Array[String]()
    val local            = api.ThisQualifier.create()
    val public           = api.Public.create()
    val privateLocal     = api.Private.create(local)
    val protectedLocal   = api.Protected.create(local)
    val unqualified      = api.Unqualified.create()
    val thisPath         = api.This.create()
    val emptyType        = api.EmptyType.create()
    val emptyModifiers   =
      new api.Modifiers(false, false, false, false, false,false, false, false)
  }

  /** Some Dotty types do not have a corresponding type in xsbti.api.* that
   *  represents them. Until this is fixed we can workaround this by using
   *  special annotations that can never appear in the source code to
   *  represent these types.
   *
   *  @param tp      An approximation of the type we're trying to represent
   *  @param marker  A special annotation to differentiate our type
   */
  private def withMarker(tp: api.Type, marker: api.Annotation) =
    api.Annotated.of(tp, Array(marker))
  private def marker(name: String) =
    api.Annotation.of(api.Constant.of(Constants.emptyType, name), Array())
  private val orMarker = marker("Or")
  private val byNameMarker = marker("ByName")
  private val matchMarker = marker("Match")
  private val superMarker = marker("Super")

  def apiSource(symbols: Iterable[Symbol]): Seq[api.ClassLike] = {
    def apiClasses(tree: Tree): Unit = tree match {
      case PackageDef(_, stats) =>
        stats.foreach(apiClasses)
      case tree: TypeDef =>
        apiClass(tree.symbol.asClass)
      case _ =>
    }

    symbols.foreach(_.tree foreach apiClasses)
    forceThunks()

    allNonLocalClassesInSrc.toSeq
  }

  def apiClass(sym: ClassSymbol): api.ClassLikeDef =
    classLikeCache.getOrElseUpdate(sym, computeClass(sym))

  def mainClasses: Set[String] = {
    forceThunks()
    _mainClasses.toSet
  }

  private def computeClass(sym: ClassSymbol): api.ClassLikeDef = {
    import xsbti.api.{DefinitionType => dt}
    val defType =
      if (sym.isTrait) dt.Trait
      else if (sym.isModuleClass) {
        // TODO PackageClass is when we have package object
        // TODO look at name
        // Instance of PackageSymbol
        // top level class (owner is a package symbol) that is a module class with the name ending in package or $package
        // dt.Module // TODO figure out PackageClass
        if (sym.hack_isPackageClass) dt.PackageModule
        else dt.Module
      } else dt.ClassDef

    val selfType = apiType(sym.givenSelfType.orNull)
    
    // import dotty.tools.dotc.core.Names as dottyNames
    // import dotty.tools.dotc.core.NameOps.*
    // val name = sym.fullName.stripModuleClassSuffix.toString
    // val name = dottyNames.typeName(sym.fullName).stripModuleClassSuffix.toString // TODO use underlying
    val name = sym.fullNameNoModuleClassSuffix
      // We strip module class suffix. Zinc relies on a class and its companion having the same name
    
    val tparams = sym.typeParams.map(apiTypeParameter).toArray
    
    val structure = apiClassStructure(sym)
    val acc = apiAccess(sym)
    val modifiers = apiModifiers(sym)
    val anns = apiAnnotations(sym, inlineOrigin = None).toArray
    val topLevel = sym.isTopLevel
    // TODO CHECK: does sealedChildren include this
    //     ANSWER: no, it doesn't
    val childrenOfSealedClass = sym.sealedDescendants.sorted(classFirstSort).map(c =>
      c match
        case c: ClassSymbol => apiType(c.typeRef)
        case c: TermSymbol => apiType(c.termRef)
    ).toArray

    val cl = api.ClassLike.of(
      name, acc, modifiers, anns, defType, api.SafeLazy.strict(selfType), api.SafeLazy.strict(structure), Constants.emptyStringArray,
      childrenOfSealedClass, topLevel, tparams)

    allNonLocalClassesInSrc += cl
    if !sym.isLocal then
      nonLocalClassSymbols.getOrElseUpdate(source, mutable.HashSet.empty) += sym

    if (sym.hack_isStatic && !sym.isTrait && sym.hasMainMethod) {
       // If sym is an object, all main methods count, otherwise only @static ones count.
      _mainClasses += name
    }

    api.ClassLikeDef.of(name, acc, modifiers, anns, tparams, defType)
  }

  def apiClassStructure(csym: ClassSymbol): api.Structure = {
    val bases = {
      val ancestorTypes0 = linearizedAncestorTypes(csym)
      // TODO handle value classes
      if (csym.hack_isDerivedValueClass) { // TODO (later) reimplement from ErasedValueClass
        // val underlying = ValueClasses.valueClassUnbox(csym).info.finalResultType
        val underlying = csym.declarations.find(_.isParamAccessor).get.localRef.finalResultType // TODO check
        // The underlying type of a value class should be part of the name hash
        // of the value class (see the test `value-class-underlying`), this is accomplished
        // by adding the underlying type to the list of parent types.
        underlying :: ancestorTypes0
      } else
        ancestorTypes0
      // ancestorTypes0
    }

    val apiBases = bases.map(apiType)

    // Synthetic methods that are always present do not affect the API
    // and can therefore be ignored.
    def alwaysPresent(s: TermOrTypeSymbol) = csym.isModuleClass && s.isConstructor
    val decls = csym.declarations
                    .filter(!alwaysPresent(_))
    val apiDecls = apiDefinitions(decls)

    val declSet = decls.toSet
    // TODO: We shouldn't have to compute inherited members. Instead, `Structure`
    // should have a lazy `parentStructures` field.
    val inherited = csym.linearization
      .filter(bc => bc.sourceLanguage != SourceLanguage.Scala2)
      .flatMap(_.declarations.filter(s => !(s.isPrivate || declSet.contains(s))))
    // Inherited members need to be computed lazily because a class might contain
    // itself as an inherited member, like in `class A { class B extends A }`,
    // this works because of `classLikeCache`
    val apiInherited = lzy(apiDefinitions(inherited).toArray)

    api.Structure.of(api.SafeLazy.strict(apiBases.toArray), api.SafeLazy.strict(apiDecls.toArray), apiInherited)
  }

  def linearizedAncestorTypes(csym: ClassSymbol): List[Type] = {
    val ref = csym.appliedRefInsideThis
    // Note that the ordering of classes in `linearization` is important.
    csym.linearization.tail.map(ref.baseType(_).get)
    // val ref = info.appliedRef
    // Note that the ordering of classes in `baseClasses` is important.
    // info.baseClasses.tail.map(ref.baseType)
  }

  // The hash generated by sbt for definitions is supposed to be symmetric so
  // we shouldn't have to sort them, but it actually isn't symmetric for
  // definitions which are classes, therefore we need to sort classes to
  // ensure a stable hash.
  // Modules and classes come first and are sorted by name, all other
  // definitions come later and are not sorted.
  private object classFirstSort extends Ordering[Symbol] {
    override def compare(a: Symbol, b: Symbol) = {
      val aIsClass = a.isClass
      val bIsClass = b.isClass
      if (aIsClass == bIsClass) {
        if (aIsClass) {
          val ac = a.asClass
          val bc = b.asClass
          if (ac.isModuleClass == bc.isModuleClass)
            ac.fullName.toString.compareTo(bc.fullName.toString)
          else if (ac.isModuleClass)
            -1
          else
            1
        } else
          0
      } else if (aIsClass)
      -1
    else
      1
    }
  }

  def apiDefinitions(defs: List[Symbol]): List[api.ClassDefinition] =
    defs.sorted(classFirstSort).map(apiDefinition(_, inlineOrigin = None))

  /** `inlineOrigin` denotes an optional inline method that we are
   *  currently hashing the body of. If it exists, include extra information
   *  that is missing after erasure
   */
  def apiDefinition(sym: Symbol, inlineOrigin: Option[Symbol]): api.ClassDefinition = {
    if (sym.isClass) {
      apiClass(sym.asClass)
    } else if (sym.isType) {
      apiTypeMember(sym.asType)
    } else {
      val trmsym = sym.asTerm
      trmsym.kind match {
        case TermSymbolKind.Var =>
          api.Var.of(sym.name.toString, apiAccess(sym), apiModifiers(sym),
            apiAnnotations(sym, inlineOrigin).toArray, apiType(trmsym.declaredType))
        case TermSymbolKind.Val =>
          api.Val.of(sym.name.toString, apiAccess(sym), apiModifiers(sym),
            apiAnnotations(sym, inlineOrigin).toArray, apiType(trmsym.declaredType))
        case _ =>
          apiDef(sym.asTerm, inlineOrigin)
      }
    }
  }

  /** `inlineOrigin` denotes an optional inline method that we are
   *  currently hashing the body of. If it exists, include extra information
   *  that is missing after erasure
   */
  def apiDef(sym: TermSymbol, inlineOrigin: Option[Symbol]): api.Def = {

    var seenInlineExtras = false
    var inlineExtras = 41

    def mixInlineParam(p: Symbol): Unit =
      if inlineOrigin.isDefined && p.isInline then
        seenInlineExtras = true
        inlineExtras = hashInlineParam(p, inlineExtras)

    def inlineExtrasAnnot: Option[api.Annotation] =
      val h = inlineExtras
      Option.when(seenInlineExtras) {
        marker(s"${MurmurHash3.finalizeHash(h, "inlineExtras".hashCode)}")
      }

    def tparamList(pt: TypeLambda): List[api.TypeParameter] =
      pt.paramNames.lazyZip(pt.paramInfos).map((pname, pbounds) =>
        apiTypeParameter(pname.toString, Variance.Invariant, pbounds.low, pbounds.high)
      )

    def eitherToUnion[A, B](either: Either[List[A], List[B]]): List[A | B] = either match {
        case Left(params) => params
        case Right(params) => params
      }
    
    def paramList(mt: MethodType, params: ParamSymbolsClause): api.ParameterList =
      val apiParams = eitherToUnion(params).lazyZip(mt.paramInfos).map((param, ptype) =>
                    mixInlineParam(param)
                    api.MethodParameter.of(
                      param.name.toString, apiType(ptype), param.isParamWithDefault, api.ParameterModifier.Plain))
      api.ParameterList.of(apiParams.toArray, mt.isImplicitOrContextual)

    def paramLists(t: TypeOrMethodic, paramss: List[ParamSymbolsClause]): List[api.ParameterList] = t match {
      case pt: TypeLambda =>
        paramLists(pt.resultType, paramss.drop(1))
      case mt: MethodType =>
        val pnames = mt.paramNames
        val ptypes = mt.paramInfos
        val restpe = mt.resultType
        val sameLength = eitherToUnion(paramss.head).hasSameLengthAs(pnames)
        assert(paramss.nonEmpty && sameLength,
          s"mismatch for $sym, ${sym.paramSymss}")
        paramList(mt, paramss.head) :: paramLists(restpe, paramss.tail)
      case _ =>
        Nil
    }

    /** returns list of pairs of 1: the position in all parameter lists, and 2: a type parameter list */
    def tparamLists(t: TypeOrMethodic, index: Int): List[(Int, List[api.TypeParameter])] = t match
      case pt: TypeLambda =>
        (index, tparamList(pt)) :: tparamLists(pt.resultType, index + 1)
      case mt: MethodType =>
        tparamLists(mt.resultType, index + 1)
      case _ =>
        Nil

    val (tparams, tparamsExtras) = sym.declaredType match
      case pt: TypeLambda =>
        (tparamList(pt), tparamLists(pt.resultType, index = 1))
      case mt: MethodType =>
        (Nil, tparamLists(mt.resultType, index = 1))
      case _ =>
        (Nil, Nil)

    val vparamss = paramLists(sym.declaredType, sym.paramSymss)
    val retTp = sym.declaredType.finalResultType.widenExpr

    val tparamsExtraAnnot = Option.when(tparamsExtras.nonEmpty) {
      marker(s"${hashTparamsExtras(tparamsExtras)("tparamsExtra".hashCode)}")
    }

    val annotations = inlineExtrasAnnot ++: tparamsExtraAnnot ++: apiAnnotations(sym, inlineOrigin)

    // api.Def.of(sym.zincMangledName.toString, apiAccess(sym), apiModifiers(sym),
    //   annotations.toArray, tparams.toArray, vparamss.toArray, apiType(retTp))
    // TODO no need for zincMangledName, Constructor names just need to be unique
    api.Def.of(sym.zincMangledName.toString, apiAccess(sym), apiModifiers(sym),
      annotations.toArray, tparams.toArray, vparamss.toArray, apiType(retTp))
  }

  def apiTypeMember(sym: TypeSymbol): api.TypeMember = {
    val typeParams = Array[api.TypeParameter]()
    val name = sym.name.toString
    val access = apiAccess(sym)
    val modifiers = apiModifiers(sym)
    val as = apiAnnotations(sym, inlineOrigin = None)
    val tpe = sym.localRef  // TODO check

    if (sym.isTypeAlias)
      api.TypeAlias.of(name, access, modifiers, as.toArray, typeParams, apiType(tpe.bounds.high))
    else {
      // assert(sym.isAbstractOrParamType)
      assert(sym.isAbstractMember || sym.isAbstractClass || sym.isInstanceOf[TypeParamSymbol]) // TODO check
      api.TypeDeclaration.of(name, access, modifiers, as.toArray, typeParams, apiType(tpe.bounds.low), apiType(tpe.bounds.high))
    }
    // api.TypeAlias.of(name, access, modifiers, as.toArray, typeParams, Constants.emptyType)
  }

  // Hack to represent dotty types which don't have an equivalent in xsbti
  def combineApiTypes(apiTps: api.Type*): api.Type = {
    api.Structure.of(api.SafeLazy.strict(apiTps.toArray),
      api.SafeLazy.strict(Array()), api.SafeLazy.strict(Array()))
  }

  def apiType(tp: TypeMappable | Null): api.Type = { // TOOD maybe use Option[TypeMappable]
    typeCache.getOrElseUpdate(tp, computeType(tp))
  }

  private def computeType(tp: TypeMappable | Null): api.Type = {
    // TODO: Never dealias. We currently have to dealias because
    // sbt main class discovery relies on the signature of the main
    // method being fully dealiased. See https://github.com/sbt/zinc/issues/102
    // val tp2 = if (!tp.isLambdaSub) tp.dealiasKeepAnnots else tp
    
    val tp2 = try
      if tp != null && !tp.hack_isLambdaSub && tp != defn.SyntacticNothingType /*FIXME workaround to avoid infinite recursion*/ then tp.hack_dealiasKeepAnnots else tp // FIXME isLambdaSub is private
    catch
      case e => 
        error(s"Error in computeType: $source, $tp")
        error(e.toString)
        error(e.getStackTrace.mkString("\n"))
        null
    // val tp2 = tp
    tp2 match { // TODO add more cases
      case NoPrefix /*| NoType*/ | null =>
        Constants.emptyType
      case pr: PackageRef =>
        val pathComponents =
          pr.fullyQualifiedName.toString.split('.')
            .map(api.Id.of)
            .map(_.asInstanceOf[api.PathComponent])
        api.Singleton.of(api.Path.of(pathComponents)) // TODO add Constants.thisPath
      case tp: NamedType =>
        // val sym = tp.localSymbol
        val sym = tp.optSymbol.get
        // A type can sometimes be represented by multiple different NamedTypes
        // (they will be `=:=` to each other, but not `==`), and the compiler
        // may choose to use any of these representation, there is no stability
        // guarantee. We avoid this instability by always normalizing the
        // prefix: if it's a package, if we didn't do this sbt might conclude
        // that some API changed when it didn't, leading to overcompilation
        // (recompiling more things than what is needed for incremental
        // compilation to be correct).

        // val prefix = if (sym.maybeOwner.is(Package)) // { type T } here T does not have an owner
        //   sym.owner.thisType
        // else
        //   tp.prefix

        // FIXME is not wrapped in a ThisType, because prefix is a package instead of a module class
        val prefix = if (sym.owner.isPackage) // { type T } here T does not have an owner
          sym.owner.asPackage.packageRef // TODO check if this is correct
        else
          tp.prefix

        // val prefix = tp.prefix
        // api.Projection.of(apiType(prefix), sym.name.toString)
        api.Projection.of(apiThis(sym.owner), sym.name.toString)
      case tp: AppliedType =>
        val tycon = tp.tycon
        val args = tp.args
        def processArg(arg: TypeOrWildcard): api.Type = arg match {
          case arg: WildcardTypeArg => // Handle wildcard parameters
            val bounds = arg.bounds
            if (bounds.isNothingAnyBounds) // TODO check if this is correct or use isDirectRef
              Constants.emptyType
            else {
              val lo = bounds.low
              val hi = bounds.high
              val name = "_"
              val ref = api.ParameterRef.of(name)
              api.Existential.of(ref,
                Array(apiTypeParameter(name, Variance.Invariant, lo, hi)))
            }
          case _ =>
            apiType(arg)
        }

        val apiTycon = apiType(tycon)
        val apiArgs = args.map(processArg)
        api.Parameterized.of(apiTycon, apiArgs.toArray)
      case tl: TypeLambda =>
        val apiTparams = tl.typeParams.map(apiTypeParameter)
        val apiRes = apiType(tl.resultType)
        api.Polymorphic.of(apiRes, apiTparams.toArray)
      case rt: RefinedType =>
        val name = rt.refinedName.toString
        val parent = apiType(rt.parent)

        def typeRefinement(name: String, tp: TypeBounds): api.TypeMember = tp match {
          case TypeAlias(alias) =>
            api.TypeAlias.of(name,
              Constants.public, Constants.emptyModifiers, Array(), Array(), apiType(alias))
          case tp: TypeBounds =>
            val lo = tp.low
            val hi = tp.high
            api.TypeDeclaration.of(name,
              Constants.public, Constants.emptyModifiers, Array(), Array(), apiType(lo), apiType(hi))
        }
        val decl = rt match {
          case rt: TypeRefinement =>
            typeRefinement(name, rt.refinedBounds)
          case _ =>
            // TODO proper logging
            debugLog(s"sbt-api: skipped structural refinement in $rt")
            // report.debuglog(i"sbt-api: skipped structural refinement in $rt")
            null
        }

        // Aggressive caching for RefinedTypes: `typeCache` is enough as long as two
        // RefinedType are `==`, but this is only the case when their `refinedInfo`
        // are `==` and this is not always the case, consider:
        //
        //     val foo: { type Bla = a.b.T }
        //     val bar: { type Bla = a.b.T }
        //
        // The sbt API representations of `foo` and `bar` (let's call them `apiFoo`
        // and `apiBar`) will both be instances of `Structure`. If `typeCache` was
        // the only cache, then in some cases we would have `apiFoo eq apiBar` and
        // in other cases we would just have `apiFoo == apiBar` (this happens
        // because the dotty representation of `a.b.T` is unstable, see the comment
        // in the `NamedType` case above).
        //
        // The fact that we may or may not have `apiFoo eq apiBar` is more than
        // an optimisation issue: it will determine whether the sbt name hash for
        // `Bla` contains one or two entries (because sbt `NameHashing` will not
        // traverse both `apiFoo` and `apiBar` if they are `eq`), therefore the
        // name hash of `Bla` will be unstable, unless we make sure that
        // `apiFoo == apiBar` always imply `apiFoo eq apiBar`. This is what
        // `refinedTypeCache` is for.
        refinedTypeCache.getOrElseUpdate((parent, decl), {
          val adecl: Array[api.ClassDefinition] = if (decl == null) Array() else Array(decl)
          api.Structure.of(api.SafeLazy.strict(Array(parent)), api.SafeLazy.strict(adecl), api.SafeLazy.strict(Array()))
        })
      case tp: RecType =>
        apiType(tp.parent)
      case tp: RecThis =>
        val recType = tp.binder
        // `tp` must be present inside `recType`, so calling `apiType` on
        // `recType` would lead to an infinite recursion, we avoid this by
        //  computing the representation of `recType` lazily.
        apiLazy(recType)
      case tp: AndType =>
        combineApiTypes(apiType(tp.first), apiType(tp.second))
      case tp: OrType =>
        val s = combineApiTypes(apiType(tp.first), apiType(tp.second))
        withMarker(s, orMarker)
      case tp: ByNameType =>
        withMarker(apiType(tp.resultType), byNameMarker)
      case tp: MatchType =>
        ???
        // val bound = tp.bound
        // val scrut = tp.scrutinee
        // val cases = tp.cases
        // val s = combineApiTypes(apiType(bound) :: apiType(scrut) :: cases.map(apiType)*)
        // withMarker(s, matchMarker)
      case tp: ConstantType =>
        val constant = tp.value
        api.Constant.of(apiType(constant.wideType), constant.stringValue)
      case tp: AnnotatedType =>
        val tpe = tp.typ
        val annot = tp.annotation
        api.Annotated.of(apiType(tpe), Array(apiAnnotation(annot)))
      case tp: ThisType =>
        apiThis(tp.cls)
      case tp: ParamRef =>
        val paramName = tp match
          // TODO: Distinguishing parameters based on their names alone is not enough,
          // the binder is also needed (at least for type lambdas).
          case tp: TypeParamRef => tp.paramName
          case tp: TermParamRef => tp.paramName
        api.ParameterRef.of(paramName.toString)
      // case tp: LazyRef =>
      //   apiType(tp.ref)
      // case tp: TypeVar =>
      //   apiType(tp.underlying)
      case tp: SuperType =>
        val thistpe = tp.thistpe
        val supertpe = tp.underlying
        val s = combineApiTypes(apiType(thistpe), apiType(supertpe))
        withMarker(s, superMarker)
      case tp: NothingType =>
        apiType(defn.SyntacticNothingType)
      case _ => {
        // internalError(i"Unhandled type $tp of class ${tp.getClass}")
        internalError(s"Unhandled type $tp of class ${tp.getClass}")
        Constants.emptyType
      }
    }
  }

  private def apiLazy(tp: => Type): api.Type = {
    // TODO: The sbt api needs a convenient way to make a lazy type.
    // For now, we repurpose Structure for this.
    val apiTp = lzy(Array(apiType(tp)))
    api.Structure.of(apiTp, api.SafeLazy.strict(Array()), api.SafeLazy.strict(Array()))
  }

  def apiThis(sym: Symbol): api.Singleton = {
    val pathComponents = sym.ownersIterator.takeWhile(!_.isEffectiveRoot)
      .map(s => api.Id.of(s.name.toString))
    api.Singleton.of(api.Path.of(pathComponents.toArray.reverse ++ Array(Constants.thisPath)))
  }

  def apiTypeParameter(tparam: TypeConstructorParam): api.TypeParameter =
    apiTypeParameter(tparam.name.toString, tparam.variance,
      tparam.declaredBounds.low, tparam.declaredBounds.high)

  def apiTypeParameter(name: String, variance: Variance, lo: Type, hi: Type): api.TypeParameter =
    api.TypeParameter.of(name, Array(), Array(), apiVariance(variance),
      apiType(lo), apiType(hi))

  def apiVariance(v: Variance): api.Variance = {
    import api.Variance.*
    v match
    case Variance.Invariant => Invariant
    case Variance.Covariant => Covariant
    case Variance.Contravariant => Contravariant
  }

  def apiAccess(sym: Symbol): api.Access = {
    // Symbols which are private[foo] do not have the flag Private set,
    // but their `privateWithin` exists, see `Parsers#ParserCommon#normalize`.
    import tastyquery.Modifiers.Visibility.*
    val visibility = sym match
      case sym: TermOrTypeSymbol => Some(sym.visibility)
      case _ => None
    if (visibility.contains(Public))
      Constants.public
    else if (visibility.contains(PrivateThis))
      Constants.privateLocal
    else if (visibility.contains(ProtectedThis))
      Constants.protectedLocal
    else {
      // val qualifier =
      //   if (sym.privateWithin eq NoSymbol)
      //     Constants.unqualified
      //   else
      //     api.IdQualifier.of(sym.privateWithin.fullName.toString)
      val qualifier =
        val scope = visibility.flatMap {
          case ScopedProtected(scope) => Some(scope)
          case ScopedPrivate(scope) => Some(scope)
          case _ => None
        }
        scope match
          case Some(scope) => api.IdQualifier.of(scope.fullName.toString)
          case _ =>
            Constants.unqualified
      if (visibility.exists{ case Protected | ProtectedThis | ScopedProtected => true; case _ => false})
        api.Protected.of(qualifier)
      else
        api.Private.of(qualifier)
    }
  }

  def apiModifiers(sym: Symbol): api.Modifiers = {
    // val absOver = sym.is(AbsOverride)
    // val abs = absOver || sym.isOneOf(Trait | Abstract | Deferred)
    // val over = absOver || sym.is(Override)
    // new api.Modifiers(abs, over, sym.is(Final), sym.is(Sealed),
    //   sym.isOneOf(GivenOrImplicit), sym.is(Lazy), sym.is(Macro), sym.isSuperAccessor)
    val absOver = sym.isAbstractOverride
    val abs = absOver || sym.isTrait || sym.isAbstractClass || sym.isAbstractMember
    val over = absOver || sym.hack_isOverride // TODO ignore override
    
    val isFinal = sym.isFinal
    val isSealed = sym.isSealed
    val isGivenOrImplicit = sym.isGivenOrUsing || sym.isImplicit

    val isLazy = sym.isLazyVal

    val isMacro = sym.isMacro
    
    val isSuperAccessor = sym.name match
      case _: SuperAccessorName => true
      case _ => false
    

    new api.Modifiers(abs, over, isFinal, isSealed,
      isGivenOrImplicit, isLazy, isMacro, isSuperAccessor)
  }

  /** `inlineOrigin` denotes an optional inline method that we are
   *  currently hashing the body of.
   */
  def apiAnnotations(s: Symbol, inlineOrigin: Option[Symbol]): List[api.Annotation] = {
    val annots = new mutable.ListBuffer[api.Annotation]
    // val inlineBody = Inlines.bodyToInline(s)
    // if !inlineBody.isEmpty then
    //   // If the body of an inline def changes, all the reverse dependencies of
    //   // this method need to be recompiled. sbt has no way of tracking method
    //   // bodies, so we include the hash of the body of the method as part of the
    //   // signature we send to sbt.

    //   def hash[U](inlineOrigin: Option[Symbol]): Int =
    //     assert(seenInlineCache.add(s)) // will fail if already seen, guarded by treeHash
    //     treeHash(inlineBody, inlineOrigin)

    //   val inlineHash =
    //     if inlineOrigin.exists then hash(inlineOrigin)
    //     else inlineBodyCache.getOrElseUpdate(s, hash(inlineOrigin = s).tap(_ => seenInlineCache.clear()))

    //   annots += marker(inlineHash.toString)

    // end if

    // In the Scala2 ExtractAPI phase we only extract annotations that extend
    // StaticAnnotation, but in Dotty we currently pickle all annotations so we
    // extract everything, except:
    // - annotations missing from the classpath which we simply skip over
    // - inline body annotations which are handled above
    // - the Child annotation since we already extract children via
    //   `api.ClassLike#childrenOfSealedClass` and adding this annotation would
    //   lead to overcompilation when using zinc's
    //   `IncOptions#useOptimizedSealed`.
    s.annotations.foreach { annot =>
      val sym = annot.symbol
      // if sym.exists && sym != defn.BodyAnnot && sym != defn.ChildAnnot then
      //   annots += apiAnnotation(annot)
      if sym.sourceLanguage != SourceLanguage.Java then // FIXME workaround for Java annotations
        annots += apiAnnotation(annot)
    }

    annots.toList
  }

  /** Produce a hash for a tree that is as stable as possible:
   *  it should stay the same across compiler runs, compiler instances,
   *  JVMs, etc.
   *
   * `inlineOrigin` denotes an optional inline method that we are hashing the body of, where `tree` could be
   * its body, or the body of another method referenced in a call chain leading to `inlineOrigin`.
   *
   * If `inlineOrigin` is NoSymbol, then tree is the tree of an annotation.
   */
  def treeHash(tree: Tree, inlineOrigin: Option[Symbol]): Int =
    def nameHash(n: Name, initHash: Int): Int =
      val h = n match
        case _: TermName =>
          MurmurHash3.mix(initHash, TermNameHash)
        case _ =>
          MurmurHash3.mix(initHash, TypeNameHash)

      // The hashCode of the name itself is not stable across compiler instances
      MurmurHash3.mix(h, n.toString.hashCode)
    end nameHash

    def constantHash(c: Constant, initHash: Int): Int =
      var h = MurmurHash3.mix(initHash, c.tag)
      c.tag match
        case NullTag =>
          // No value to hash, the tag is enough.
        case ClazzTag =>
          // Go through `apiType` to get a value with a stable hash, it'd
          // be better to use Murmur here too instead of relying on
          // `hashCode`, but that would essentially mean duplicating
          // https://github.com/sbt/zinc/blob/develop/internal/zinc-apiinfo/src/main/scala/xsbt/api/HashAPI.scala
          // and at that point we might as well do type hashing on our own
          // representation.
          h = MurmurHash3.mix(h, apiType(c.typeValue).hashCode)
        case _ =>
          h = MurmurHash3.mix(h, c.value.hashCode)
      h
    end constantHash

    def cannotHash(what: String, elem: Any, tree: Tree): Unit =
      internalError(s"Don't know how to produce a stable hash for $what", tree.pos)
      // internalError(i"Don't know how to produce a stable hash for $what", pos.sourcePos)

    def positionedHash(p: Tree, initHash: Int): Int =
      var h = initHash

      // p match
      //   case p: ast.WithLazyFields => p.forceFields()
      //   case _ =>

      if inlineOrigin.isDefined then
        p match
          case ref: TermReferenceTree @unchecked =>
            val sym = ref.symbol
            // if sym.is(Inline, butNot = Param) && !seenInlineCache.contains(sym) then
            if sym.isInline && !seenInlineCache.contains(sym) then  // TODO butNot = Param
              // An inline method that calls another inline method will eventually inline the call
              // at a non-inline callsite, in this case if the implementation of the nested call
              // changes, then the callsite will have a different API, we should hash the definition
              h = MurmurHash3.mix(h, apiDefinition(sym, inlineOrigin).hashCode)
          case _ =>

      // FIXME: If `p` is a tree we should probably take its type into account
      // when hashing it, but producing a stable hash for a type is not trivial
      // since the same type might have multiple representations, for method
      // signatures this is already handled by `computeType` and the machinery
      // in Zinc that generates hashes from that, if we can reliably produce
      // stable hashes for types ourselves then we could bypass all that and
      // send Zinc hashes directly.
      h = MurmurHash3.mix(h, p.productPrefix.hashCode)
      iteratorHash(p.productIterator, h)
    end positionedHash

    def iteratorHash(it: Iterator[Any], initHash: Int): Int =
      var h = initHash
      while it.hasNext do
        it.next() match
          case p: Tree =>
            h = positionedHash(p, h)
          case xs: List[?] =>
            h = iteratorHash(xs.iterator, h)
          case c: Constant =>
            h = constantHash(c, h)
          case n: NamedType =>
            h = nameHash(n.name, h)
          case n: Name =>
            h = nameHash(n, h)
          case elem =>
            cannotHash(what = s"`${elem}` of unknown class ${elem.getClass}", elem, tree)
      h
    end iteratorHash

    val seed = 4 // https://xkcd.com/221
    val h = positionedHash(tree, seed)
    MurmurHash3.finalizeHash(h, 0)
  end treeHash

  /** Hash secondary type parameters in separate marker annotation.
   *  We hash them separately because the position of type parameters is important.
   */
  private def hashTparamsExtras(tparamsExtras: List[(Int, List[api.TypeParameter])])(initHash: Int): Int =

    def mixTparams(tparams: List[api.TypeParameter])(initHash: Int) =
      var h = initHash
      var elems = tparams
      while elems.nonEmpty do
        h = MurmurHash3.mix(h, elems.head.hashCode)
        elems = elems.tail
      h

    def mixIndexAndTparams(index: Int, tparams: List[api.TypeParameter])(initHash: Int) =
      mixTparams(tparams)(MurmurHash3.mix(initHash, index))

    var h = initHash
    var extras = tparamsExtras
    var len = 0
    while extras.nonEmpty do
      h = mixIndexAndTparams(index = extras.head(0), tparams = extras.head(1))(h)
      extras = extras.tail
      len += 1
    MurmurHash3.finalizeHash(h, len)
  end hashTparamsExtras

  /** Mix in the name hash also because otherwise switching which
   *  parameter is inline will not affect the hash.
   */
  private def hashInlineParam(p: Symbol, h: Int) =
    MurmurHash3.mix(p.name.toString.hashCode, MurmurHash3.mix(h, InlineParamHash))

  private def apiAnnotation(annot: Annotation): api.Annotation = {
    // Like with inline defs, the whole body of the annotation and not just its
    // type is part of its API so we need to store its hash, but Zinc wants us
    // to extract the annotation type and its arguments, so we use a dummy
    // annotation argument to store the hash of the tree. We still need to
    // extract the annotation type in the way Zinc expects because sbt uses this
    // information to find tests to run (for example junit tests are
    // annotated @org.junit.Test).
    api.Annotation.of(
      apiType(annot.tree.tpe), // Used by sbt to find tests to run
      Array(api.AnnotationArgument.of("TREE_HASH", treeHash(annot.tree, inlineOrigin = None).toString)))
  }

end ExtractAPITastyCollector