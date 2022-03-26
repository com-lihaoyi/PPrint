package pprint
import language.experimental.macros
import reflect.macros.blackbox.Context
import scala.reflect.macros.TypecheckException

trait TPrintLowPri{
  implicit def default[T]: TPrint[T] = macro TPrintLowPri.typePrintImpl[T]
}
object TPrintLowPri{
  sealed trait WrapType
  object WrapType{
    case object NoWrap extends WrapType
    case object Infix extends WrapType
    case object Tuple extends WrapType
  }
  def typePrintImpl[T: c.WeakTypeTag](c: Context): c.Expr[TPrint[T]] = {
    // Used to provide "empty string" values in quasiquotes

    import c.universe._
    val tpe = weakTypeOf[T]
    val rendered = typePrintImplRec(c)(tpe, rightMost = true).render
    val res = c.Expr[TPrint[T]](
      q"_root_.pprint.TPrint.recolor(_root_.fansi.Str($rendered))"
    )
    res
  }

  val functionTypes = Range.inclusive(0, 22).map(i => s"scala.Function$i").toSet
  val tupleTypes = Range.inclusive(0, 22).map(i => s"scala.Tuple$i").toSet

  def typePrintImplRec[T](c: Context)(tpe: c.Type, rightMost: Boolean): fansi.Str = {
    typePrintImplRec0(c)(tpe, rightMost)._1
  }
  def typePrintImplRec0[T](c: Context)(tpe: c.Type, rightMost: Boolean): (fansi.Str, WrapType) = {
    import c.universe._
    def printSymString(s: Symbol) =
      if (s.name.decodedName.toString.startsWith("_$")) "_"
      else s.name.decodedName.toString.stripSuffix(".type")

    def literalColor(s: fansi.Str): fansi.Str = fansi.Color.Green(s)
    def printSym(s: Symbol): fansi.Str = literalColor(printSymString(s))


    def printSymFull(s: Symbol): fansi.Str = {
      if (lookup(s)) printSym(s)
      else printSymFull(s.owner) ++ "." ++ printSym(s)
    }

    /**
      * Looks up a symbol in the enclosing scope and returns
      * whether it exists in scope by the same name
      */
    def lookup(s: Symbol) = {
      val cas = c.asInstanceOf[reflect.macros.runtime.Context]
      val g = cas.global
      val gName = s.name.asInstanceOf[g.Name]
      val lookedUps = for(n <- Stream(gName.toTermName, gName.toTypeName)) yield {
        cas.callsiteTyper
          .context
          .lookupSymbol(n, _ => true)
          .symbol
      }

      if (!s.isType) {
        // Try to de-reference `val` references
        lookedUps.exists(x =>
          x == s || x.tpe.termSymbol == s.asTerm
        )
      } else {
        // Try to resolve aliases for types
        lookedUps.exists(x =>
          x == s || x.tpe.typeSymbol == s.asInstanceOf[g.Symbol].tpe.typeSymbol
        )}
    }

    def prefixFor(pre: Type, sym: Symbol): fansi.Str = {
      // Depending on what the prefix is, you may use `#`, `.`
      // or even need to wrap the prefix in parentheses
      val sep = pre match{
        case x if x.toString.endsWith(".type") => typePrintImplRec(c)(pre, false) ++ "."
        case x: TypeRef => literalColor(typePrintImplRec(c)(pre, true)) ++ "#"
        case x: SingleType => literalColor(typePrintImplRec(c)(pre, false)) ++ "."
        case x: ThisType => literalColor(typePrintImplRec(c)(pre, false)) ++ "."
        case x => fansi.Str("(") ++ typePrintImplRec(c)(pre, true) ++ ")#"
      }

      val prefix = if (!lookup(sym)) sep else fansi.Str("")
      prefix ++ printSym(sym)
    }


    def printArgSyms(args: List[Symbol]): fansi.Str = {
      def added = args
        .map{x =>
          val TypeBounds(lo, hi) = x.info
          printSym(x) ++ printBounds(lo, hi)
        }
        .reduceLeft[fansi.Str]((l, r) => l ++ ", " ++ r)

      if (args == Nil) fansi.Str("") else fansi.Str("[") ++ added ++ "]"
    }
    def printArgs(args: List[Type]): fansi.Str = {
      def added = args.map(typePrintImplRec(c)(_, true))
        .reduceLeft[fansi.Str]((l, r) => l ++ ", " ++ r)

      if (args == Nil) fansi.Str("") else fansi.Str("[") ++ added ++ "]"
    }

    def printBounds(lo: Type, hi: Type) = {
      val loTree = if (lo =:= typeOf[Nothing]) fansi.Str("") else fansi.Str(" >: ") ++ typePrintImplRec(c)(lo, true)
      val hiTree = if (hi =:= typeOf[Any]) fansi.Str("") else fansi.Str(" <: ") ++ typePrintImplRec(c)(hi, true)
      loTree ++ hiTree
    }

    def showRefinement(quantified: List[Symbol]) = {
      def stmts = for{
        t <- quantified
        suffix <- t.info match {
          case PolyType(typeParams, resultType) =>
            val paramTree = printArgSyms(t.asInstanceOf[TypeSymbol].typeParams)
            val resultBounds =
              if (resultType =:= typeOf[Any]) fansi.Str("")
              else fansi.Str(" <: ") ++ typePrintImplRec(c)(resultType, true)

            Some(paramTree ++ resultBounds)
          case TypeBounds(lo, hi)
            if t.toString.contains("$") && lo =:= typeOf[Nothing] && hi =:= typeOf[Any] =>
            None
          case TypeBounds(lo, hi) =>
            Some( printBounds(lo, hi) )
        }
      } yield {
        if (t.toString.endsWith(".type")) {
          val TypeBounds(lo, hi) = t.info
          val RefinedType(parents, defs) = hi
          val filtered = internal.refinedType(
            parents.filter(x => !(x =:= typeOf[scala.Singleton])),
            defs
          )

          fansi.Str("val ") ++ literalColor(t.name.toString.stripSuffix(".type")) ++
          ": " ++ typePrintImplRec(c)(filtered, true)
        }else {
          fansi.Str("type ") ++ printSym(t) ++ suffix
        }
      }
      if (stmts.length == 0) None
      else Some(stmts.reduceLeft((l, r) => l + "; " + r))
    }

    tpe match {
      case TypeBounds(lo, hi) =>
        val res = printBounds(lo, hi)
        (fansi.Str("_") ++ res, WrapType.NoWrap)
      case ThisType(sym) =>
        (printSymFull(sym) + (if(sym.isPackage || sym.isModuleClass) "" else ".this.type"), WrapType.NoWrap)

      case SingleType(NoPrefix, sym)    => (printSym(sym) ++ (if (rightMost) ".type" else ""), WrapType.NoWrap)
      case SingleType(pre, sym)         => (prefixFor(pre, sym) ++ (if (rightMost) ".type" else ""), WrapType.NoWrap)
      // Special-case operator two-parameter types as infix
      case TypeRef(pre, sym, List(left, right))
        if lookup(sym) && sym.name.encodedName.toString != sym.name.decodedName.toString =>

        (
          typePrintImplRec(c)(left, true) ++ " " ++ printSym(sym) ++ " " ++ typePrintImplRec(c)(right, true),
          WrapType.Infix
        )

      case TypeRef(pre, sym, args) if functionTypes.contains(sym.fullName) =>
        args match{
          case Seq(r) => (fansi.Str("() => ") ++ typePrintImplRec(c)(r, true), WrapType.Infix)

          case many =>
            val (left, leftWrap) = typePrintImplRec0(c)(many.head, true)

            if (many.size == 2 && leftWrap == WrapType.NoWrap){
              (left ++ " => " ++ typePrintImplRec(c)(many(1), true), WrapType.Infix)
            }else (
              fansi.Str("(") ++
              fansi.Str.join(
                (left +: many.init.tail.map(typePrintImplRec(c)(_, true))),
                sep = ", "

              ) ++
              ") => " ++ typePrintImplRec(c)(many.last, true),
              WrapType.Infix
            )
        }
      case TypeRef(pre, sym, args) if tupleTypes.contains(sym.fullName) =>
        (
          fansi.Str("(") ++
          fansi.Str.join(args.map(typePrintImplRec(c)(_, true)), sep = ", ") ++
          ")",
          WrapType.Tuple
        )

      case TypeRef(NoPrefix, sym, args) => (printSym(sym) ++ printArgs(args), WrapType.NoWrap)
      case TypeRef(pre, sym, args)      =>
        if (sym.fullName == "scala.<byname>") (fansi.Str("=> ") ++ typePrintImplRec(c)(args(0), true), WrapType.Infix)
        else (prefixFor(pre, sym) ++ printArgs(args), WrapType.NoWrap)
      case et @ ExistentialType(quantified, underlying) =>
        (
          showRefinement(quantified) match{
            case None => typePrintImplRec(c)(underlying, true)
            case Some(block) => typePrintImplRec(c)(underlying, true) ++ " forSome { " ++ block ++ " }"
          },
          WrapType.NoWrap
        )
      case AnnotatedType(annots, tp)    =>
        val mapped = annots.map(x => " @" + typePrintImplRec(c)(x.tpe, true))
          .reduceLeft((x, y) => x + y)

        (
          typePrintImplRec(c)(tp, true) + mapped,
          WrapType.NoWrap
        )
      case RefinedType(parents, defs) =>
        val pre =
          if (parents.forall(_ =:= typeOf[AnyRef])) ""
          else parents
            .map(typePrintImplRec(c)(_, true))
            .reduceLeft[fansi.Str]((l, r) => l  ++ " with " ++ r)
        (pre + (if (defs.isEmpty) "" else "{" ++ defs.mkString(";") ++ "}"), WrapType.NoWrap)
      case PolyType(typeParams, resultType) =>
        val params = printArgSyms(typeParams)
        (
          params ++ typePrintImplRec(c)(resultType, true),
          WrapType.NoWrap
        )
      case ConstantType(value) =>
        val pprintedValue =
          pprint.PPrinter.BlackWhite.copy(colorLiteral = fansi.Color.Green).apply(value.value)

        (pprintedValue, WrapType.NoWrap)
    }
  }

}
