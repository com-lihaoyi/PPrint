package pprint

trait TPrintLowPri{
  inline given default[T]: TPrint[T] = ${ TPrintLowPri.typePrintImpl[T] }
}

object TPrintLowPri{

  import scala.quoted._
  import sourcecode.Text.generate


  extension (expr: Expr[String]) {
    def +(other: Expr[String])(using Quotes): Expr[String] =
      '{ $expr + $other }
  }

  extension (exprs: List[Expr[String]]) {
    def mkStringExpr(sep: String)(using Quotes): Expr[String] =
      exprs match {
        case expr :: Nil =>
          expr
        case _ =>
          exprs.reduceLeft { (l, r) => l + Expr(sep) + r }
      }
  }

  def typePrintImpl[T](using Quotes, Type[T]): Expr[TPrint[T]] = {

    import quotes.reflect._
    import util._

    def literalColor(cfg: Expr[TPrintColors], s: Expr[fansi.Str]) = {
      '{ $cfg.typeColor($s).render }
    }

    def printSymString(cfg: Expr[TPrintColors], s: String) =
      if (s.toString.startsWith("_$")) "_"
      else s.toString.stripSuffix(".type")

    def printBounds(cfg: Expr[TPrintColors])(lo: TypeRepr, hi: TypeRepr) = {
      val loTree =
        if (lo =:= TypeRepr.of[Nothing]) None else Some(Expr(" >: ") + rec0(cfg)(lo) )
      val hiTree =
        if (hi =:= TypeRepr.of[Any]) None else Some(Expr(" <: ") + rec0(cfg)(hi) )
      val underscore = Expr("_")
      loTree.orElse(hiTree).map(underscore + _).getOrElse(underscore)
    }

    def printSym(cfg: Expr[TPrintColors], s: String): Expr[String] = {
      val expr = Expr(s)
      literalColor(cfg, '{ fansi.Str($expr) })
    }

    //TODO: We don't currently use this method
    def prefixFor(cfg: Expr[TPrintColors])(pre: TypeTree, sym: String): Expr[String] = {
      // Depending on what the prefix is, you may use `#`, `.`
      // or even need to wrap the prefix in parentheses
      val sep = pre match {
        case x if x.toString.endsWith(".type") =>
          rec0(cfg)(pre.tpe) + Expr(".")
      }
      sep + printSym(cfg, sym)
    }


    def printArgs(cfg: Expr[TPrintColors])(args: List[TypeRepr]): Expr[String] = {
      val added = args.map {
        case TypeBounds(lo, hi) =>
          printBounds(cfg)(lo, hi)
        case tpe: TypeRepr =>
          rec0(cfg)(tpe, false)
      }.mkStringExpr(", ")
      Expr("[") + added + Expr("]")
    }


    object RefinedType {
      def unapply(tpe: TypeRepr): Option[(TypeRepr, List[(String, TypeRepr)])] = tpe match {
        case Refinement(p, i, b) =>
          unapply(p).map {
            case (pp, bs) => (pp, (i -> b) :: bs)
          }.orElse(Some((p, (i -> b) :: Nil)))
        case _ => None
      }
    }

    def rec0(cfg: Expr[TPrintColors])(tpe: TypeRepr, end: Boolean = false): Expr[String] = tpe match {
      case TypeRef(NoPrefix(), sym) =>
        printSym(cfg, sym)
        // TODO: Add prefix handling back in once it works!
      case TypeRef(_, sym) =>
        printSym(cfg, sym)
      case AppliedType(tpe, args) =>
        printSym(cfg, tpe.typeSymbol.name) + printArgs(cfg)(args)
      case RefinedType(tpe, refinements) =>
        val pre = rec0(cfg)(tpe)
        lazy val defs = refinements.collect {
          case (name, tpe: TypeRepr) =>
            Expr("type " + name + " = ") + rec0(cfg)(tpe)
          case (name, TypeBounds(lo, hi)) =>
            Expr("type " + name) + printBounds(cfg)(lo, hi) + rec0(cfg)(tpe)
        }.mkStringExpr("; ")
        pre + (if(refinements.isEmpty) '{ "" } else Expr("{") + defs + Expr("}"))
      case AnnotatedType(parent, annot) =>
        rec0(cfg)(parent, end)
      case _ =>
        Expr(Type.show[T])
    }
    '{
      new TPrint[T] {
        final def render(implicit cfg: TPrintColors): String = ${ rec0('cfg)(TypeRepr.of[T]) }
      }
    }
  }
}
