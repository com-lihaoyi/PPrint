package pprint

/**
  * A lazy AST representing pretty-printable text. Models `foo(a, b)`
  * `foo op bar`, and terminals `foo` in both lazy and eager forms
  */
sealed trait Tree
object Tree{

  /**
    * Foo(aa, bbb, cccc)
    */
  case class Apply(prefix: String,
                   body: Iterator[Tree]) extends Tree

  /**
    * LHS op RHS
    */
  case class Infix(lhs: Tree, op: String, rhs: Tree) extends Tree

  /**
    * "xyz"
    */
  case class Literal(body: String) extends Tree{
    val hasNewLine = body.exists(c => c == '\n' || c == '\r')
  }

  /**
    * x = y
    */
  case class KeyValue(key: String, value: Tree) extends Tree

  /**
    * xyz
    */
  case class Lazy(body0: Ctx => Iterator[String]) extends Tree

  case class Ctx(width: Int,
                 leftOffset: Int,
                 indentCount: Int,
                 indentStep: Int,
                 literalColor: fansi.Attrs,
                 applyPrefixColor: fansi.Attrs)
}

abstract class Walker{
  val tuplePrefix = "scala.Tuple"
  def showFieldNames = true
  def escapeUnicode = false
  def additionalHandlers: PartialFunction[Any, Tree]
  def treeify(x: Any): Tree = additionalHandlers.lift(x).getOrElse{
    x match{

      case null => Tree.Literal("null")
      case x: Char =>
        val sb = new StringBuilder
        sb.append('\'')
        Util.escapeChar(x, sb, escapeUnicode)
        sb.append('\'')
        Tree.Literal(sb.toString)
      case x: Byte => Tree.Literal(x.toString)
      case x: Short => Tree.Literal(x.toString)
      case x: Int => Tree.Literal(x.toString)
      case x: Long => Tree.Literal(x.toString + "L")
      case x: Float => Tree.Literal(x.toString + "F")
      case x: Double => Tree.Literal(x.toString)
      case x: String =>
        if (x.exists(c => c == '\n' || c == '\r')) Tree.Literal("\"\"\"" + x + "\"\"\"")
        else Tree.Literal(Util.literalize(x, escapeUnicode))

      case x: Symbol => Tree.Literal("'" + x.name)

      case x: scala.collection.Map[_, _] =>
        Tree.Apply(
          StringPrefix(x),
          x.iterator.flatMap { case (k, v) =>
            Seq(Tree.Infix(treeify(k), "->", treeify(v)))
          }
        )

      case x: Iterable[_] => Tree.Apply(StringPrefix(x), x.iterator.map(x => treeify(x)))

      case None => Tree.Literal("None")

      case it: Iterator[_] =>
        // required since 2.13
        if (it.isEmpty)
          Tree.Literal("empty iterator")
        else
          Tree.Literal("non-empty iterator")

      case x: Array[_] => Tree.Apply("Array", x.iterator.map(x => treeify(x)))

      case x: Product =>
        val className = x.getClass.getName
        if (x.productArity == 0) Tree.Lazy(ctx => Iterator(x.toString))
        else if(x.productArity == 2 && Util.isOperator(x.productPrefix)){
          Tree.Infix(
            treeify(x.productElement(0)),

            x.productPrefix,
            treeify(x.productElement(1))
          )
        } else (className.startsWith(tuplePrefix), className.lift(tuplePrefix.length)) match{
          // leave out tuple1, so it gets printed as Tuple1(foo) instead of (foo)
          // Don't check the whole suffix, because of specialization there may be
          // funny characters after the digit
          case (true, Some('2' | '3' | '4' | '5' | '6' | '7' | '8' | '9')) =>
            Tree.Apply("", x.productIterator.map(x => treeify(x)))

          case _ =>
            Tree.Apply(x.productPrefix, ProductSupport.treeifyProductElements(x, this))
        }

      case x => Tree.Lazy(ctx => Iterator(x.toString))
    }
  }


}
