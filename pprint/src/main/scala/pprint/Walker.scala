package ammonite.pprint


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
    * xyz
    */
  case class Lazy(body0: () => String) extends Tree{
    lazy val body = body0()
    lazy val hasNewLine = body.exists(c => c == '\n' || c == '\r')
  }
}

class Walker{
  def println(x: Any) = {
    Predef.println(System.nanoTime() + "\t" + x)
  }
  val tuplePrefix = "scala.Tuple"
  def treeify(x: Any): Tree = {
    println("Treeify! " + x.isInstanceOf[Iterable[_]])
    x match{
//
      case null =>
        println("null")
        Tree.Literal("null")
      case x: Char =>
        println("char")
        val sb = new StringBuilder
        sb.append(''')
        Util.escapeChar(x, sb)
        sb.append(''')
        Tree.Literal(sb.toString)
      case x: Byte => println("byte"); Tree.Literal(x.toString)
      case x: Short => println("short"); Tree.Literal(x.toString)
      case x: Int => println("int"); Tree.Literal(x.toString)
      case x: Long => println("long"); Tree.Literal(x.toString + "L")
      case x: Float => println("float"); Tree.Literal(x.toString + "F")
      case x: Double => println("double"); Tree.Literal(x.toString)
      case x: String =>
        println("string")
        if (x.exists(c => c == '\n' || c == '\r')) Tree.Literal("\"\"\"" + x + "\"\"\"")
        else Tree.Literal(Util.literalize(x))

      case x: Symbol =>
        println("symbol")
        Tree.Literal(x.toString)

      case x: scala.collection.Map[_, _] =>
        println("map")
        Tree.Apply(
          x.stringPrefix,
          x.iterator.flatMap { case (k, v) =>
            Seq(Tree.Infix(treeify(k), "->", treeify(v)))
          }
        )

      case x: Iterable[_] =>
        println("iterable ")
        Tree.Apply(x.stringPrefix, x.iterator.map(x => treeify(x)))

      case None =>
        println("none")
        Tree.Literal("None")

      case x: Array[_] =>
        println("array")
        Tree.Apply("Array", x.iterator.map(x => treeify(x)))

      case x: Product =>
        println("product")
        val className = x.getClass.getName
        if (x.productArity == 0) Tree.Lazy(x.toString)
        else (className.startsWith(tuplePrefix), className.lift(tuplePrefix.length)) match{
          // leave out tuple1, so it gets printed as Tuple1(foo) instead of (foo)
          // Don't check the whole suffix, because of specialization there may be
          // funny characters after the digit
          case (true, Some('2' | '3' | '4' | '5' | '6' | '7' | '8' | '9')) =>
            Tree.Apply("", x.productIterator.map(x => treeify(x)))

          case _ =>
            Tree.Apply(x.productPrefix, x.productIterator.map(x => treeify(x)))
        }

      case x =>
        println("fallthrough")
        Tree.Lazy(() => x.toString)
    }
  }


}