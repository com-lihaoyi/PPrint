package pprint


class PPrinter extends Walker{
  /**
    * How to convert a thing into a [[Tree]] that can be pretty-printed.
    * Override me if you want to have customized pretty-printing for some
    * specific types, and delegate the default case to `super.treeify`
    * for the rest.
    */
  override def treeify(x: Any) = super.treeify(x)

  /**
    * What color should literals be in the pretty-printed output?
    */
  val colorLiteral: fansi.Attrs = fansi.Color.Green

  /**
    * What color should Foo in Foo(bar, baz) be in the pretty-printed output?
    */
  val colorApplyPrefix: fansi.Attrs = fansi.Color.Yellow

  /**
    * How much to indent the output by each time?
    */
  val indentStr: String = "  "

  /**
    * Logs a given value to stdout with some metadata to identify where the log
    * message came from. Hard-coded and not very flexible, but you can easily
    * implement your own log method if you want to customize it further.
    */
  def log(x: sourcecode.Text[Any],
          tag: String = "",
          width: Int = 100,
          height: Int = 50,
          indent: Int = 2)
         (implicit line: sourcecode.Line,
          enclosing: sourcecode.Enclosing): Unit = {




    def joinSeq[T](seq: Seq[T], sep: T): Seq[T] = {
      seq.flatMap(x => Seq(x, sep)).dropRight(1)
    }
    val enclosingWithoutAnonfuns =
      enclosing.value.split(' ').filter(_ != "$anonfun").mkString(" ")
    val coloredEnclosing = joinSeq(
      enclosingWithoutAnonfuns.split('.').map( x =>
        joinSeq(x.split('#').map(fansi.Color.Magenta(_)), fansi.Str("#"))
      ),
      Seq(fansi.Str("."))
    ).flatten

    val tagStrs =
      if (tag.isEmpty) Seq()
      else Seq(fansi.Color.Cyan(tag), fansi.Str(" "))

    val prefix = coloredEnclosing ++ Seq(
      fansi.Str(":"),
      fansi.Color.Green(line.value.toString),
      fansi.Str(" "),
      fansi.Color.Cyan(x.source),
      fansi.Str(": ")
    ) ++ tagStrs
    val str = fansi.Str.join(prefix ++ tokenize(x.value, width, height, indent).toSeq:_*)

    println(str)
  }

  /**
    * Converts an [[Any]] into a large colored `fansi.Str`
    */
  def apply(x: Any, width: Int = 100, height: Int = 50, indent: Int = 2): fansi.Str = {
    fansi.Str.join(tokenize(x, width, height, indent).toSeq:_*)
  }

  /**
    * Converts an [[Any]] into an iterator of colored chunks, wrapped at a
    * certain width and truncated at a certain height
    */
  def tokenize(x: Any,
               width: Int = 100,
               height: Int = 50,
               indent: Int = 2): Iterator[fansi.Str] = {

    // The three stages within the pretty-printing process:

    // Convert the Any into a lazy Tree of `Apply`, `Infix` and `Lazy`/`Strict` literals
    val tree = treeify(x)
    // Render the `Any` into a stream of tokens, properly indented and wrapped
    // at the given width
    val renderer = new Renderer(width, colorApplyPrefix, colorLiteral, " " * indent)
    val rendered = renderer.rec(tree, 0, 0).iter
    // Truncate the output stream once it's wrapped-at-width height goes
    // beyond the desired height
    val truncated = new Truncated(rendered, width, height)
    truncated
  }
}

object PPrinter {
  object Color extends PPrinter
  object BlackWhite extends PPrinter{
    override val colorLiteral = fansi.Attrs.Empty
    override val colorApplyPrefix = fansi.Attrs.Empty
  }
}