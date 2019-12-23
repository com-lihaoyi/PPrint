package pprint

/**
  *
  * @param defaultWidth How wide to allow a pretty-printed value to become
  *                     before wrapping
  * @param defaultHeight How tall to allow the pretty-printed output to become
  *                      before truncated it with a `...`
  * @param defaultIndent How many spaces to indent each nested [[Tree.Apply]] by
  * @param colorLiteral What color to assign to literals like `"lol"` or 31337
  * @param colorApplyPrefix What color to assign to `Foo` in `Foo(bar, baz)`
  * @param additionalHandlers Provide this to override how certain types are
  *                           pretty-printed at runtime
  */
case class PPrinter(defaultWidth: Int = 100,
                    defaultHeight: Int = 500,
                    defaultIndent: Int = 2,
                    colorLiteral: fansi.Attrs = fansi.Color.Green,
                    colorApplyPrefix: fansi.Attrs = fansi.Color.Yellow,
                    additionalHandlers: PartialFunction[Any, Tree] = PartialFunction.empty)
  extends Walker{ outer =>

  /**
    * How to convert a thing into a [[Tree]] that can be pretty-printed.
    */
  override def treeify(x: Any) = super.treeify(x)

  /**
    * Logs a given value to stdout with some metadata to identify where the log
    * message came from. Hard-coded and not very flexible, but you can easily
    * implement your own log method if you want to customize it further.
    */
  def log[T](x: sourcecode.Text[T],
             tag: String = "",
             width: Int = defaultWidth,
             height: Int = defaultHeight,
             indent: Int = defaultIndent,
             initialOffset: Int = 0)
            (implicit line: sourcecode.Line,
             fileName: sourcecode.FileName): T = {

    def joinSeq[T](seq: Seq[T], sep: T): Seq[T] = {
      seq.flatMap(x => Seq(x, sep)).dropRight(1)
    }

    val tagStrs =
      if (tag.isEmpty) Seq()
      else Seq(fansi.Color.Cyan(tag), fansi.Str(" "))

    val prefix = Seq(
      fansi.Color.Magenta(fileName.value),
      fansi.Str(":"),
      fansi.Color.Green(line.value.toString),
      fansi.Str(" "),
      fansi.Color.Cyan(x.source),
      fansi.Str(": ")
    ) ++ tagStrs
    val str = fansi.Str.join(prefix ++ tokenize(x.value, width, height, indent).toSeq:_*)

    println(str)
    x.value
  }

  /**
    * Converts an [[Any]] into a large colored `fansi.Str`
    */
  def apply(x: Any,
            width: Int = defaultWidth,
            height: Int = defaultHeight,
            indent: Int = defaultIndent,
            initialOffset: Int = 0): fansi.Str = {
    fansi.Str.join(this.tokenize(x, width, height, indent).toSeq:_*)
  }

  /**
    * Converts an [[Any]] into a large colored `fansi.Str`
    */
  def pprintln[T](x: T,
                  width: Int = defaultWidth,
                  height: Int = defaultHeight,
                 indent: Int = defaultIndent,
                 initialOffset: Int = 0): Unit = {
    tokenize(x, width, height, indent, initialOffset).foreach(print)
    println()
    x
  }

  /**
    * Converts an [[Any]] into an iterator of colored chunks, wrapped at a
    * certain width and truncated at a certain height
    */
  def tokenize(x: Any,
               width: Int = defaultWidth,
               height: Int = defaultHeight,
               indent: Int = defaultIndent,
               initialOffset: Int = 0): Iterator[fansi.Str] = {

    // The three stages within the pretty-printing process:

    // Convert the Any into a lazy Tree of `Apply`, `Infix` and `Lazy`/`Strict` literals
    val tree = this.treeify(x)
    // Render the `Any` into a stream of tokens, properly indented and wrapped
    // at the given width
    val renderer = new Renderer(width, colorApplyPrefix, colorLiteral, indent)
    val rendered = renderer.rec(tree, initialOffset, 0).iter
    // Truncate the output stream once it's wrapped-at-width height goes
    // beyond the desired height
    val truncated = new Truncated(rendered, width, height)
    truncated
  }
}

object PPrinter {
  object Color extends PPrinter
  object BlackWhite extends PPrinter(
    colorLiteral = fansi.Attrs.Empty,
    colorApplyPrefix = fansi.Attrs.Empty
  )
}
