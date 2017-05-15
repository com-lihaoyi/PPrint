package ammonite.pprint

/**
  * Created by lihaoyi on 15/5/17.
  */
class PPrint extends Walker{
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

  def tokenize(x: Any, maxWidth: Int = 100, indent: Int = 0): Iterator[fansi.Str] = {
    val tree = treeify(x)
    val renderer = new Renderer(maxWidth, colorApplyPrefix, colorLiteral, indentStr)
    renderer.rec(tree, indent * 2, indent).iter
  }
}

object PPrint {
  object Color extends PPrint
  object BlackWhite extends PPrint{
    override val colorLiteral = fansi.Attrs.Empty
    override val colorApplyPrefix = fansi.Attrs.Empty
  }
}