package ammonite.pprint

/**
  * Created by lihaoyi on 15/5/17.
  */
class PPrint extends Walker{
  /**
    * How to convert a thing into a [[Tree]] that can be pretty-printed.
    */
  override def treeify(x: Any) = super.treeify(x)

  val colorLiteral: fansi.Attrs = fansi.Color.Green

  val colorApplyPrefix: fansi.Attrs = fansi.Color.Yellow

  val indentStr: String = "  "

  def tokenize(x: Any, maxWidth: Int = 100, indent: Int = 0): Iterator[fansi.Str] = {
    println(x)
    println("Treeifying")
    val tree = treeify(x)
    println("new Renderer")
//    val renderer = new Renderer(maxWidth, colorApplyPrefix, colorLiteral, indentStr)
//    println("rendering")
//    renderer.rec(tree, indent * 2, indent).iter
    ???
  }

}
object PPrint {
  object Color extends PPrint
  object BlackWhite extends PPrint{
    override val colorLiteral = fansi.Attrs.Empty
    override val colorApplyPrefix = fansi.Attrs.Empty
  }
}