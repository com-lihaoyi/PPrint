package test.pprint
import pprint.PPrinter

object Check{
  val blackWhite = new PPrinter(){
    override def showFieldNames = false
  }
  val color = new PPrinter(){
    override def showFieldNames = false
  }
  val blackWhiteFields = new PPrinter()
  val colorFields = new PPrinter()
}
class Check(width: Int = 100, height: Int = 99999, renderTwice: Boolean = false, fields: Boolean = false){
  def apply(t: Any, expected: String*) = {

    val blackWhite = if (fields) Check.blackWhiteFields else Check.blackWhite
    val color = if (fields) Check.colorFields else Check.color
    val printers =
      if (!renderTwice) Seq(blackWhite)
      else Seq(blackWhite, color)
    // Make sure we
    for (pprinter <- printers){
      val pprinted = fansi.Str.join(blackWhite.tokenize(t, width, height).toStream:_*).plainText

      utest.assert(expected.map(_.trim).contains(pprinted))
    }
  }
}
