package test.pprint
import pprint.PPrinter

class Check(width: Int = 100, height: Int = 99999, renderTwice: Boolean = false){
  def apply(t: Any, expected: String*) = {
    val printers =
      if (!renderTwice) Seq(PPrinter.BlackWhite)
      else Seq(PPrinter.BlackWhite, PPrinter.Color)
    // Make sure we
    for (pprinter <- printers){
      val pprinted = fansi.Str.join(
        PPrinter.BlackWhite.tokenize(t, width, height).toStream:_*
      ).plainText

      utest.assert(expected.map(_.trim).contains(pprinted))
    }
  }
}
