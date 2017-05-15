package ammonite.pprint

import scala.annotation.switch

object Util{
  def escapeChar(c: Char,
                 sb: StringBuilder,
                 unicode: Boolean = true) = (c: @switch) match {
    case '"' => sb.append("\\\"")
    case '\\' => sb.append("\\\\")
    case '\b' => sb.append("\\b")
    case '\f' => sb.append("\\f")
    case '\n' => sb.append("\\n")
    case '\r' => sb.append("\\r")
    case '\t' => sb.append("\\t")
    case c =>
      if (c < ' ' || (c > '~' && unicode)) sb.append("\\u%04x" format c.toInt)
      else sb.append(c)
  }


  /**
    * Convert a string to a C&P-able literal. Basically
    * copied verbatim from the uPickle source code.
    */
  def literalize(s: IndexedSeq[Char], unicode: Boolean = true) = {
    val sb = new StringBuilder
    sb.append('"')
    var i = 0
    val len = s.length
    while (i < len) {
      Util.escapeChar(s(i), sb)
      i += 1
    }
    sb.append('"')

    sb.result()
  }
}
