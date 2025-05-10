package pprint

package object interpolation {

  def interpolate(pprinter: PPrinter, sc: StringContext, args: Any*): String = {
    val sb = new StringBuilder()
    val partsIterator = sc.parts.iterator
    val argsIterator = args.iterator

    if (partsIterator.hasNext) {
      sb.append(partsIterator.next())
    }

    while (argsIterator.hasNext) {
      val arg = argsIterator.next()
      sb.append(pprinter(arg).render)
      if (partsIterator.hasNext) {
        sb.append(partsIterator.next())
      }
    }

    while (partsIterator.hasNext) {
      sb.append(partsIterator.next())
    }

    sb.toString()
  }

  implicit class BlackWhiteContext(self: StringContext) extends AnyVal {
    def bw(args: Any*): String = interpolate(PPrinter.BlackWhite, self, args)
  }
  implicit class ColorContext(self: StringContext) extends AnyVal {
    def clr(args: Any*): String = interpolate(PPrinter.Color, self, args)
  }
}
