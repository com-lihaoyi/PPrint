package test.pprint
import ammonite.pprint.PPrint
import utest._
class Check(width: Int = 100, height: Int = 9999){
  def apply(t: Any, expected: String*) = {

    println("pprinting...")
    val pprinted0 = PPrint.BlackWhite
    println("pprinted0")
    val pprinted1 = pprinted0.tokenize(t, width)

    println("pprinted1 " + pprinted1)

    val pprinted = pprinted1.flatMap(_.plainText.toCharArray)
    val output = new StringBuilder()
    val lineLengths = collection.mutable.Buffer(0)
    def handleNormalChar(char: Char) = {
      output.append(char)
      if (char == '\n' || char == '\r'){
        lineLengths.append(0)
      }else if (lineLengths.last == width){
        lineLengths.append(0)
      }else{
        lineLengths(lineLengths.length-1) += 1
      }
    }
    def completedLines = lineLengths.length-1
    while(pprinted.hasNext && completedLines < height){
      val char = pprinted.next()

      char match {
        case '\n' if pprinted.hasNext =>
          pprinted.next() match{
            case '\r' =>
              lineLengths.append(0)

              output.append("\n\r")
            case c2 =>
              handleNormalChar(char)
              if (completedLines < height) handleNormalChar(c2)
          }
        case '\r' if pprinted.hasNext =>
          pprinted.next() match{
            case '\n' =>
              lineLengths.append(0)
              output.append("\r\n")
            case c2 =>
              handleNormalChar(char)
              if (completedLines < height) handleNormalChar(c2)
          }
        case _ => handleNormalChar(char)
      }
    }


    val string =
      if (!pprinted.hasNext) output.mkString
      else {
        assert(completedLines == height)
        // -2 because it's not the last, empty line, but the one
        // before that
        val lastCompletedLineLength = lineLengths(lineLengths.length-2)

        val cutOffset = output.length-1 - lastCompletedLineLength

        output.replace(cutOffset, output.length, "...").mkString
      }


    utest.asserts.assert(expected.map(_.trim).contains(string))
  }


}