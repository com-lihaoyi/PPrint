package test.pprint
import pprint.PPrint
import utest._
class Check(width: Int = 100, height: Int = 9999){
  def truncate(input: Iterator[fansi.Str]): Iterator[fansi.Str] = {

  }
  def apply(t: Any, expected: String*) = {

    val pprinted = PPrint.BlackWhite.tokenize(t, width)
    val output = new StringBuilder()
    val lineLengths = collection.mutable.Buffer(0)
    var previousSlashN = false
    var previousSlashR = false
    def handleNormalChar(char: Char) = {
      previousSlashN = false
      previousSlashR = false
      output.append(char)
      if (char == '\n' && previousSlashR || char == '\r' && previousSlashN){
        // do nothing
      }else if (char == '\n'){
        previousSlashN = true
        lineLengths.append(0)
      } else if(char == '\r') {
        previousSlashR = true
        lineLengths.append(0)
      }
      else if (lineLengths.last == width) lineLengths.append(0)
      else lineLengths(lineLengths.length-1) += 1
    }

    def completedLines = lineLengths.length-1

    var finishedChunk = false
    while(pprinted.hasNext && completedLines < height){
      val chunk = pprinted.next()
      val chars = chunk.getChars
      var i = 0
      finishedChunk = false
      while(i < chars.length && completedLines < height){
        val char = chars(i)
        handleNormalChar(char)
        i += 1
      }
      if (i == chars.length) finishedChunk = true
    }


    val string =
      if (!pprinted.hasNext && finishedChunk) output.mkString
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