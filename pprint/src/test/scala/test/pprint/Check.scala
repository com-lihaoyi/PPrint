package test.pprint
import pprint.{PPrint, Util}
import utest._

import scala.collection.mutable
class Check(width: Int = 100, height: Int = 9999){
  def nonEmptyIterator(input0: Iterator[fansi.Str]) = new Iterator[fansi.Str]{
    val input = input0.buffered
    def skipEmpty() = {
      while(input.hasNext && input.head.length == 0) input.next()
    }
    def hasNext = {
      skipEmpty()
      input.hasNext
    }

    def next() = {
      skipEmpty()
      val chunk = input.next()
      chunk
    }
  }
  def truncate(chunks0: Iterator[fansi.Str]): Iterator[fansi.Str] = new Iterator[fansi.Str]{
    val chunks = nonEmptyIterator(chunks0)
    val lineLengths = collection.mutable.Buffer(0)

    var previousSlashN = false
    var previousSlashR = false
    def handleNormalChar(char: Char) = {
      previousSlashN = false
      previousSlashR = false
      if (char == '\n' && previousSlashR || char == '\r' && previousSlashN){
        // do nothing
      }else if (char == '\n'){
        previousSlashN = true
        lineLengths.append(0)
      } else if(char == '\r') {
        previousSlashR = true
        lineLengths.append(0)
      }
      else if (lineLengths.last == width) lineLengths.append(1)
      else lineLengths(lineLengths.length-1) += 1

    }

    def completedLines = lineLengths.length-1

    var finishedChunk = false

    var lastLineFinished = false
    var lastChunkLeftover = fansi.Str("")


    def hasNext = (chunks.hasNext && completedLines < height - 1) || !lastLineFinished


    def consumeChunkUntilLine(chunk: fansi.Str, lineLimit: Int) ={
      var i = 0
      val chars = chunk.getChars
      while (i < chars.length && completedLines < lineLimit) {
        val char = chars(i)
        handleNormalChar(char)
        i += 1
      }
      if (i == chunk.length) None else Some(i)
    }

    def next() = if (completedLines < height - 1) {
      val chunk = chunks.next()
      consumeChunkUntilLine(chunk, height - 1) match{
        case None =>
          if (!chunks.hasNext) lastLineFinished = true
          chunk
        case Some(i) =>
          // chunk was partially consumed. This should only happen if the chunk
          // is overshooting the vertical limit

          // If the last line is not empty, it means there is a character
          // on that last line. In such a case
          val splitPoint = if (lineLengths.last != 0) i - 1 else i
          lastChunkLeftover = chunk.substring(splitPoint, chunk.length)
          chunk.substring(0, splitPoint)
      }

    }else if (!lastLineFinished) {
      val buffer = mutable.Buffer.empty[fansi.Str]
      var charsLeftOver = false
      consumeChunkUntilLine(lastChunkLeftover, height) match{
        case None => buffer.append(lastChunkLeftover)
        case Some(i) =>
          charsLeftOver = true
          buffer.append(lastChunkLeftover.substring(0, i - 1))
      }
      while(chunks.hasNext && completedLines < height){
        val chunk = chunks.next()

        consumeChunkUntilLine(chunk, height) match{
          case None => buffer.append(chunk)
          case Some(i) =>
            charsLeftOver = true
            buffer.append(chunk.substring(0, i))
        }

      }

      lastLineFinished = true

      if (charsLeftOver || chunks.hasNext) fansi.Str("...")
      else buffer.map(_.render).mkString
    }else{
      throw new java.util.NoSuchElementException("next on empty iterator")
    }
  }

  def apply(t: Any, expected: String*) = {

    val pprinted = PPrint.BlackWhite.tokenize(t, width)
    val string = truncate(pprinted).map(_.plainText).mkString

    utest.asserts.assert(expected.map(_.trim).contains(string))
  }


}