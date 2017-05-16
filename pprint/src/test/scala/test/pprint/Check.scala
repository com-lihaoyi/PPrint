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
      println(fansi.Color.Magenta("chunk: ") ++ fansi.Color.Green(Util.literalize(chunk.plainText)))
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
      println(fansi.Color.Yellow("char: ") ++ fansi.Color.Green(Util.literalize(char.toString)))
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
      i
    }
    def next() = if (completedLines < height - 1) {
      val chunk = chunks.next()


      val i = consumeChunkUntilLine(chunk, height - 1)
      if (completedLines == height - 1) {
        lastChunkLeftover = chunk.substring(i, chunk.length)
        chunk.substring(0, i)
      } else if (i == chunk.length) {
        if (!chunks.hasNext) lastLineFinished = true
        chunk
      } else {
        lastChunkLeftover = chunk.substring(i, chunk.length)
        chunk.substring(0, i)
      }
    }else if (!lastLineFinished) {
      val buffer = mutable.Buffer.empty[fansi.Str]
      var charsLeftOver = false
      val i = consumeChunkUntilLine(lastChunkLeftover, height)
      if (i < lastChunkLeftover.length) charsLeftOver = true
      if (i > 0) buffer.append(lastChunkLeftover.substring(0, i - 1))
      while(chunks.hasNext && completedLines < height){
        val chunk = chunks.next()
        val chars = chunk.getChars

        val i = consumeChunkUntilLine(chunk, height)

        if (i > 0) buffer.append(chunk.substring(0, i))
        else if (i < chars.length) {
          charsLeftOver = true
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