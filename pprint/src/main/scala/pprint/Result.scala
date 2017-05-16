package pprint

import scala.annotation.switch


/**
  * The intermediate return type of the pretty-print system: provides an
  * iterator which produces the actual string output, as well as metadata
  * around that output that is only available after the iterator is exhausted
  */
class Result(val iter: Iterator[fansi.Str],
             hasNewline0: => Boolean,
             lastLineLength0: => Int){
  lazy val hasNewline = {
    assert(iter.isEmpty)
    hasNewline0
  }
  lazy val lastLineLength = {
    assert(iter.isEmpty)
    lastLineLength0
  }
  def flatMap(f: (Boolean, Int) => Result): Result = {
    var newHasNewline = false
    var newLastLineLength = 0

    val mergedIterator = Result.concat(
      () => iter,
      () => {
        val newResult = f(hasNewline, lastLineLength0)
        newResult.iter.map{ x =>
          if (!newResult.iter.hasNext){
            newHasNewline = newResult.hasNewline
            newLastLineLength = newResult.lastLineLength
          }
          x
        }
      }
    )
    new Result(
      mergedIterator,
      newHasNewline | hasNewline,
      if (newHasNewline) newLastLineLength
      else newLastLineLength + lastLineLength
    )

  }
}

object Result{
  def fromString(s: => fansi.Str) = {
    lazy val lines = s.plainText.lines.toArray
    new Result(Iterator(s), lines.length > 1, lines.last.length)
  }

  // I have no idea why this is necessary, but without doing this, the
  // default way of concatenation e.g.
  //
  // val middle = first ++ lastChildIter ++ sep ++ remaining
  //
  // Was throwing weird NullPointerExceptions I couldn't figure out =(
  def concat[T](is: (() => Iterator[T])*) = new Iterator[T]{
    var thunks = is.toList
    var head: Iterator[T] = null
    def check() = {
      while (thunks.nonEmpty && (head == null || head.isEmpty)) {
        head = thunks.head()
        thunks = thunks.tail
      }
    }

    def hasNext = {
      check()
      head != null && (thunks.nonEmpty || head.nonEmpty)
    }

    def next() = {
      check()
      head.next()
    }
  }
}
