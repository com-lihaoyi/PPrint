package pprint


/**
  * The intermediate return type of the pretty-print system: provides an
  * iterator which produces the actual string output, as well as metadata
  * around that output that is only available after the iterator is exhausted
  */
class Result(val iter: Iterator[fansi.Str],
             completedLineCount0: => Int,
             lastLineLength0: => Int){
  lazy val completedLineCount = {
    require(iter.isEmpty)
    completedLineCount0
  }
  lazy val lastLineLength = {
    require(iter.isEmpty)
    lastLineLength0
  }
  def flatMap(f: (Int, Int) => Result): Result = {
    var newCompletedLineCount = 0
    var newLastLineLength = 0

    val mergedIterator = Result.concat(
      () => iter,
      () => {
        require(!iter.hasNext)
        val newResult = f(completedLineCount, lastLineLength0)
        newResult.iter.map{ x =>
          if (!newResult.iter.hasNext){
            newCompletedLineCount = newResult.completedLineCount
            newLastLineLength = newResult.lastLineLength
          }
          x
        }
      }
    )
    new Result(
      mergedIterator,
      newCompletedLineCount + completedLineCount,
      if (newCompletedLineCount > 0) newLastLineLength
      else newLastLineLength + lastLineLength
    )

  }
}

object Result{
  def fromString(s: => fansi.Str) = {
    lazy val lines = s.plainText.lines.toArray
    new Result(Iterator(s), lines.length - 1, lines.last.length)
  }

  // I have no idea why this is necessary, but without doing this, the
  // default way of concatenation e.g.
  //
  // val middle = first ++ lastChildIter ++ sep ++ remaining
  //
  // Was throwing weird NullPointerExceptions I couldn't figure out =(
  //
  // Also, ++ didn't seem to be sufficiently lazy, so it was forcing
  // things earlier than it really needed to. It isn't documented anywhere
  // how lazy it's meant to be, whereas `concat` here is transparently lazy
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
