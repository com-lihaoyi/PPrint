package pprint

class Renderer(maxWidth: Int,
               colorApplyPrefix: fansi.Attrs,
               colorLiteral: fansi.Attrs,
               indentStr: String){
  val x = PPrinter
  def joinIter[T](it: Iterator[Iterator[T]], joiner: T) = new Iterator[Iterator[T]]{
    var count = 0
    def hasNext = it.hasNext

    def next() = {
      val res =
        if (count % 2 == 0) it.next()
        else Iterator(joiner)
      count += 1
      res
    }
  }
  def dropRight(it: Iterator[fansi.Str]) = new Iterator[fansi.Str]{
    def hasNext = it.hasNext

    def next() = {
      val res = it.next()
      if (hasNext) res
      else ""
    }
  }


  def rec(x: Tree, leftOffset: Int, indentCount: Int): Result = {

    x match{
      case Tree.Apply(prefix, body) =>
        // Render children and buffer them until you fill up a single line,
        // or you run out of children.
        //
        // Even before rendering any children, the indentation, prefix
        // and the two open/close parens already take up a few characters
        var totalHorizontalWidth = leftOffset + prefix.length + 2
        val buffer = collection.mutable.Buffer.empty[Seq[fansi.Str]]
        var lastChildIter = Iterator[fansi.Str]()
        var lastChildSep = ""
        var childCompletedLineCount = 0
        while(body.hasNext && totalHorizontalWidth <= maxWidth && childCompletedLineCount == 0){

          val child = body.next()
          val childRes = rec(child, (indentCount + 1) * indentStr.length, indentCount + 1)

          val childBuffer = collection.mutable.Buffer.empty[fansi.Str]
          while(childRes.iter.hasNext && totalHorizontalWidth < maxWidth){
            val next = childRes.iter.next()
            childBuffer += next
            totalHorizontalWidth += next.length
          }


          if (body.hasNext) {
            totalHorizontalWidth += 2
            if (!childRes.iter.hasNext) {
              childBuffer += ","
            }
          }

          if (!childRes.iter.hasNext){
            childCompletedLineCount = childCompletedLineCount + childRes.completedLineCount
          }else{
            lastChildIter = childRes.iter
            if (body.hasNext) lastChildSep = ","
          }

          buffer += childBuffer
        }

        val start = Iterator(colorApplyPrefix(prefix), fansi.Str("("))

        val sep = Iterator(fansi.Str(lastChildSep))

        if (totalHorizontalWidth <= maxWidth && childCompletedLineCount == 0) {
          val middle = joinIter(buffer.iterator.map(_.iterator), fansi.Str(" ")).flatten
          val iter = Result.concat(() => start, () => middle, () => Iterator(fansi.Str(")")))
          val length: Int = buffer.iterator.map(_.iterator.map(_.length).sum).sum
          new Result(iter, 0, length)
        } else {
          def first = joinIter(
            buffer.iterator.map(_.iterator),
            fansi.Str("\n" + indentStr * (indentCount+1))
          ).flatten


          def remaining0 = dropRight(
            body.flatMap(c =>
              Iterator(fansi.Str("\n" + (indentStr * (indentCount+1)))) ++
              rec(c, (indentCount + 1) * indentStr.length, indentCount + 1).iter ++
              Iterator(fansi.Str(","))
            )
          )

          def iter = Result.concat[fansi.Str](
            () => start,
            () => Iterator(fansi.Str("\n"), indentStr * (indentCount + 1)),
            () => first,
            () => lastChildIter,
            () => sep,
            // If there are no rendered items in the buffer, we do not need
            // to add the leading newline, since the end-of-
            () => if (buffer.isEmpty) remaining0.drop(1) else remaining0,
            () => Iterator(fansi.Str("\n"), indentStr * indentCount, fansi.Str(")"))
          )

          new Result(iter, childCompletedLineCount + 2, indentCount * indentStr.length + 1)
        }

      case Tree.Infix(lhs, op, rhs) =>
        rec(lhs, leftOffset, indentCount).flatMap{ (lhsNewline, lhsLastLineLength) =>
          Result.fromString(" " + op + " ").flatMap((_, _) =>
            rec(rhs, lhsLastLineLength, indentCount)
          )
        }

      case t: Tree.Lazy =>

        lazy val str = t.body0(Tree.Ctx(
          maxWidth, leftOffset, indentCount,
          indentStr, colorLiteral, colorApplyPrefix
        ))
        new Truncated(str.map(fansi.Str(_)), maxWidth, height = 99999999).toResult

      case t: Tree.Literal => Result.fromString(colorLiteral(t.body))

    }
  }
}

