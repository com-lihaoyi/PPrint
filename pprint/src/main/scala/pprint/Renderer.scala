package pprint

import scala.annotation.switch

class Renderer(maxWidth: Int,
               colorApplyPrefix: fansi.Attrs,
               colorLiteral: fansi.Attrs,
               indentStr: String){
  val x = PPrint
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


  def println(x: Any) = {
    Predef.println(System.nanoTime() + "\t" + x)
  }
  def rec(x: Tree, leftOffset: Int, indent: Int): Result = {
    println("rec " + x)

    x match{
      case Tree.Apply(prefix, body) =>
        println("Tree.apply(" + prefix + ")")
        // Render children and buffer them until you fill up a single line,
        // or you run out of children.
        //
        // Even before rendering any children, the indentation, prefix
        // and the two open/close parens already take up a few characters
        var totalHorizontalWidth = indent * 2 + prefix.length + 2
        val buffer = collection.mutable.Buffer.empty[Seq[fansi.Str]]
        var lastChildIter = Iterator[fansi.Str]()
        var lastChildSep = ""
        var childHasNewline = false
        while(body.hasNext && totalHorizontalWidth <= maxWidth && !childHasNewline){

          val child = body.next()
          val childRes = rec(child, leftOffset + 2, indent + 1)

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
            childHasNewline = childHasNewline | childRes.hasNewline
          }else{
            lastChildIter = childRes.iter
            if (body.hasNext) lastChildSep = ","
          }

          buffer += childBuffer
        }

        val start = Iterator(colorApplyPrefix(prefix), fansi.Str("("))

        val sep = Iterator(fansi.Str(lastChildSep))

        if (totalHorizontalWidth <= maxWidth && !childHasNewline) {
          val middle = joinIter(buffer.iterator.map(_.iterator), fansi.Str(" ")).flatten
          val iter = Result.concat(() => start, () => middle, () => Iterator(fansi.Str(")")))
          val length: Int = buffer.iterator.map(_.iterator.map(_.length).sum).sum
          println("Tree.apply(" + prefix + ") End")
          new Result(iter, false, length)
        } else {
          def first = joinIter(
            buffer.iterator.map(_.iterator),
            fansi.Str("\n" + indentStr * (indent+1))
          ).flatten


          def remaining = dropRight(
            body.flatMap(c =>
              Iterator(fansi.Str("\n"), fansi.Str(indentStr * (indent+1))) ++
                rec(c, leftOffset + 2, indent + 1).iter ++
                Iterator(fansi.Str(","))
            )
          )



          def middle = Result.concat[fansi.Str](
            () => first,
            () => lastChildIter,
            () => sep,
            () => remaining
          )


          def iter = Result.concat[fansi.Str](
            () => start,
            () => Iterator(fansi.Str("\n"), indentStr * (indent + 1)),
            () => middle,
            () => Iterator(fansi.Str("\n"), indentStr * indent, fansi.Str(")"))
          )

          new Result(iter, true, indent * 2 + 1)
        }

      case Tree.Infix(lhs, op, rhs) =>
        rec(lhs, leftOffset, indent).flatMap{ (lhsNewline, lhsLastLineLength) =>
          Result.fromString(" -> ").flatMap((_, _) =>
            rec(rhs, lhsLastLineLength, indent)
          )
        }

      case t: Tree.Lazy =>
        println("render lazy")
        Result.fromString(t.body)
      case t: Tree.Literal => Result.fromString(colorLiteral(t.body))

    }
  }
}

