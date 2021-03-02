package test.pprint

import utest._
import pprint.PPrinter

import scala.collection.SortedMap

object AdvancedTests extends TestSuite{

  class C(){
    var counter = 0
    override def toString = {
      counter += 1

      // Make sure the fact that this fella renders ansi colors
      // as part of toString doesn't muck up our computation of width/height
      fansi.Color.Red("C").toString
    }
  }


  val tests = TestSuite{
    test("applyPrefixWidthExactlyMaxWidth"){
      case class Foo(is: List[Int])
      val rendered = Check.color.apply(
        Foo(List(1)),
        width = 10
      )
      assert(
        rendered.plainText ==
        """Foo(
          |  List(1)
          |)""".stripMargin
      )

    }

    test("truncatedAttrs"){
      def check(input: Iterator[String],
                width: Int,
                height: Int,
                expectedCompletedLineCount: Int,
                expectedLastLineLength: Int) = {

        val t = new pprint.Truncated(
          input.map(fansi.Str(_)),
          width,
          height
        )

        t.foreach(_ => ())

        val completedLineCount = t.completedLineCount
        val lastLineLength = t.lastLineLength
        assert(
          completedLineCount == expectedCompletedLineCount,
          lastLineLength == expectedLastLineLength
        )
      }
      check(
        Iterator("1234567"),
        width = 50, height = 50,
        expectedCompletedLineCount = 0,
        expectedLastLineLength = 7
      )
      check(
        Iterator("1234567"),
        width = 7, height = 50,
        expectedCompletedLineCount = 0,
        expectedLastLineLength = 7
      )
      check(
        Iterator("1234567"),
        width = 6, height = 50,
        expectedCompletedLineCount = 1,
        expectedLastLineLength = 1
      )
      check(
        Iterator("12", "34", "5", "67"),
        width = 6, height = 50,
        expectedCompletedLineCount = 1,
        expectedLastLineLength = 1
      )
    }

    test("config"){
      val res1 = pprint.apply(List(1, 2, 3)).plainText
      assert(res1 == "List(1, 2, 3)")

      val res2 = pprint.copy(defaultWidth = 6).apply(List(1, 2, 3), width = 6).plainText
      assert(res2 == "List(\n  1,\n  2,\n  3\n)")

      val res3 = pprint.copy(defaultWidth = 6).apply(List(1, 2, 3)).plainText
      assert(res3 == "List(\n  1,\n  2,\n  3\n)")

      val res4 = pprint.copy(defaultWidth = 6).copy(defaultIndent = 4).apply(List(1, 2, 3)).plainText
      assert(res4 == "List(\n    1,\n    2,\n    3\n)")

      val res5 = pprint.copy(additionalHandlers = {
        case x: Int => pprint.Tree.Literal((-x).toString)
      }).apply(List(1, 2, 3)).plainText

      assert(res5 == "List(-1, -2, -3)")
    }

    test("Laziness"){
      val Check = new Check(width = 20, height = 5)
      test("list"){
        test("Horizontal"){
          val C = new C
          Check(
            List.fill(4)(C),
            """List(C, C, C, C)"""
          )
          val counter = C.counter
          // https://github.com/scala-js/scala-js/issues/2953
          if (sys.props("java.vm.name") != "Scala.js") {
            assert(counter == 4)
          }
        }
        test("Vertical"){
          val C = new C
          Check(
            List.fill(100)(C),
            """List(
              |  C,
              |  C,
              |  C,
              |...""".stripMargin
          )
          //          10        20
          //List(C, C, C, C, C, ) ....

          // 5 horizontal renders before deciding it can't fit,
          // then it re-uses those renders and lays them out
          // vertically, taking the first 3 before being cut off
          val counter = C.counter
          // https://github.com/scala-js/scala-js/issues/2953
          if (sys.props("java.vm.name") != "Scala.js"){
            assert(counter == 5)
          }
        }
      }

      test("map"){
        test("Horizontal"){
          val Check = new Check(width = 24, height = 5)
          val C = new C
          Check(
            SortedMap(List.tabulate(2)(_ -> C):_*),
            """Map(0 -> C, 1 -> C)""",
            """TreeMap(0 -> C, 1 -> C)""".stripMargin
          )
          val counter = C.counter
          // https://github.com/scala-js/scala-js/issues/2953
          if (sys.props("java.vm.name") != "Scala.js") {
            assert(counter == 2)
          }
        }
        test("Vertical"){
          val C = new C
          Check(
            SortedMap(List.tabulate(100)(_ -> C):_*),
            """Map(
              |  0 -> C,
              |  1 -> C,
              |  2 -> C,
              |...""".stripMargin,
            """TreeMap(
              |  0 -> C,
              |  1 -> C,
              |  2 -> C,
              |...""".stripMargin
          )
          //          10        20
          //Map(0 -> C, 1 -> C, 2 -> C
          //                    ^ break

          // 2 horizontal renders (and change) before deciding it can't fit
          // 4 vertical renders before overshooting
          val count = C.counter
          // https://github.com/scala-js/scala-js/issues/2953
          if (sys.props("java.vm.name") != "Scala.js") {
            assert(count == 4)
          }
        }
      }

      test("unicode"){
        val withEscaping = new PPrinter(){
          override def escapeUnicode = true
        }
        val withoutEscaping = new PPrinter(){
          override def escapeUnicode = false
        }

        val toCheck = List("foo", "йцук", "漢字")

        val withEscapingRes = withEscaping.apply(toCheck).plainText
        val withoutEscapingRes = withoutEscaping.apply(toCheck).plainText

        assert(withEscapingRes == "List(\"foo\", \"\\u0439\\u0446\\u0443\\u043a\", \"\\u6f22\\u5b57\")")
        assert(withoutEscapingRes == """List("foo", "йцук", "漢字")""")
      }
    }
  }


}
