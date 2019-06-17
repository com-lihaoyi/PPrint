package test.pprint

import pprint.Util
import utest._


object UnitTests extends TestSuite{


  val tests = TestSuite{
    test("escapeChar"){
      def check(c: Char, expected: String) = {

        val escaped = pprint.Util.escapeChar(c, new StringBuilder).toString
        assert(escaped == expected)
      }
      check('a', "a")
      check('-', "-")
      check('\n', "\\n")
      check('\\', "\\\\")
      check('\t', "\\t")
    }
    test("literalize"){
      val simple = pprint.Util.literalize("hi i am a cow")
      val simpleExpected = """ "hi i am a cow" """.trim
      assert(simple == simpleExpected)

      val escaped = pprint.Util.literalize("hi i am a \"cow\"")
      val escapedExpected = """ "hi i am a \"cow\"" """.trim
      assert(escaped == escapedExpected)
    }
    test("concatIter"){

      var count = 0
      def check(iter: Iterator[Iterator[Int]],
                joiner: => Iterator[Int],
                expected: Vector[Int]) = {
        val joined = new Util.ConcatIterator(
          iter,
          () => joiner
        )
        val output = joined.toVector
        assert(output == expected)
      }
      test("multipleItems") - check(
        Iterator(
          Iterator(1, 2, 3),
          Iterator(4, 5, 6),
          Iterator(7, 8, 9)
        ),
        {
          count -= 1
          Iterator(count, count-1)
        },
        Vector(1, 2, 3, -1, -2, 4, 5, 6, -2, -3, 7, 8, 9)
      )
      test("singleItem") - check(
        Iterator(
          Iterator(1, 2, 3)
        ),
        Iterator(9),
        Vector(1, 2, 3)
      )

      test("empty") - check(
        Iterator(),
        Iterator(9),
        Vector()
      )
      test("empty2") - check(
        Iterator(Iterator()),
        Iterator(9),
        Vector()
      )
      test("joinedEmpties") - check(
        Iterator(Iterator(), Iterator()),
        Iterator(9),
        Vector(9)
      )

      test("halfEmpty") - check(
        Iterator(Iterator(1), Iterator()),
        Iterator(9),
        Vector(1, 9)
      )
      test("halfEmpty2") - check(
        Iterator(Iterator(), Iterator(1, 2, 3)),
        Iterator(9),
        Vector(9, 1, 2, 3)
      )

    }
  }
}
