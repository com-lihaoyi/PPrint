package test.pprint

import pprint.Util
import utest._


object UnitTests extends TestSuite{


  val tests = TestSuite{
    'escapeChar{
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
    'literalize{
      val simple = pprint.Util.literalize("hi i am a cow")
      val simpleExpected = """ "hi i am a cow" """.trim
      assert(simple == simpleExpected)

      val escaped = pprint.Util.literalize("hi i am a \"cow\"")
      val escapedExpected = """ "hi i am a \"cow\"" """.trim
      assert(escaped == escapedExpected)
    }
    'concatIter{

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
      'multipleItems - check(
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
      'singleItem - check(
        Iterator(
          Iterator(1, 2, 3)
        ),
        Iterator(9),
        Vector(1, 2, 3)
      )

      'empty - check(
        Iterator(),
        Iterator(9),
        Vector()
      )
      'empty2 - check(
        Iterator(Iterator()),
        Iterator(9),
        Vector()
      )
      'joinedEmpties- check(
        Iterator(Iterator(), Iterator()),
        Iterator(9),
        Vector(9)
      )

      'halfEmpty- check(
        Iterator(Iterator(1), Iterator()),
        Iterator(9),
        Vector(1, 9)
      )
      'halfEmpty2- check(
        Iterator(Iterator(), Iterator(1, 2, 3)),
        Iterator(9),
        Vector(9, 1, 2, 3)
      )

    }
  }
}
