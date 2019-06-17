package test.pprint

import pprint.PPrinter
import utest._

import scala.annotation.tailrec

object VerticalTests extends TestSuite{

  val tests = TestSuite{


    test("Vertical"){

      val Check = new Check(width = 25, renderTwice = true)
      test("singleNested"){
        test - new Check(width = 5)(
          List(1, 2, 3),
          """List(
            |  1,
            |  2,
            |  3
            |)
          """.stripMargin
        )
        test - Check(
          List("12", "12", "12"),
          """List("12", "12", "12")"""
        )
        test - Check(
          List("123", "123", "123"),
          """List("123", "123", "123")"""
        )
        test - Check(
          List("1234", "123", "123"),
          """List(
            |  "1234",
            |  "123",
            |  "123"
            |)""".stripMargin
        )
        test - Check(
          Map(1 -> 2, 3 -> 4),
          """Map(1 -> 2, 3 -> 4)"""
        )
        test - Check(
          Map(List(1, 2) -> List(3, 4), List(5, 6) -> List(7, 8)),
          """Map(
            |  List(1, 2) -> List(3, 4),
            |  List(5, 6) -> List(7, 8)
            |)""".stripMargin
        )

        test - Check(
          Map(
            List(123, 456, 789, 123, 456) -> List(3, 4, 3, 4),
            List(5, 6) -> List(7, 8)
          ),
          """Map(
            |  List(
            |    123,
            |    456,
            |    789,
            |    123,
            |    456
            |  ) -> List(3, 4, 3, 4),
            |  List(5, 6) -> List(7, 8)
            |)""".stripMargin
        )

        test - Check(
          Map(
            List(5, 6) -> List(7, 8),
            List(123, 456, 789, 123, 456) -> List(123, 456, 789, 123, 456)
          ),
          """Map(
            |  List(5, 6) -> List(7, 8),
            |  List(
            |    123,
            |    456,
            |    789,
            |    123,
            |    456
            |  ) -> List(
            |    123,
            |    456,
            |    789,
            |    123,
            |    456
            |  )
            |)""".stripMargin
        )

        test - Check(
          List("12345", "12345", "12345"),
          """List(
            |  "12345",
            |  "12345",
            |  "12345"
            |)""".stripMargin
        )
        test - Check(
          Foo(123, Seq("hello world", "moo")),
          """Foo(
            |  123,
            |  List(
            |    "hello world",
            |    "moo"
            |  )
            |)""".stripMargin
        )
        test - Check(
          Foo(123, Seq("moo")),
          """Foo(123, List("moo"))""".stripMargin
        )

      }
      test("doubleNested"){

        test - Check(
          List(Seq("omg", "omg"), Seq("mgg", "mgg"), Seq("ggx", "ggx")),
          """List(
            |  List("omg", "omg"),
            |  List("mgg", "mgg"),
            |  List("ggx", "ggx")
            |)""".stripMargin
        )
        test - Check(
          List(Seq("omg", "omg", "omg", "omg"), Seq("mgg", "mgg"), Seq("ggx", "ggx")),
          """List(
            |  List(
            |    "omg",
            |    "omg",
            |    "omg",
            |    "omg"
            |  ),
            |  List("mgg", "mgg"),
            |  List("ggx", "ggx")
            |)""".stripMargin
        )
        test - Check(
          List(
            Seq(
              Seq("mgg", "mgg", "lols"),
              Seq("mgg", "mgg")
            ),
            Seq(
              Seq("ggx", "ggx"),
              Seq("ggx", "ggx", "wtfx")
            )
          ),
          """List(
            |  List(
            |    List(
            |      "mgg",
            |      "mgg",
            |      "lols"
            |    ),
            |    List("mgg", "mgg")
            |  ),
            |  List(
            |    List("ggx", "ggx"),
            |    List(
            |      "ggx",
            |      "ggx",
            |      "wtfx"
            |    )
            |  )
            |)""".stripMargin
        )
        test - Check(
          FooG(Vector(FooG(Array(Foo(123, Nil)), Nil)), Nil),
          """FooG(
            |  Vector(
            |    FooG(
            |      Array(
            |        Foo(123, List())
            |      ),
            |      List()
            |    )
            |  ),
            |  List()
            |)
          """.stripMargin
        )
        test - Check(
          FooG(FooG(Seq(Foo(3, Nil)), Nil), Nil),
          """FooG(
            |  FooG(
            |    List(Foo(3, List())),
            |    List()
            |  ),
            |  List()
            |)""".stripMargin
        )
      }
    }
    test("traited"){
      val Check = new Check()
      Check(Nested.ODef.Foo(2, "ba"), "Foo(2, \"ba\")")
      Check(Nested.CDef.Foo(2, "ba"), "Foo(2, \"ba\")")
    }
    test("Color"){
      def count(haystack: Iterator[fansi.Str], needles: (String, Int)*) = {
        val str = haystack.map(_.render).mkString
        for ((needle, expected) <- needles){
          val count = countSubstring(str, needle)

          assert(count == expected)
        }
      }
      def countSubstring(str1:String, str2:String):Int={
        @tailrec def count(pos:Int, c:Int):Int={
          val idx=str1 indexOf(str2, pos)
          if(idx == -1) c else count(idx+str2.size, c+1)
        }
        count(0,0)
      }

      import Console._
      val cReset = fansi.Color.Reset.escape

      test - count(PPrinter.Color.tokenize(123), GREEN -> 1, cReset -> 1)
      test - count(PPrinter.Color.tokenize(""), GREEN -> 1, cReset -> 1)
      test - count(PPrinter.Color.tokenize(Seq(1, 2, 3)), GREEN -> 3, YELLOW -> 1, cReset -> 4)
      test - count(
        PPrinter.Color.tokenize(Map(1 -> Nil, 2 -> Seq(" "), 3 -> Seq("   "))),
        GREEN -> 5, YELLOW -> 4, cReset -> 9
      )
    }

    test("Truncation"){
      test("longNoTruncation"){
        val Check = new Check()
        test - Check("a" * 10000,"\""+"a" * 10000+"\"")
        test - Check(
          List.fill(30)(100),
          """List(
            |  100,
            |  100,
            |  100,
            |  100,
            |  100,
            |  100,
            |  100,
            |  100,
            |  100,
            |  100,
            |  100,
            |  100,
            |  100,
            |  100,
            |  100,
            |  100,
            |  100,
            |  100,
            |  100,
            |  100,
            |  100,
            |  100,
            |  100,
            |  100,
            |  100,
            |  100,
            |  100,
            |  100,
            |  100,
            |  100
            |)""".stripMargin
        )
      }

      test("shortNonTruncated"){
        val Check = new Check(height = 15)
        test - Check("a"*1000, "\"" + "a"*1000 + "\"")
        test - Check(List(1,2,3,4), "List(1, 2, 3, 4)")
        test - Check(
          List.fill(13)("asdfghjklqwertz"),
          """List(
            |  "asdfghjklqwertz",
            |  "asdfghjklqwertz",
            |  "asdfghjklqwertz",
            |  "asdfghjklqwertz",
            |  "asdfghjklqwertz",
            |  "asdfghjklqwertz",
            |  "asdfghjklqwertz",
            |  "asdfghjklqwertz",
            |  "asdfghjklqwertz",
            |  "asdfghjklqwertz",
            |  "asdfghjklqwertz",
            |  "asdfghjklqwertz",
            |  "asdfghjklqwertz"
            |)
          """.stripMargin
        )
      }

      test("shortLinesTruncated"){
        val Check = new Check(height = 15)
        test - Check(
          List.fill(15)("foobarbaz"),
          """List(
            |  "foobarbaz",
            |  "foobarbaz",
            |  "foobarbaz",
            |  "foobarbaz",
            |  "foobarbaz",
            |  "foobarbaz",
            |  "foobarbaz",
            |  "foobarbaz",
            |  "foobarbaz",
            |  "foobarbaz",
            |  "foobarbaz",
            |  "foobarbaz",
            |  "foobarbaz",
            |...""".stripMargin
        )
        test - Check(
          List.fill(150)("foobarbaz"),
          """List(
            |  "foobarbaz",
            |  "foobarbaz",
            |  "foobarbaz",
            |  "foobarbaz",
            |  "foobarbaz",
            |  "foobarbaz",
            |  "foobarbaz",
            |  "foobarbaz",
            |  "foobarbaz",
            |  "foobarbaz",
            |  "foobarbaz",
            |  "foobarbaz",
            |  "foobarbaz",
            |...""".stripMargin
        )
      }

      test("longLineTruncated"){
        // These print out one long line, but at the width that the
        // pretty-printer is configured to, it (including any trailing ...)
        // wraps to fit within the desired width and height
        test{
          val Check = new Check(width = 5, height = 3)
          Check(
            "a" * 13,
            "\"aaaa" +
             "aaaaa" +
             "aaaa\""
          )
        }
        test{
          val Check = new Check(width = 5, height = 3)
          Check(
            "a" * 1000,
            "\"aaaa" +
             "aaaaa" +
             "..."
          )
        }
        test{
          val Check = new Check(width = 60, height = 5)
          Check(
            "a" * 1000,
            "\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"+
             "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"+
             "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"+
             "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"+
             "..."
          )
        }
      }

      test("stream"){
        val Check = new Check(height = 5)
        Check(
          Stream.continually("foo"),
          """Stream(
            |  "foo",
            |  "foo",
            |  "foo",
            |...
          """.stripMargin
        )
      }
    }

    test("wrappedLines"){
      val Check = new Check(width = 8, height = 5)

      Check(
        "1234567890\n"*10,
        "\"\"\"1234567890\n1234567890\n..."
      )
      // The result looks like 10 wide 3 deep, but because of the wrapping
      // (maxWidth = 8) it is actually 8 wide and 5 deep.
    }
  }


}
