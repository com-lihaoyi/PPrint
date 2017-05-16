package test.pprint

import pprint.PPrint
import utest._

import scala.annotation.tailrec
import scala.collection.{SortedMap, immutable => imm}
import scala.util.matching.Regex

object VerticalTests extends TestSuite{

  class C(){
    var counter = 0
    override def toString = {
      counter += 1
      def println(x: Any) = {
        Predef.println(System.nanoTime() + "\t" + x)
      }
      println("increment!")
      "C"
    }
  }

  val tests = TestSuite{

//    'ansiStripping {
//
//      val colorsToCheck = Seq(
//        pprint.Colors.Colored,
//        pprint.Colors.BlackWhite,
//        pprint.Colors(
//          fansi.Color.Yellow ++ fansi.Underlined.On,
//          fansi.Color.Blue ++ fansi.Bold.On
//        )
//      )
//      for(color <- colorsToCheck){
//        val cfg = pprint.Config.Defaults.PPrintConfig.copy(width = 5, height = 3, colors=color)
//        Check(
//          List(1, 2, 3, 4, 5),
//          """List(
//            |  1,
//            |  2,
//            |...""".stripMargin
//        )(implicitly, cfg)
//      }
//    }
    'Laziness{
      val Check = new Check(width = 20, height = 5)
      'list{
        'Horizontal {
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
        'Vertical{
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

      'map{
        'Horizontal{
          val C = new C
          Check(
            SortedMap(List.tabulate(2)(_ -> C):_*),
            """Map(0 -> C, 1 -> C)"""
          )
          val counter = C.counter
          // https://github.com/scala-js/scala-js/issues/2953
          if (sys.props("java.vm.name") != "Scala.js") {
            assert(counter == 2)
          }
        }
        'Vertical{
          val C = new C
          Check(
            SortedMap(List.tabulate(100)(_ -> C):_*),
            """Map(
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
    }
    'Vertical{

      val Check = new Check(width = 25)
      'singleNested {
        * - Check(
          List("12", "12", "12"),
          """List("12", "12", "12")"""
        )
        * - Check(
          List("123", "123", "123"),
          """List("123", "123", "123")"""
        )
        * - Check(
          List("1234", "123", "123"),
          """List(
            |  "1234",
            |  "123",
            |  "123"
            |)""".stripMargin
        )
        * - Check(
          Map(1 -> 2, 3 -> 4),
          """Map(1 -> 2, 3 -> 4)"""
        )
        * - Check(
          Map(List(1, 2) -> List(3, 4), List(5, 6) -> List(7, 8)),
          """Map(
            |  List(1, 2) -> List(3, 4),
            |  List(5, 6) -> List(7, 8)
            |)""".stripMargin
        )

        * - Check(
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

        * - Check(
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

        * - Check(
          List("12345", "12345", "12345"),
          """List(
            |  "12345",
            |  "12345",
            |  "12345"
            |)""".stripMargin
        )
        * - Check(
          Foo(123, Seq("hello world", "moo")),
          """Foo(
            |  123,
            |  List(
            |    "hello world",
            |    "moo"
            |  )
            |)""".stripMargin
        )
        * - Check(
          Foo(123, Seq("moo")),
          """Foo(123, List("moo"))""".stripMargin
        )

      }
      'doubleNested{

        * - Check(
          List(Seq("omg", "omg"), Seq("mgg", "mgg"), Seq("ggx", "ggx")),
          """List(
            |  List("omg", "omg"),
            |  List("mgg", "mgg"),
            |  List("ggx", "ggx")
            |)""".stripMargin
        )
        * - Check(
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
        * - Check(
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
        * - Check(
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
        * - Check(
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
    'traited {
      val Check = new Check()
      Check(Nested.ODef.Foo(2, "ba"), "Foo(2, \"ba\")")
      Check(Nested.CDef.Foo(2, "ba"), "Foo(2, \"ba\")")
    }
    'Color{
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
      * - count(PPrint.Color.tokenize(123), GREEN -> 1, cReset -> 1)
      * - count(PPrint.Color.tokenize(""), GREEN -> 1, cReset -> 1)
      * - count(PPrint.Color.tokenize(Seq(1, 2, 3)), GREEN -> 3, YELLOW -> 1, cReset -> 4)
      * - count(
        PPrint.Color.tokenize(Map(1 -> Nil, 2 -> Seq(" "), 3 -> Seq("   "))),
        GREEN -> 5, YELLOW -> 4, cReset -> 9
      )
    }

    'Truncation{
//      'test{
//        Check()
//      }
      'longNoTruncation{
        val Check = new Check()
        * - Check("a" * 10000,"\""+"a" * 10000+"\"")
        * - Check(
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

      'shortNonTruncated{
        val Check = new Check(height = 15)
        * - Check("a"*1000, "\"" + "a"*1000 + "\"")
        * - Check(List(1,2,3,4), "List(1, 2, 3, 4)")
        * - Check(
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

      'shortLinesTruncated{
        val Check = new Check(height = 15)
        * - Check(
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
        * - Check(
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

      'longLineTruncated{
        val Check = new Check(width = 100, height = 3)
        Check(
          "a" * 1000,
          """"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa..."""
        )
      }

      'stream{
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

    'wrappedLines{
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
