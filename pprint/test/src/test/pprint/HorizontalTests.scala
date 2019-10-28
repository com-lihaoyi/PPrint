package test.pprint

import utest._
import scala.collection.{immutable => imm, mutable}
object HorizontalTests extends TestSuite{
  val Check = new Check(9999)
  val tests = TestSuite{
    test("Horizontal"){

      test("primitives"){
        test("Unit"){
          test - Check((), "()", "undefined")
        }
        test("Char"){
          test - Check('\n', "'\\n'")
          test - Check('a', "'a'")
        }
        test("Byte"){
          test - Check(123.toByte, "123")
          test - Check(-123.toByte, "-123")
        }
        test("Short"){
          test - Check(123.toShort, "123")
          test - Check(-12345.toShort, "-12345")
        }
        test("Int"){
          test - Check(123, "123")
          test - Check(-1234567, "-1234567")
        }
        test("Long"){
          test - Check(123456789012345L, "123456789012345L")
          test - Check(-123456789012345L, "-123456789012345L")
        }
        test("Float"){
          test - Check(0.75F, "0.75F", "0.750000F")
          test - Check(-13.5F, "-13.5F", "-13.500000F")
        }
        test("Double"){
          test - Check(0.125, "0.125", "0.125F", "0.125000")
          test - Check(-0.125, "-0.125", "-0.125F", "-0.125000")
        }
        test("String"){
          val tq = "\"\"\""
          test - Check("i am a cow", """ "i am a cow" """)
          test - Check( """ "hello" """.trim, """ "\"hello\"" """.trim)

          test - Check("\n", s"""
          |$tq
          |$tq
          """.stripMargin)
          test - Check("\n\n\n", s"""
          |$tq
          |
          |
          |$tq
          """.stripMargin)
          val n = 1000
          test - Check(
            "\n" + "ABCDEFG" * n,
            "\"\"\"\n" + "ABCDEFG" * n + "\"\"\""
          )
        }
        test("Symbols"){
          test - Check('hello, """'hello""")
          test - Check('I_AM_A_COW, """'I_AM_A_COW""")
        }
      }

      test("misc"){
        test("Nothing") - intercept[Exception](Check(throw new Exception(), ""))
        test("Null"){
          Check(null, "null")
          Check(null: String, "null")
          Check(Seq("look!", null: String, "hi"), """List("look!", null, "hi")""")
        }
        test("Either"){
          Check(Left(123): Either[Int, Int], "Left(123)")
          Check(Left(123): Left[Int, Int], "Left(123)")

          Check(Left(123), "Left(123)")
          Check(Right((1, "2", 3)), """Right((1, "2", 3))""")
        }
        test("Options"){
          Check(Some(123), "Some(123)")
          Check(None: Option[Int], "None")
          Check(None: Option[Nothing], "None")
          Check(None, "None")
          Check(Some(None), "Some(None)")
        }
        test("Default"){
          val baos = new java.io.ByteArrayOutputStream()
          Check(baos, baos.toString)

        }
      }

      test("collections"){
        // Fallback to toString
        test("Iterator"){
          Check(Iterator(), "empty iterator", "<iterator>")
          Check(Iterator(1, 2, 3), "non-empty iterator", "<iterator>")
          Check(Option(Iterator(1, 2, 3)), "Some(non-empty iterator)", "Some(<iterator>)")
        }

        test("Iterator") - Check(Iterable('1', '2', '3'), "List('1', '2', '3')")

        test("Array") - Check(Array(1, 2, 3), "Array(1, 2, 3)")
        test("Seq") - Check(Seq(1, 2, 3), "List(1, 2, 3)")
        test("List") - Check(List("1", "2", "3"), """List("1", "2", "3")""")
        test("Vector") - Check(Vector('omg, 'wtf, 'bbq), """Vector('omg, 'wtf, 'bbq)""")

        test("Buffer") - Check(
          mutable.Buffer('omg, 'wtf, 'bbq),
          """ArrayBuffer('omg, 'wtf, 'bbq)""",
          """WrappedArray('omg, 'wtf, 'bbq)"""
        )


        // Streams are hard-coded to always display vertically, in order
        // to make streaming pretty-printing sane
        test("Stream") - Check(
          Stream('omg, 'wtf, 'bbq),
          """Stream('omg, 'wtf, 'bbq)"""
        )
        test("Iterable") - Check(Iterable('omg, 'wtf, 'bbq), """List('omg, 'wtf, 'bbq)""")
        test("Traversable") - Check(Traversable('omg, 'wtf, 'bbq), """List('omg, 'wtf, 'bbq)""")
        test("Set") - Check(Set('omg), """Set('omg)""")
        test("mutableSet") - Check(mutable.Set('omg), """Set('omg)""", """HashSet('omg)""")
        test("collectionSet") - Check(collection.Set('omg), """Set('omg)""")
        test("SortedSet") - Check(
          imm.SortedSet("1", "2", "3"),
          """TreeSet("1", "2", "3")""",
          """Set("1", "2", "3")"""
        )
        test("Map"){
          Check(Map("key" -> "value"), """Map("key" -> "value")""")
        }
        test("collectionMap"){
          Check(Map("key" -> "value"): collection.Map[String, String], """Map("key" -> "value")""")
        }

        test("mutableMap"){
          Check(
            mutable.Map("key" -> "value"),
            """Map("key" -> "value")""",
            """HashMap("key" -> "value")"""
          )
        }

        test("SortedMap") - Check(
          imm.SortedMap("key" -> "v", "key2" -> "v2"),
          """Map("key" -> "v", "key2" -> "v2")""",
          """TreeMap("key" -> "v", "key2" -> "v2")"""
        )
      }

      test("tuples"){

        test("normal"){

          Check(Tuple1("123"), """Tuple1("123")""")
          Check((1, 2, "123"), """(1, 2, "123")""")
          Check(

            (1, 2, "123", (100L, 200L), 1.5F, 0.1),
            """(1, 2, "123", (100L, 200L), 1.5F, 0.1)""",
            """(1, 2, "123", (100L, 200L), 1.5F, 0.1F)""",
            """(1, 2, "123", (100L, 200L), 1.500000F, 0.100000)"""
          )
        }
        test("infix"){
          case class ::(x: Any, y: Any)
          Check(::(1, 2), "1 :: 2")
          Check(::(0, ::(1, 2)), "0 :: 1 :: 2")
        }
      }
    }

  }


}
