package test.pprint

import utest._
import scala.collection.{immutable => imm, mutable}
object HorizontalTests extends TestSuite{
  val Check = new Check(9999)
  val tests = TestSuite{
    'Horizontal {

      'primitives {
        'Unit {
          * - Check((), "()", "undefined")
        }
        'Char {
          * - Check('\n', "'\\n'")
          * - Check('a', "'a'")
        }
        'Byte {
          * - Check(123.toByte, "123")
          * - Check(-123.toByte, "-123")
        }
        'Short {
          * - Check(123.toShort, "123")
          * - Check(-12345.toShort, "-12345")
        }
        'Int {
          * - Check(123, "123")
          * - Check(-1234567, "-1234567")
        }
        'Long {
          * - Check(123456789012345L, "123456789012345L")
          * - Check(-123456789012345L, "-123456789012345L")
        }
        'Float {
          * - Check(0.75F, "0.75F", "0.750000F")
          * - Check(-13.5F, "-13.5F", "-13.500000F")
        }
        'Double {
          * - Check(0.125, "0.125", "0.125F", "0.125000")
          * - Check(-0.125, "-0.125", "-0.125F", "-0.125000")
        }
        'String {
          val tq = "\"\"\""
          * - Check("i am a cow", """ "i am a cow" """)
          * - Check( """ "hello" """.trim, """ "\"hello\"" """.trim)

          * - Check("\n", s"""
          |$tq
          |$tq
          """.stripMargin)
          * - Check("\n\n\n", s"""
          |$tq
          |
          |
          |$tq
          """.stripMargin)
          val n = 1000
          * - Check(
            "\n" + "ABCDEFG" * n,
            "\"\"\"\n" + "ABCDEFG" * n + "\"\"\""
          )
        }
        'Symbols {
          * - Check('hello, """'hello""")
          * - Check('I_AM_A_COW, """'I_AM_A_COW""")
        }
      }

      'misc {
        'Nothing - intercept[Exception](Check(throw new Exception(), ""))
        'Null {
          Check(null, "null")
          Check(null: String, "null")
          Check(Seq("look!", null: String, "hi"), """List("look!", null, "hi")""")
        }
        'Either {
          Check(Left(123): Either[Int, Int], "Left(123)")
          Check(Left(123): Left[Int, Int], "Left(123)")

          Check(Left(123), "Left(123)")
          Check(Right((1, "2", 3)), """Right((1, "2", 3))""")
        }
        'Options {
          Check(Some(123), "Some(123)")
          Check(None: Option[Int], "None")
          Check(None: Option[Nothing], "None")
          Check(None, "None")
          Check(Some(None), "Some(None)")
        }
        'Default{
          val baos = new java.io.ByteArrayOutputStream()
          Check(baos, baos.toString)

        }
      }

      'collections {
        // Fallback to toString
        'Iterator - {
          Check(Iterator(), "empty iterator")
          Check(Iterator(1, 2, 3), "non-empty iterator")
          Check(Option(Iterator(1, 2, 3)), "Some(non-empty iterator)")
        }

        'Iterator - Check(Iterable('1', '2', '3'), "List('1', '2', '3')")

        'Array - Check(Array(1, 2, 3), "Array(1, 2, 3)")
        'Seq - Check(Seq(1, 2, 3), "List(1, 2, 3)")
        'List - Check(List("1", "2", "3"), """List("1", "2", "3")""")
        'Vector - Check(Vector('omg, 'wtf, 'bbq), """Vector('omg, 'wtf, 'bbq)""")

        'Buffer - Check(
          mutable.Buffer('omg, 'wtf, 'bbq),
          """ArrayBuffer('omg, 'wtf, 'bbq)""",
          """WrappedArray('omg, 'wtf, 'bbq)"""
        )


        // Streams are hard-coded to always display vertically, in order
        // to make streaming pretty-printing sane
        'Stream - Check(
          Stream('omg, 'wtf, 'bbq),
          """Stream('omg, 'wtf, 'bbq)"""
        )
        'Iterable - Check(Iterable('omg, 'wtf, 'bbq), """List('omg, 'wtf, 'bbq)""")
        'Traversable - Check(Traversable('omg, 'wtf, 'bbq), """List('omg, 'wtf, 'bbq)""")
        'Set - Check(Set('omg), """Set('omg)""")
        'mutableSet - Check(mutable.Set('omg), """Set('omg)""")
        'collectionSet - Check(collection.Set('omg), """Set('omg)""")
        'SortedSet - Check(
          imm.SortedSet("1", "2", "3"),
          """TreeSet("1", "2", "3")"""
        )
        'Map {
          Check(Map("key" -> "value"), """Map("key" -> "value")""")
        }
        'collectionMap {
          Check(Map("key" -> "value"): collection.Map[String, String], """Map("key" -> "value")""")
        }

        'mutableMap {
          Check(mutable.Map("key" -> "value"), """Map("key" -> "value")""")
        }

        'SortedMap - Check(
          imm.SortedMap("key" -> "v", "key2" -> "v2"),
          """Map("key" -> "v", "key2" -> "v2")"""
        )
      }

      'tuples {

        'normal {

          Check(Tuple1("123"), """Tuple1("123")""")
          Check((1, 2, "123"), """(1, 2, "123")""")
          Check(

            (1, 2, "123", (100L, 200L), 1.5F, 0.1),
            """(1, 2, "123", (100L, 200L), 1.5F, 0.1)""",
            """(1, 2, "123", (100L, 200L), 1.5F, 0.1F)""",
            """(1, 2, "123", (100L, 200L), 1.500000F, 0.100000)"""
          )
        }
        'infix{
          case class ::(x: Any, y: Any)
          Check(::(1, 2), "1 :: 2")
          Check(::(0, ::(1, 2)), "0 :: 1 :: 2")
        }
      }
    }

  }


}
