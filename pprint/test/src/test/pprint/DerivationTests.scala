package test.pprint



import utest._

case class CustomToString(){
  override def toString = "LA LA LA"
}
sealed trait Customs
object Customs{
  case class A(i: Int) extends Customs
  case class B(s: String) extends Customs{
    override def toString = "Beeee"
  }
}
object DerivationTests extends TestSuite{

  val Check = new Check(100, 99999, false, false)

  val tests = TestSuite{
    test("singletons"){
      import Singletons._
      Check(Standalone, "Standalone")
      Check(BB, "BB")
      Check(CC, "CC")
      Check(CC: AA, "CC")
    }
    test("adts"){
      import ADTs._
      Check(
        ADTb(123, "hello world"),
        """ADTb(123, "hello world")"""
      )

      Check(
        Seq(ADTb(123, "hello world"), ADTb(-999, "i am cow")),
        """List(ADTb(123, "hello world"), ADTb(-999, "i am cow"))"""
      )

      Check(ADT0(), "ADT0()")
    }
    test("sealedHierarchies"){
      import DeepHierarchy._
      Check(
        AnQ(1),
        "AnQ(1)"
      )
      Check(
        AnQ(1): Q,
        "AnQ(1)"
      )
      Check(
        E(false),
        "E(false)"
      )
      Check(
        F(AnQ(1)): A,
        "F(AnQ(1))"
      )
    }
    test("varargs"){
      import Varargs._
      Check(
        Sentence("omg", "2", "3"),
        """Sentence("omg", WrappedArray("2", "3"))""",
        """Sentence("omg", ArrayBuffer("2", "3"))""", // 2.10
        """Sentence("omg", ArraySeq("2", "3"))""", // 2.13
        """Sentence("omg", WrappedVarArgs("2", "3"))""" // Scala.JS 2.13
      )
    }
    test("genericADTs"){
      import GenericADTs._
      Check(DeltaHardcoded.Remove("omg"), """Remove("omg")""")
      Check(
        Delta.Insert(List("omg", "wtf"), (1, 0.2)),
        """Insert(List("omg", "wtf"), (1, 0.2))""",
        """Insert(List("omg", "wtf"), (1, 0.2F))""",
        """Insert(List("omg", "wtf"), (1, 0.200000))"""
      )
      Check(
        DeltaInvariant.Clear[Int, String](),
        """Clear()"""
      )
      Check(
        DeltaInvariant.Clear(),
        """Clear()"""
      )

      Check(
        DeltaHardcoded.Remove(List(1, 2, 3)): DeltaHardcoded[Seq[Int], String],
        """Remove(List(1, 2, 3))"""
      )
      Check(
        Delta.Insert(List("omg", "wtf"), (1, 0.2)): Delta[List[String], (Int, Double)],
        """Insert(List("omg", "wtf"), (1, 0.2))""",
        """Insert(List("omg", "wtf"), (1, 0.2F))""",
        """Insert(List("omg", "wtf"), (1, 0.200000))"""
      )
      Check(
        DeltaInvariant.Clear(): DeltaInvariant[Int, String],
        """Clear()"""
      )

    }


    test("fallback"){
      // make sure we can pprint stuff that looks nothing like a case class
      // by falling back to good old toString
      import Amorphous._
      val a =  new A()
      Check(a, a.toString)
      Check(a: Any, a.toString)
      Check(Seq("lol", 1, 'c'), """List("lol", 1, 'c')""")
      Check(("lol", 1, 'c'): AnyRef, """("lol", 1, 'c')""")

      // Even random non-Scala stuff should work
      val x = new java.util.Random()
      Check(x, x.toString)
      val z = new java.util.UUID(0, -1)
      Check(z, "00000000-0000-0000-ffff-ffffffffffff")
      // Make sure when dealing with composite data structures, we continue
      // to use the static versions as deep as we can go before falling back
      // to toString
      Check(
        Generic.ADT(x, x: java.io.Serializable, "lol", "lol": Any, (1.5, 2.5), (1.5, 2.5): AnyRef),
        s"""ADT($x, $x, "lol", "lol", (1.5, 2.5), (1.5, 2.5))""",
        s"""ADT($x, $x, "lol", "lol", (1.5F, 2.5F), (1.5F, 2.5F))""",
        s"""ADT(
           |  $x,
           |  $x,
           |  "lol",
           |  "lol",
           |  (1.500000, 2.500000),
           |  (1.500000, 2.500000)
           |)""".stripMargin
      )

    }
    test("enums"){
      val days1 = pprint.PPrinter.BlackWhite.tokenize(
        java.util.concurrent.TimeUnit.DAYS
      ).mkString

      val days2 = pprint.PPrinter.BlackWhite.tokenize(
        scala.concurrent.duration.SECONDS: java.util.concurrent.TimeUnit
      ).mkString

      assert(
        days1 == "DAYS",
        days2 == "SECONDS"
      )
    }
    test("issue28"){
      val r = new Issue28.MyProduct2 
      Check(r : Issue28.MyProduct2, """("asdf", 333)""")
    }
    test("issue92"){
      val r = new Issue92.Rational {
        override def compare(that: Issue92.Rational): Int = ???
      }
      Check(r : Issue92.Rational, r.toString)
    }
    test("test"){
      Check(
        C2(List(C1("hello", List("world")))),
        """C2(List(C1("hello", List("world"))))"""
      )
    }
  }
}
