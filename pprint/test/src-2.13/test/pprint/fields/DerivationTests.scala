package test.pprint
package fields



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

  val Check = new Check(fields = true)

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
        """ADTb(i = 123, s = "hello world")"""
      )

      Check(
        Seq(ADTb(123, "hello world"), ADTb(-999, "i am cow")),
        """List(ADTb(i = 123, s = "hello world"), ADTb(i = -999, s = "i am cow"))"""
      )

      Check(ADT0(), "ADT0()")
    }
    test("sealedHierarchies"){
      import DeepHierarchy._
      Check(
        AnQ(1),
        "AnQ(i = 1)"
      )
      Check(
        AnQ(1): Q,
        "AnQ(i = 1)"
      )
      Check(
        E(false),
        "E(b = false)"
      )
      Check(
        F(AnQ(1)): A,
        "F(q = AnQ(i = 1))"
      )
    }
    test("varargs"){
      import Varargs._
      Check(
        Sentence("omg", "2", "3"),
        """Sentence(a = "omg", bs = WrappedArray("2", "3"))""",
        """Sentence(a = "omg", bs = ArrayBuffer("2", "3"))""", // 2.10
        """Sentence(a = "omg", bs = ArraySeq("2", "3"))""", // 2.13
        """Sentence(a = "omg", bs = WrappedVarArgs("2", "3"))""" // Scala.JS 2.13
      )
    }
    test("genericADTs"){
      import GenericADTs._
      Check(DeltaHardcoded.Remove("omg"), """Remove(key = "omg")""")
      Check(
        Delta.Insert(List("omg", "wtf"), (1, 0.2)),
        """Insert(key = List("omg", "wtf"), value = (1, 0.2))""",
        """Insert(key = List("omg", "wtf"), value = (1, 0.2F))""",
        """Insert(key = List("omg", "wtf"), value = (1, 0.200000))"""
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
        """Remove(key = List(1, 2, 3))"""
      )
      Check(
        Delta.Insert(List("omg", "wtf"), (1, 0.2)): Delta[List[String], (Int, Double)],
        """Insert(key = List("omg", "wtf"), value = (1, 0.2))""",
        """Insert(key = List("omg", "wtf"), value = (1, 0.2F))""",
        """Insert(key = List("omg", "wtf"), value = (1, 0.200000))"""
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
        s"""ADT(
           |  a = $x,
           |  b = $x,
           |  c = "lol",
           |  d = "lol",
           |  e = (1.5, 2.5),
           |  f = (1.5, 2.5)
           |)""".stripMargin,
        s"""ADT(
           |  a = $x,
           |  b = $x,
           |  c = "lol",
           |  d = "lol",
           |  e = (1.5F, 2.5F),
           |  f = (1.5F, 2.5F)
           |)""".stripMargin,
        s"""ADT(
           |  a = $x,
           |  b = $x,
           |  c = "lol",
           |  d = "lol",
           |  e = (1.500000, 2.500000),
           |  f = (1.500000, 2.500000)
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
    test("issue92"){
      val r = new Issue92.Rational {
        override def compare(that: Issue92.Rational): Int = ???
      }
      Check(r : Issue92.Rational, r.toString)
    }
    test("test"){
      Check(
        C2(List(C1("hello", List("world")))),
        """C2(results = List(C1(name = "hello", types = List("world"))))"""
      )
    }
  }
}


