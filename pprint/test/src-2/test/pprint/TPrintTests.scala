package test.pprint

import pprint.{TPrint, TPrintColors}
import utest._

object TPrintTests extends TestSuite{

  class M

  val tests = TestSuite{
    //
    type X = scala.Int with scala.Predef.String{}
    val x = ""
    def checkVal[T](expected: String, expr: => T)(implicit tprint: TPrint[T]) = {
      check[T](expected)(tprint)
    }

    def check[T](expected: String*)(implicit tprint: TPrint[T]) = {
      val tprinted = tprint.render.render
      assert(expected.contains(tprinted))
    }
    test("plain"){
      test("simple"){
        check[X]("X")
        check[String]("String")
        check[java.lang.String]("String")
        check[Int]("Int")

        check[scala.Int]("Int")
        def t[T] = check[T]("T")
        t
      }

      test("nothing"){
        check[Nothing]("Nothing")
        // Inferred nothings behave weirdly, make sure it works!
        check("Nothing")
        checkVal("Nothing", throw new Exception())
        checkVal("Some[Nothing]", Some(???))
      }

      test("singleton"){
        check[x.type]("x.type")
        check[TPrintTests.this.M]("M")
        check[TPrintTests.type]("TPrintTests.type")
      }

      test("java"){
        check[java.util.Set[_]]("java.util.Set[_]")
        check[java.util.Set[_ <: String]]("java.util.Set[_] forSome { type _ <: String }")
        check[java.util.Set[String]]("java.util.Set[String]")
      }

      test("mutable"){

        check[collection.mutable.Buffer[Int]]("collection.mutable.Buffer[Int]")
        import collection.mutable
        check[collection.mutable.Buffer[Int]]("mutable.Buffer[Int]")
        check[Seq[Int]]("Seq[Int]")

        // can't use scala.util.Properties on Scala.JS
        val is213Plus = classOf[Seq[Int]].getName != "scala.collection.Seq"
        check[collection.Seq[Int]](if (is213Plus) "collection.Seq[Int]" else "Seq[Int]")
        check[collection.immutable.Seq[Int]](if (is213Plus) "Seq[Int]" else "collection.immutable.Seq[Int]")

      }
      test("compound"){
        check[Map[Int, List[String]]]("Map[Int, List[String]]")
        check[Int => String]("Int => String")
        check[(Int, Float) => String]("(Int, Float) => String")
        check[(Int, Float, Double, Char, Byte, Short, Long) => String](
          "(Int, Float, Double, Char, Byte, Short, Long) => String"
        )
        check[(Int, Float) => (String, String)]("(Int, Float) => (String, String)")
        check[(Int, String)]("(Int, String)")
        check[(Int, String, (Int, String), Double)]("(Int, String, (Int, String), Double)")
        check[Int {val x: Int}]("Int{val x: Int}")
        check[Int with String]("Int with String")
      }
      test("existential"){
        check[{type T = Int}]("{type T = Int}")

        check[Map[_, _]]("Map[_, _]")
        check[Map[K, Int] forSome { type K }](
          "Map[K, Int] forSome { type K }"
        )
        check[Map[K, Int] forSome { type K <: Int }](
          "Map[K, Int] forSome { type K <: Int }"
        )
        check[Map[K, V] forSome { type K <: Int; type V >: String }](
          "Map[K, V] forSome { type K <: Int; type V >: String }"
        )
        check[Map[K, V] forSome { type K <: Int; val x: Float; type V >: String }](
          "Map[K, V] forSome { type K <: Int; val x: Float; type V >: String }"
        )
        class C{
          type T
        }
        check[x.T forSome { val x: Int with C} ](
          "x.T forSome { val x: Int with C }"
        )
        check[K[Int] forSome { type K[_ <: Int] <: Seq[Int] }](
          "K[Int] forSome { type K[_ <: Int] <: Seq[Int] }",
          "K[Int] forSome { type K[_ <: Int] <: _ <: Seq[Int] }"
        )
        check[K[Int] forSome { type K[X <: Int] <: Seq[X] }](
          "K[Int] forSome { type K[X <: Int] <: Seq[X] }",
          "K[Int] forSome { type K[X <: Int] <: _ <: Seq[X] }"
        )
        check[K[Int] forSome { type K[X] }](
          "K[Int] forSome { type K[X] }",
          "K[Int] forSome { type K[X] <: _ }"
        )
        check[K[Int] forSome { type K[_] <: Seq[_]}](
          "K[Int] forSome { type K[_] <: Seq[_] }",
          "K[Int] forSome { type K[_] <: _ <: Seq[_] }"
        )
        // https://issues.scala-lang.org/browse/SI-9325
        //      check[K[Int] forSome { type K[_] >: C }](
        //        "K[Int] forSome { type K[_] >: Int }"
        //      )
      }


      test("typeMember"){
        class C{ type V; class U}
        check[C#V]("C#V")
        check[C#U]("C#U")
        object O{
          class P
        }
        check[O.P]("O.P")
      }
      test("thisType"){
        class T {
          check[T.this.type]("T.this.type")
        }
        new T()
      }
      test("annotated"){
        // Can't use the normal implicit method, because of SI-8079
        val rendered = TPrint.default[M@deprecated].render
        assert(rendered.toString() == "M @deprecated")
      }

      class Custom

      test("complex"){
        class A
        class B{
          class C
        }
        check[(A with B)#C]("(A with B)#C")
        check[({type T = Int})#T]("Int")
        check[(Custom with B)#C]("(Custom with B)#C")

      }
      test("higherKinded"){
        class C[T[_]]
        check[C[List]]("C[List]")
      }
      test("byName"){
        check[(=> Int) => Double]("(=> Int) => Double")
        check[(=> Int, String, => (=> Char) => Float) => Double](
          "(=> Int, String, => (=> Char) => Float) => Double"
        )
      }
      test("range"){
        check[Range]("Range")
        checkVal("Range.Inclusive", 0 to 10)
        checkVal("Range", 0 until 10)
        check[Range.Inclusive]("Range.Inclusive")
      }
    }
    test("colored"){
      import pprint.TPrintColors.Colors._
      def checkColor[T](expected: String)(implicit tprint: TPrint[T]) = {
        val tprinted = tprint.render.toString.replace(
          fansi.Color.Green.escape, "<"
        ).replace(
          fansi.Color.Reset.escape, ">"
        )
        assert(tprinted == expected)
      }

      test - checkColor[String]("<String>")
      test - checkColor[Map[Int, String]]("<Map>[<Int>, <String>]")
      test - checkColor[collection.mutable.Seq[Int]]("<collection>.<mutable>.<Seq>[<Int>]")
      //      Not going to bother coloring these for now, since they're quite uncommon
      //      test - checkColor[{type T = Int; val x: String; def y: Char; var z: Double}](
      //                    "{type <T> = <Int>; val <x>: <String>; def <y>: <Char>; var <z>: <Double>}"
      //      )
      test - checkColor[Map[K, V] forSome {
        type K <: Int; val x: Float; type V >: (String, Float with Double)
      }](
        "<Map>[<K>, <V>] forSome { " +
          "type <K> <: <Int>; val <x>: <Float>; " +
          "type <V> >: (<String>, <Float> with <Double>) " +
          "}"
      )
    }

    test("functionGrouping"){
      test("simple")(
        check[(Int => Option[Int]) => Int]("(Int => Option[Int]) => Int")
      )
      test("lazy")(
        check[() => (Int => Option[Int])]("() => Int => Option[Int]")
      )
      test("complex")(
        check[
          (Int => Option[Int]) =>
            ((Int => String) => Int) =>
              (Int => Int)
        ]("(Int => Option[Int]) => ((Int => String) => Int) => Int => Int")
      )
    }
    test("wildcards"){
      case class MyList[T <: String]()
      check[MyList[_]]("MyList[_]")
    }
    test("weirdImplicits"){
      trait Lower {
        implicit def monad[M[_],T](i: T): M[T] = ???
        implicit def preventNothing[T](i: T): Nothing = ???
      }
      object Higher extends Lower{
        implicit def value[M[_],T](l: M[T]): T = ???
      }
      import Higher._

      check[Int]("Int")
    }
    test("nothingWithWeirdImport"){
      import scala.reflect.runtime.universe._
      check[Nothing]("Nothing")
    }
    test("constant"){
      check[123]("123")
      check["xyz"]("\"xyz\"")
      check[Seq[true]]("Seq[true]")
      assert(
        implicitly[TPrint[123]].render(TPrintColors.Colors) == fansi.Color.Green("123"),
        implicitly[TPrint["xyz"]].render(TPrintColors.Colors)  == fansi.Color.Green("\"xyz\""),
        implicitly[TPrint[Seq[true]]].render(TPrintColors.Colors)  == fansi.Color.Green("Seq") ++ "[" ++ fansi.Color.Green("true") ++ "]"
      )
    }
  }
}

