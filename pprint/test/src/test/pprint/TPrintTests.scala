package pprint

import pprint.TPrint
import utest._

object TPrintTests extends TestSuite{

  class M

  val tests = TestSuite{
    //
    type X = scala.Int with scala.Predef.String{}
    val x = ""
    test("plain"){
      def checkVal[T](expected: String, expr: => T)(implicit tprint: TPrint[T]) = {
        check[T](expected)(tprint)
      }

      def check[T](expected: String*)(implicit tprint: TPrint[T]) = {
        val tprinted = tprint.render
        assert(expected.contains(tprinted))
      }
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
        assert(TPrint.default[M@deprecated].render == "M @deprecated")
      }

      class Custom
      test("custom"){
        // Maybe we want to add some extra decoration
        implicit def customTPrint: TPrint[Custom] = TPrint.lambda(cfg => "+++Custom+++")
        check[Custom]("+++Custom+++")
        check[List[Custom]]("List[+++Custom+++]")

        // Or make it look like F#
        implicit def StreamTPrint[T: TPrint]: TPrint[Stream[T]] = TPrint.lambda(
          c => implicitly[TPrint[T]].render(c) + " Stream"
        )
        check[Stream[Int]]("Int Stream")

        // Note how it works recursively
        check[Stream[Custom]]("+++Custom+++ Stream")
      }

      test("complex"){
        class A
        class B{
          class C
        }
        check[(A with B)#C]("(A with B)#C")
        check[({type T = Int})#T]("Int")
        implicit def customTPrint: TPrint[Custom] = TPrint.lambda(cfg => "+++Custom+++")
        check[(Custom with B)#C]("(+++Custom+++ with B)#C")

      }
      test("higherKinded"){
        class C[T[_]]
        check[C[List]]("C[List]")
      }
      test("byName"){
        check[(=> Int) => Double]("Function1[=> Int, Double]")
        check[(=> Int, String, => (=> Char) => Float) => Double](
          "Function3[=> Int, String, => Function1[=> Char, Float], Double]"
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
        val tprinted = tprint.render.replace(
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
  }

}