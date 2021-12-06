package pprint

import pprint.TPrint
import utest._


object TPrintTests extends TestSuite{

  class M

  val tests = TestSuite{
    //
    type X = scala.Int with scala.Predef.String
    val x = ""
    test("plain"){
      def checkVal[T](expected: String, expr: => T)(implicit tprint: TPrint[T]) = {
        check[T](expected)(using tprint)
      }

      def check[T](expected: String*)(implicit tprint: TPrint[T]) = {
        val tprinted = tprint.render
        assert(expected.contains(tprinted.render))
      }

      test("simple"){

        //        check[X]("X")
        check[String]("String")
        check[java.lang.String]("String")
        check[Int]("Int")


        val a = List(1,2,3).tail
        checkVal("List[Int]", a)

        check[scala.Int]("Int")
        def t[T] = check[T]("T")
        t
      }

      test("nothing"){
        test - check[Nothing]("Nothing")
        //Inferred nothings behave weirdly, make sure it works!
        test - check("Nothing")
        test - checkVal("Nothing", throw new Exception())
        test - checkVal("Some[Nothing]", Some(???))
      }

      test("singleton"){
        check[x.type]("x.type")
        check[TPrintTests.this.M]("M")
        //   check[TPrintTests.type]("TPrintTests.type")
      }

      test("java"){
        //check[java.util.Set[_]]("java.util.Set[_]")
        //       check[java.util.Set[_ <: Int]]("java.util.Set[_]")
        //       check[java.util.Set[_ <: String]]("java.util.Set[_] forSome { type _ <: String }")
        //       check[java.util.Set[_ <: String]]("java.util.Set[_] forSome { type _ <: String }")
        //        check[java.util.Set[String]]("java.util.Set[String]")
      }

      test("mutable"){

        //   check[collection.mutable.Buffer[Int]]("collection.mutable.Buffer[Int]")
        import collection.mutable
        //        TPrint.default[mutable.Buffer[Int]]
        //check[mutable.Buffer[Int]]("mutable.Buffer[Int]")
        check[Seq[Int]]("Seq[Int]")

        // can't use scala.util.Properties on Scala.JS
        //val is213Plus = classOf[Seq[Int]].getName != "scala.collection.Seq"
        // check[collection.Seq[Int]](if (is213Plus) "collection.Seq[Int]" else "Seq[Int]")
        // check[collection.immutable.Seq[Int]](if (is213Plus) "Seq[Int]" else "collection.immutable.Seq[Int]")

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
        //check[Int {val x: Int}]("Int{val x: Int}")
        //check[Int & String]("Int with String")
      }
      test("existential") {
        //TODO: Implicit doesn’t reolve
        // check[{type T = Int}]("{type T = Int}")

        check[Map[_, _]]("Map[_, _]")
      }


      //     test("thisType"){
      //       class T {
      //         check[T.this.type]("T.this.type")
      //       }
      //       new T()
      //     }
      //     test("annotated"){
      //       // Can't use the normal implicit method, because of SI-8079
      //       assert(TPrint.default[M@deprecated].render == "M @deprecated")
      //     }

      class Custom

      //     test("complex"){
      //       class A
      //       class B{
      //         class C
      //       }
      //       check[(A with B)#C]("(A with B)#C")
      //       check[({type T = Int})#T]("Int")
      //       implicit def customTPrint: TPrint[Custom] = TPrint.lambda(cfg => "+++Custom+++")
      //       check[(Custom with B)#C]("(+++Custom+++ with B)#C")

      //     }
      test("higherKinded"){

        //TODO: No idea why this breaks
        //class C[T[_]]
        //check[C[List]]("C[List]")(TPrint.default[C[List]])
      }
      // test("byName"){
      //   check[(=> Int) => Double]("Function1[=> Int, Double]")
      //   check[(=> Int, String, => (=> Char) => Float) => Double](
      //     "Function3[=> Int, String, => Function1[=> Char, Float], Double]"
      //   )
      //  }
      //     test("range"){
      //       check[Range]("Range")
      //       checkVal("Range.Inclusive", 0 to 10)
      //       checkVal("Range", 0 until 10)
      //       check[Range.Inclusive]("Range.Inclusive")
      //     }
      //   }
      //   test("colored"){
      //     import pprint.TPrintColors.Colors._
      //     def checkColor[T](expected: String)(implicit tprint: TPrint[T]) = {
      //       val tprinted = tprint.render.replace(
      //         fansi.Color.Green.escape, "<"
      //       ).replace(
      //         fansi.Color.Reset.escape, ">"
      //       )
      //       assert(tprinted == expected)
      //     }

      //     test - checkColor[String]("<String>")
      //     test - checkColor[Map[Int, String]]("<Map>[<Int>, <String>]")
      //     test - checkColor[collection.mutable.Seq[Int]]("<collection>.<mutable>.<Seq>[<Int>]")
      //     //      Not going to bother coloring these for now, since they're quite uncommon
      //     //      test - checkColor[{type T = Int; val x: String; def y: Char; var z: Double}](
      //     //                    "{type <T> = <Int>; val <x>: <String>; def <y>: <Char>; var <z>: <Double>}"
      //     //      )
      //     // test - checkColor[Map[K, V] forSome {
      //     //   type K <: Int; val x: Float; type V >: (String, Float with Double)
      //     // }](
      //     //   "<Map>[<K>, <V>] forSome { " +
      //     //     "type <K> <: <Int>; val <x>: <Float>; " +
      //     //     "type <V> >: (<String>, <Float> with <Double>) " +
      //     //     "}"
      //     // )
    }
  }

}
