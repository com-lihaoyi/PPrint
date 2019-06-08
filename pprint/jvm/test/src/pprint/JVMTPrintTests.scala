//package pprint
//
//import pprint.TPrint
//import utest._
//
//object JVMTPrintTests extends TestSuite{
//
//  class M
//
//  val tests = TestSuite{
//    //
//    type X = scala.Int with scala.Predef.String{}
//    val x = ""
//
//    'infix {
//      object X {
//        class ++[Int, String]
//      }
//
//      check[X.++[Int, String]]("X.++[Int, String]")
//      check[Either[Int, String]]("Either[Int, String]")
//
//      import X.++
//      check[Int ++ String]("Int ++ String")
//      check[++[Int, String]]("Int ++ String")
//
//      import shapeless._
//
//      check[Int :: String :: (Char, Seq[Float]) :: HNil](
//        "Int :: String :: (Char, Seq[Float]) :: HNil"
//      )
//
//    }
//  }
//
//}