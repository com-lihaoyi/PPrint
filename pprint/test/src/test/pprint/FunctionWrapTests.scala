package test.pprint

import utest._


object FunctionWrapTests extends TestSuite {

  def check[T](expected: String)(implicit tprint: pprint.TPrint[T]) = {
    val tprinted = tprint.render
    assert(expected.equals(tprinted))
  }

  val tests = Tests {
//    test("higherKindedFunctions"){
//      test("simple") {
//        check[(Int => Option[Int]) => Int]("(Int => Option[Int]) => Int")
//      }
//      test("lazy")(
//        check[() => (Int => Option[Int])]("() => Int => Option[Int]")
//      )
//      test("complex")(
//        check[
//          (Int => Option[Int]) =>
//            ((Int => String) => Int) =>
//              (Int => Int)
//        ]("(Int => Option[Int]) => ((Int => String) => Int) => Int => Int")
//      )
//    }
  }
}
