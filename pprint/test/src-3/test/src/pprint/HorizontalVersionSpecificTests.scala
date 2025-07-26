package test.pprint

import utest._
import scala.collection.{immutable => imm, mutable}

object HorizontalVersionSpecificTests extends TestSuite{

  val Check = new Check(100, 9999, false, false)

  val tests = TestSuite{
    // Streams are hard-coded to always display vertically, in order
    // to make streaming pretty-printing sane
    test("LazyList") - Check(
      LazyList("omg", "wtf", "bbq"),
      """LazyList("omg", "wtf", "bbq")"""
    )

  }


}
