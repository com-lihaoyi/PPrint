package test.pprint

import utest._
import scala.collection.{immutable => imm, mutable}

object HorizontalVersionSpecificTests extends TestSuite{

  val Check = new Check(100, 9999, false, false)

  val tests = TestSuite{
    // Only show LazyList contents if they've been computed already
    test("LazyList") {
      test - Check(
        { val l = LazyList("omg", "wtf", "bbq"); l.toArray; l },
        """LazyList("omg", "wtf", "bbq")"""
      )
      test - Check(
        "omg" #:: LazyList.empty,
        """LazyList(<not computed>)"""
      )
    }
  }


}
