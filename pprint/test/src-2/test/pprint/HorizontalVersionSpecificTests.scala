package test.pprint

import utest._
import scala.collection.{immutable => imm, mutable}

object HorizontalVersionSpecificTests extends TestSuite{
  val Check = new Check(9999)
  val tests = TestSuite{
    test("Stream") - Check(
      Stream('omg, 'wtf, 'bbq),
      """Stream('omg, 'wtf, 'bbq)""",
      """Stream(Symbol(omg), <not computed>)"""
    )
  }
}
