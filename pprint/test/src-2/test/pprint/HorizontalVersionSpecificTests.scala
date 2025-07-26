package test.pprint

import utest._
import scala.collection.{immutable => imm, mutable}

object HorizontalVersionSpecificTests extends TestSuite{
  val Check = new Check(9999)
  val tests = TestSuite{
    // Streams are hard-coded to always display vertically, in order
    // to make streaming pretty-printing sane
    test("Stream") - Check(
      Stream('omg, 'wtf, 'bbq),
      """Stream('omg, 'wtf, 'bbq)"""
    )
  }
}
