package test.pprint
import pprint.{PPrinter, Truncated, Util}
import utest._

import scala.collection.mutable
class Check(width: Int = 100, height: Int = 99999){
  def apply(t: Any, expected: String*) = {
    val pprinted = PPrinter.BlackWhite.tokenize(t, width, height).map(_.plainText).mkString
    utest.asserts.assert(expected.map(_.trim).contains(pprinted))
  }
}