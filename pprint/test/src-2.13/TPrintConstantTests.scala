import pprint.{TPrint, TPrintColors}
import utest._

object TPrintConstantTests extends TestSuite{

  val tests = TestSuite{
    test("constant"){
      assert(
        implicitly[TPrint[123]].render(TPrintColors.Colors) == fansi.Color.Green("123"),
        implicitly[TPrint["xyz"]].render(TPrintColors.Colors)  == fansi.Color.Green("\"xyz\""),
        implicitly[TPrint[Seq[true]]].render(TPrintColors.Colors)  == fansi.Color.Green("Seq") ++ "[" ++ fansi.Color.Green("true") ++ "]"
      )
    }
  }
}

