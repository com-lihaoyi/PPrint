/**
  * Created by lihaoyi on 17/5/17.
  */
object TestMain {
  def main(args: Array[String]): Unit = {

    var i = 0;
    var length = 0
    val start = System.currentTimeMillis()
    case class FooG[T](is: T, strings: Seq[String])
    case class Foo(i: Int, s: Seq[String])
    val struct = FooG(
      Vector(
        FooG(Array(Foo(123, Seq("1", "2", "3"))), Seq("hello", "1"))
      ),
      List.tabulate(10)(_.toString)
    )
    while(System.currentTimeMillis() < start + 5000){
      pprint.tokenize(struct).foreach(x => length += x.length)
      i += 1
    }
    val end = System.currentTimeMillis()
    println("iterations: " + i)

    println("time: " + (end - start))
    println(pprint.tokenize(struct, width = 40).mkString)
  }
}
