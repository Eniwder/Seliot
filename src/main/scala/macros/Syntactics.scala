class Syntactics {
  private val im = "immutable"
  var vr = 123

  var notInit: Int = _

  val plusVal: Int = vr + 5

  val func1 = (s: String) => s * 3
  val func2 = (s: String, n: Int) => s * n
  def func3(s: String, n: Int, f: (String, Int) => String): String = f(s, n)

  func3("aiu", 3, _ * _)

  def doubleInt(x: Int): Int =
    x * 2

  private[this] def doubleInt2(x: Int): Int = x * 2

  implicit val implInt = 5

  def curriedFunc(one: Int)(two1: String, two2: String) = two1 * one + two2

  val partialApplied = curriedFunc(5) _

  val fy = for (i <- 1 to 10) yield i * 2

  def outerDef(x: Any)(implicit intImpl: Int) {
    def innerDef(z: Any): Unit = {
      println(z)
    }
    innerDef(x)
  }

  curriedFunc(1)("2s","3s")
  outerDef("please print")

  val ifel = if(1 == 2) "a" else "b"

  if(2 == plusVal){
    println("in if")
  }else if(3 == readLine().toInt){
    println("in if else")
  }else{
    println("in else")
  }

  class Inner {

  }

  object Inner {
    val abc = "cba"
  }

  trait InnerTrait {
    self =>
  }

  import scala.io.{Source => is}
  is.hashCode()

}