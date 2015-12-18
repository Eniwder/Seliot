class Test {
  private val im = "immutable"
    var vr = 123

  var notInit:Int = _

  val plusVal:Int =  vr + 5

  val func1 = (s:String) => s*3
  val func2 = (s:String,n:Int) => s*n
  def func3(s:String,n:Int,f:(String,Int) => String):String = f(s,n)

  def doubleInt(x: Int): Int =
    x * 2

  implicit val impInt = 5

  def curriedFunc(one:Int)(two1:String,two2:String) = two1 * one + two2

  val partialApplied = curriedFunc(5) _

}