package reflection

import org.scalatest.{Matchers, FlatSpec}
import util.ReflectionUtil

/**
  * Created by slab on 2016/02/04.
  */
class ReflectionTest extends FlatSpec with Matchers with ReflectionUtil{
  "List map " should "should Success" in {
    invokeMethod(new Sample,"testM",20)
    println(invokeMethod(List(1,2,3),"map",(x:Int) => x+1))
  }
}


class Sample{
  val x =30
  def testM(y:Int): Unit ={
    println(y*10)
  }
}