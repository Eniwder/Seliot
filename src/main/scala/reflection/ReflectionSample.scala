package reflection

import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => uni}

/**
  * Created by slab on 2015/11/30.
  */
object ReflectionSample extends App {
  val tree = reify {
    class Sample {
//    val im = 1
//    val mu = "myu myu myu"
      sayHello("Sanju", 3)
      def sayHello(name: String, n: Int) {
        println(s"Hello! $name " * n)
      }
    }

//    class B {
//      def x = 2
//    }
  }.tree

  print

  def print {
    println(show(tree))
    println(showRaw(tree))
  }


  class SampleClass {
    val im = 1
    val mu = "myu myu myu"

    def sayHello(name: String, n: Int) {
      println(s"Hello! $name " * n)
    }

  }


}
