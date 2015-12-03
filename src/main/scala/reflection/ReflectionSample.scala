package reflection

import util.ReflectionUtil

import scala.reflect.runtime.universe._

/**
  * Created by slab on 2015/11/30.
  */
object ReflectionSample extends App with ReflectionUtil {
  val tree = reify {
    class Sample {
      //    val im = 1
      val mu = "myu myu myu"
      sayHello("Sanju", 3)
      def sayHello(name: String, n: Int) {
        println(s"Hello! $name " * n)
      }
    }
        class B {
          def x = 2
        }
  }.tree

  //print

  MyTraverser.prettyPrint(tree)

  //  def print {
  //    println(tree)
  //    println(show(tree))
  //    println(showRaw(tree))
  //  }

  object MyTraverser extends Traverser {
    var applies = List[Apply]()
    override def traverse(tree: Tree): Unit = tree match {
      case app@Apply(fun, args) =>
        applies = app :: applies
        super.traverse(fun)
        super.traverseTrees(args)
      case _ => super.traverse(tree)
    }

    def prettyPrint(tree: Tree, sp: Int = 0): Unit = tree match {
      case block@Block(stats, expr) => // exprはいらない？
        println("--- enter Block ---")
        println(showRaw(stats))
        println()
        stats.foreach(prettyPrint(_, sp + 1))
      //        prettyPrint(stats)
      case list@List(xs) =>
        println("--- enter List ---")
        println(showRaw(xs))
        println()


      // case app@Apply(fun, args) =>
      //        applies = app :: applies
      //        super.traverse(fun)
      //        super.traverseTrees(args)
      case x =>
        println("--- enter Default ---")
        println("> " * sp + showRaw(x))
        println()
    }
  }


  //  class SampleClass {
  //    val im = 1
  //    val mu = "myu myu myu"
  //
  //    def sayHello(name: String, n: Int) {
  //      println(s"Hello! $name " * n)
  //    }
  //
  //  }


}