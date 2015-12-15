package reflection

import util.ReflectionUtil

import scala.language.experimental.macros
import scala.reflect.macros.blackbox
import scala.reflect.runtime.universe._

/**
  * Created by slab on 2015/11/30.
  */
object ReflectionSample extends App with ReflectionUtil {
  val tree = reify {
    class Sample[E] {
      //    val im = 1
      val mu = "myu myu myu"
      sayHello("Sanju", 3)
      def sayHello(name: String, n: Int) {
        println(s"Hello! $name " * n)
      }
    }
    //    class B {
    //      def x = 2
    //    }
  }.tree

  //print

   MyTraverser.prettyPrint(tree)
  //MyTraverser.prettyPrint(q"def a(b:Int):Int = b+1"

  //  def print {
  //    println(tree)
  //    println(show(tree))
  //    println(showRaw(tree))
  //  }

  object MyTraverser extends Traverser {
    //    var applies = List[Apply]()
    //    override def traverse(tree: Tree): Unit = tree match {
    //      case app@Apply(fun, args) =>
    //        applies = app :: applies
    //        super.traverse(fun)
    //        super.traverseTrees(args)
    //      case _ => super.traverse(tree)
    //    }

    def prettyPrint(tree: Any, sp: Int = 0) {
      println()
      tree match {
        case block@Block(stats, expr) => // exprはいらない？
          println("--- enter Block ---", block.pos)
          println(showRaw(stats))
          stats.foreach(prettyPrint(_, sp + 1))
        //        prettyPrint(stats)
        case list@List(xs) =>
          println("--- enter List ---", list)
          println(showRaw(xs))

        case classDef@ClassDef(mods, name, tparams, impl) =>
          println("--- enter ClassDef ---", classDef.pos)
          println(s"mod $mods")
          println(s"tName $name")
          println(s"tDef $tparams")
          println(s"template ${showRaw(impl)}")
          prettyPrint(impl, sp + 1)

        case template@Template(parents, self, body) =>
          println("--- enter Template ---", template.pos)
          println(s"mod $parents")
          println(s"tName $self")
          println(s"tDef ${showRaw(body)}")
          body.foreach(prettyPrint(_, sp + 1))

        case defdef@DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
          println("--- enter DefDef ---", defdef.pos,defdef.pos.start,defdef.pos.end)
          println(s"mods $mods")
          println(s"name $name")
          println(s"tparams $tparams")
          println(s"vparamss $vparamss")
          println(s"tpt $tpt", tpt.pos)
          println(s"rhs $rhs", rhs.pos)
        //          println(s"tDef ${showRaw(body)}")


        case x =>
          println("--- enter Default ---", x)
          println("> " * sp + showRaw(x))
      }
    }

//    def interpretImpl(c: blackbox.Context): c.Expr[String] = {
//      println("here")
//      println(c.universe.typeTag[SampleClass].tpe.typeSymbol.pos)
//      println(c.openMacros.mkString("\n"))
//      //      MyTraverser.prettyPrint(q"$a")
//      c.literal("aaa")
//    }

    def posImpl[T](c: blackbox.Context)(code: c.Expr[T]) = {
      prettyPrint(code.tree)
      c.literal(showRaw(code.tree))
    }

  }


  object SampleClass {
    val im = 1
    val mu = "myu myu myu"

    def sayHello(name: String, n: Int) {
      println(s"Hello! $name " * n)
    }

    def pos[T](code: T): String = macro MyTraverser.posImpl[T]
  }


}
