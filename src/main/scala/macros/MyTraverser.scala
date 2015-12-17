package macros

import java.awt.Dimension
import java.io.File

import scala.language.experimental.macros
import scala.reflect.macros.blackbox
import scala.reflect.runtime.universe._
import scala.swing.{Button, MainFrame}

object MyTraverser {
  class UI(text: String) extends MainFrame {
    title = "GUI Program #2"
    preferredSize = new Dimension(320, 240)
    contents = Button(text) {
      println(text)
    }
    visible = true
  }
  def prettyPrint(tree: Any, sp: Int = 0) {
    //   val ui = new UI(showRaw(tree))
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

      case defDef@DefDef(mods, name, tparams, vparamss, tpt, rhs) =>  // def x = ??? (val x = ???/getter/setter)
        println("--- enter DefDef ---", defDef.pos, defDef.pos.start, defDef.pos.end)
        println(s"mods $mods")
        println(s"name $name")
        println(s"tparams $tparams")
        println(s"vparamss $vparamss")
        println(s"tpt $tpt", tpt.pos)
        println(s"rhs $rhs", rhs.pos)
      //          println(s"tDef ${showRaw(body)}")

      case valDef@ValDef(mods, name, tpt, rhs) => // val x = ??? or var x = ???
        println("--- enter ValDef ---", "pos:"+valDef.pos,"focus:"+valDef.pos.focus,"point:"+valDef.pos.point,
          "column:"+valDef.pos.column,"line:"+valDef.pos.line, "start:"+valDef.pos.start, "end:"+valDef.pos.end,"focus:"+valDef.pos.focus,"???"+
            (valDef.pos.column-(valDef.pos.point-valDef.pos.start)-1),"SOP"+valDef.pos.startOrPoint)
        println(s"mods $mods")
        println(s"name $name")
        println(s"tpt $tpt", tpt.pos)
        println(s"rhs $rhs", rhs.pos)
        println(show(valDef))

      case x =>
        println("--- enter Default ---", x)
        println("> " * sp + showRaw(x))
    }
  }

  def interpretImpl[T](c: blackbox.Context)(code: c.Expr[T]) = {
    prettyPrint(code.tree)
    c.Expr(code.tree)
    new File(s"F:/Temp/Seliot/src/main/scala/macros/Out.scala").delete()
    c.literal(showRaw(code.tree))
  }

  def interpret[T](code: T): String = macro MyTraverser.interpretImpl[T]

}
