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

  // tpt = type tag ?
  // rhs = right-head side(右辺)
  // tparam = type parameter ?
  // vparam = value parameter ?

  def prettyPrint(tree: Any, sp: Int = 0) {
    //   val ui = new UI(showRaw(tree))
    println()
    tree match {
      case block@Block(stats, expr) => // exprはいらない？
        println("--- enter Block ---", block.pos)
        //      println(showRaw(stats))
        println(s"line: ${block.pos.line} range: ${block.pos.column - (block.pos.point - block.pos.start) - 1} - ${block.pos.end - block.pos.start}")
        stats.foreach(prettyPrint(_, sp + 1))
      //        prettyPrint(stats)
      case list@List(xs) =>
        println("--- enter List ---", list)
        println(showRaw(xs))

      case classDef@ClassDef(mods, name, tparams, impl) =>
        println("--- enter ClassDef ---", s"$name")
        //   println(s"mod $mods")
        println(s"line     : ${classDef.pos.line} range: ${classDef.pos.column - (classDef.pos.point - classDef.pos.start) - 1} - ${classDef.pos.end - classDef.pos.start}")
        println(s"tDef     : $tparams")
        println(s"template : ${showRaw(impl)}")
        prettyPrint(impl, sp + 1)

      case template@Template(parents, self, body) =>
        println("--- enter Template ---", template.pos)
        println(s"mod   : $parents")
        println(s"tName : $self")
        println(s"line  : ${template.pos.line} range: ${template.pos.column - (template.pos.point - template.pos.start) - 1} - ${template.pos.end - template.pos.start}")
        //  println(s"tDef ${showRaw(body)}")
        body.foreach(prettyPrint(_, sp + 1))

      case defDef@DefDef(mods, name, tparams, vparamss, tpt, rhs) =>  // def x = ??? (val x = ???/getter/setter)
        println("--- enter DefDef ---", s"[$name]")
        val prefix = if (defDef.pos.point - defDef.pos.start == 0) " // " else ""
        println(s"$prefix mods     : $mods")
        println(s"$prefix tparams  : $tparams")
        println(s"$prefix vparamss : $vparamss")
        println(s"$prefix tpt      : $tpt")
        println(s"$prefix rhs      : $rhs")
        println(s"$prefix line     : ${defDef.pos.line} range: ${defDef.pos.column - (defDef.pos.point - defDef.pos.start) - 1} - ${defDef.pos.end - defDef.pos.start}")
      //    println(s"tDef ${showRaw(defDef)}")

      case valDef@ValDef(mods, name, tpt, rhs) => // val x = ??? or var x = ???
        println("--- enter ValDef ---", s"[$name]")
        println(s"tpt  : $tpt")
        println(s"rhs  : $rhs")
        println(s"line : ${valDef.pos.line} range: ${valDef.pos.column - (valDef.pos.point - valDef.pos.start) - 1} - ${valDef.pos.end - valDef.pos.start}")
      //    println(showRaw(valDef))

      case modDef@ModuleDef(mods, name, impl) =>
        println("--- enter ModuleDef ---", s"[$name]")
        println(s"mods  : $mods")
        println(s"impl  : $impl")
        println(s"line : ${modDef.pos.line} range: ${modDef.pos.column - (modDef.pos.point - modDef.pos.start) - 1} - ${modDef.pos.end - modDef.pos.start}")
        prettyPrint(impl)

      case ifCond@If(cond, thenp, elsep)=>
        println("--- enter If ---")
        println(s"cond  : $cond")
        println(s"--thenp : $thenp")
        prettyPrint(thenp)
        println(s"--elsep : $elsep")
        prettyPrint(elsep)
        println(s"line : ${ifCond.pos.line} range: ${ifCond.pos.column - (ifCond.pos.point - ifCond.pos.start) - 1} - ${ifCond.pos.end - ifCond.pos.start}")

      case apply@Apply(fun, args) =>
        println("--- enter Apply ---", s"[$fun]")
        fun collect {case ap@Apply(_,_) => prettyPrint(ap)}
        println(s"args  : $args")

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
