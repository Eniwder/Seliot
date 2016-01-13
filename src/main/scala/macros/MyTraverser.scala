package macros

import java.awt.Dimension
import java.io.File

import scala.language.experimental.macros
import scala.reflect.macros.blackbox
import scala.reflect.runtime.universe._
import scala.swing.MainFrame

object MyTraverser {

  class AnimationWindow(tree: Any, source: String) extends MainFrame {
    title = "Animation Window"
    preferredSize = new Dimension(800, 600)
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
        println("--- enter Block ---", s"line: ${block.pos.line} range: ${block.pos.column - (block.pos.point - block.pos.start) - 1} - ${block.pos.end - block.pos.start}")
        //      println(showRaw(stats))
        stats.foreach(prettyPrint(_, sp + 1))
      //        prettyPrint(stats)
      case list@List(xs) =>
        println("--- enter List ---", list)
        println(showRaw(xs))

      case classDef@ClassDef(mods, name, tparams, impl) =>
        println("--- enter ClassDef ---", s"[$name]", s"line     : ${classDef.pos.line} range: ${classDef.pos.column - (classDef.pos.point - classDef.pos.start) - 1} - ${classDef.pos.end - classDef.pos.start}")
        //   println(s"mod $mods")
        println(s"tDef     : $tparams")
        println(s"template : ${showRaw(impl)}")
        prettyPrint(impl, sp + 1)

      case template@Template(parents, self, body) =>
        println("--- enter Template ---", s"line  : ${template.pos.line} range: ${template.pos.column - (template.pos.point - template.pos.start) - 1} - ${template.pos.end - template.pos.start}")
        println(s"mod   : $parents")
        println(s"tName : $self")
        //  println(s"tDef ${showRaw(body)}")
        body.foreach(prettyPrint(_, sp + 1))

      case defDef@DefDef(mods, name, tparams, vparamss, tpt, rhs) =>  // def x = ??? (val x = ???/getter/setter)
        println("--- enter DefDef ---", s"[$name]", s"line     : ${defDef.pos.line} range: ${defDef.pos.column - (defDef.pos.point - defDef.pos.start) - 1} - ${defDef.pos.end - defDef.pos.start}")
        val prefix = if (defDef.pos.point - defDef.pos.start == 0) " // " else ""
        println(s"$prefix mods     : $mods")
        println(s"$prefix tparams  : $tparams")
        println(s"$prefix vparamss : $vparamss")
        println(s"$prefix tpt      : $tpt")
        print(s"$prefix rhs--->")
        prettyPrint(rhs, sp + 1)

      //    println(s"tDef ${showRaw(defDef)}")

      case valDef@ValDef(mods, name, tpt, rhs) => // val x = ??? or var x = ???
        println("--- enter ValDef ---", s"[$name]", s"line : ${valDef.pos.line} range: ${valDef.pos.column - (valDef.pos.point - valDef.pos.start) - 1} - ${valDef.pos.end - valDef.pos.start}")
        println(s"tpt  : $tpt")
        println(s"rhs  : $rhs")
      //    println(showRaw(valDef))

      case modDef@ModuleDef(mods, name, impl) =>
        println("--- enter ModuleDef ---", s"[$name]")
        println(s"mods  : $mods")
        println(s"impl  : $impl")
        println(s"line : ${modDef.pos.line} range: ${modDef.pos.column - (modDef.pos.point - modDef.pos.start) - 1} - ${modDef.pos.end - modDef.pos.start}")
        prettyPrint(impl)

      case ifCond@If(cond, thenp, elsep) =>
        println("--- enter If ---", s"line : ${ifCond.pos.line} range: ${ifCond.pos.column - (ifCond.pos.point - ifCond.pos.start) - 1} - ${ifCond.pos.end - ifCond.pos.start}", sp)
        println(s"cond  : $cond")
        println(s"--thenp : $thenp")
        prettyPrint(thenp, sp + 1)
        println(s"--elsep : $elsep")
        prettyPrint(elsep, sp + 1)
        println("--- Out If ---", sp)

      case apply@Apply(fun, args) =>
        println("--- enter Apply ---", s"[$fun]")
        fun collect { case ap@Apply(_, _) => prettyPrint(ap) }
        println(s"args  : $args")

      case imprt@Import(expr, selectors) =>
        println("--- enter Import ---")
        println(s"expr      : $expr")
        println(s"selectors : $selectors")

      case mat@Match(selector, cases) =>
        println("--- enter Match ---", s"line : ${mat.pos.line} range: ${mat.pos.column - (mat.pos.point - mat.pos.start) - 1} - ${mat.pos.end - mat.pos.start}")
        println(s"selector : $selector")
        println(s"cases    : $cases")
        cases.foreach(prettyPrint(_, sp + 1))

      case caseDef@CaseDef(pat, guard, body) =>
        println("--- enter Match ---", s"line : ${caseDef.pos.line} range: ${caseDef.pos.column - (caseDef.pos.point - caseDef.pos.start) - 1} - ${caseDef.pos.end - caseDef.pos.start}")
        println(s"pat   : $pat")
        println(s"guard : $guard")
        print(s"body  : ---->")
        prettyPrint(body)

      case x =>
        println("--- enter Default ---", x)
        println("> " * sp + showRaw(x))
    }
  }

  def interpretImpl[T](c: blackbox.Context)(code: c.Expr[T])(raw: c.Expr[String]) = {
    prettyPrint(code.tree)
    val source = raw.tree.asInstanceOf[Any] match {
      case Literal(Constant(str)) => str
      case _ => ""
    }

    println(source)
    new File(s"F:/Temp/Seliot/src/main/scala/macros/Out.scala").delete()
    c.literal(showRaw(code.tree))
  }

  def interpret[T](code: T)(raw: String): String = macro MyTraverser.interpretImpl[T]

}

