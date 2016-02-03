import java.io.File

import util.ReflectionUtil

import scala.language.experimental.macros
import scala.reflect.macros.blackbox
import scala.reflect.runtime.universe._

object MyTraverser extends ReflectionUtil {

  val classes = scala.collection.mutable.Buffer[Class]()
  val module = new Class("")

  class AnimationWindow(tree: Any, source: String) {// extends MainFrame {
    //    title = "Animation Window"
    //    preferredSize = new Dimension(800, 600)
    //    visible = true

    itp(tree)

    // invokeObjectMethod("Test","combinations",2,List('a, 'b, 'c, 'd, 'e)) // バグる

    // for debug 普段は中身が消されてるので表示されない
    // outBlock,outCallStackをコメントアウトすれば出てくる
    for {
      clazz <- classes
      callStack <- clazz.allCallStacks.zipWithIndex
      block <- callStack._1.allScope.zipWithIndex
    } {
      println(s"class    :$clazz")
      println(s"callStack:${">" * callStack._2}${callStack._1}")
      println(s"block    :${" " * callStack._2}|${">" * block._2}${block._1}")
      println(s"variable :${" " * callStack._2}|${" " * block._2}${block._1.declMap.mkString(" , ")}")
    }

    def itp(tree: Any): Any = {

      // return (ClassName,FunctionName)
      def funcData(func: String): (String, String) = {
        println(func)
        //        val fntmp = func.toString.split("this.") // this.っていう余計な修飾子を排除
//        val fntmp2 = if (fntmp.length != 1) fntmp(1) else fntmp(0) // 上の続き
        val fntmp = func.toString.split('.')
        //val (className, funcName) = fntmp2.splitAt(fntmp2.lastIndexOf('.'))
        (fntmp(fntmp.length - 2), fntmp(fntmp.length - 1))
        //       (className, funcName.tail) // funcNameから余計な"."を取って返す
      }

      //   val ui = new UI(showRaw(tree))
      println()
      tree match {
        case block@Block(stats, expr) => // exprはいらない？
          println("--- enter Block ---", s"line: ${block.pos.line} range: ${block.pos.column - (block.pos.point - block.pos.start) - 1} - ${block.pos.end - block.pos.start}")
          module.currentCallStack.intoBlock()
          println(expr)
          stats.foreach(itp(_))
          module.currentCallStack.outBlock()

        case list@List(xs) =>

          println("--- enter List ---", list)
          println(showRaw(xs))

//        case classDef@ClassDef(mods, name, tparams, impl) =>
//          println("--- enter ClassDef ---", s"[$name]", s"line     : ${classDef.pos.line} range: ${classDef.pos.column - (classDef.pos.point - classDef.pos.start) - 1} - ${classDef.pos.end - classDef.pos.start}")
//          //   println(s"mod $mods")
//          println(s"tDef     : $tparams")
//          println(s"template : ${showRaw(impl)}")
//          classes += new Class(name.toString)
//          itp(impl)

        case template@Template(parents, self, body) =>
          println("--- enter Template ---", s"line  : ${template.pos.line} range: ${template.pos.column - (template.pos.point - template.pos.start) - 1} - ${template.pos.end - template.pos.start}")
          println(s"mod   : $parents")
          println(s"tName : $self")
          //  println(s"tDef ${showRaw(body)}")
          body.foreach(itp(_))

        case defDef@DefDef(mods, name, tparams, vparamss, tpt, rhs) =>  // def x = ??? (val x = ???/getter/setter)
          println("--- enter DefDef ---", s"[$name]", s"line     : ${defDef.pos.line} range: ${defDef.pos.column - (defDef.pos.point - defDef.pos.start) - 1} - ${defDef.pos.end - defDef.pos.start}")
          val prefix = if (defDef.pos.point - defDef.pos.start == 0) " // " else ""
          println(s"$prefix mods     : $mods")
          println(s"$prefix tparams  : $tparams")
          println(s"$prefix vparamss : $vparamss")
          println(s"$prefix tpt      : $tpt")
          print(s"$prefix rhs--->")
          itp(rhs)
        //    println(s"tDef ${showRaw(defDef)}")

        case valDef@ValDef(mods, name, tpt, rhs) => // val x = ??? or var x = ???
          println("--- enter ValDef ---", s"[$name]", s"line : ${valDef.pos.line} range: ${valDef.pos.column - (valDef.pos.point - valDef.pos.start) - 1} - ${valDef.pos.end - valDef.pos.start}")
          println(s"tpt  : $tpt")
          println(s"rhs  : $rhs")
          val value = itp(rhs)
          println("<><><><><><>" + value)
          module.currentCallStack.currentScope.putVariable(name.toString, value)
        //    println(showRaw(valDef))

        case modDef@ModuleDef(mods, name, impl) =>
          println("--- enter ModuleDef ---", s"[$name]")
          println(s"mods  : $mods")
          println(s"impl  : $impl")
          println(s"line : ${modDef.pos.line} range: ${modDef.pos.column - (modDef.pos.point - modDef.pos.start) - 1} - ${modDef.pos.end - modDef.pos.start}")
          itp(impl)

        case ifCond@If(cond, thenp, elsep) =>
          println("--- enter If ---", s"line : ${ifCond.pos.line} range: ${ifCond.pos.column - (ifCond.pos.point - ifCond.pos.start) - 1} - ${ifCond.pos.end - ifCond.pos.start}")
          println(s"cond  : $cond")
          print(s"--thenp : --->")
          itp(thenp)
          print(s"--elsep : --->")
          itp(elsep)
          println("--- Out If ---")

        case apply@Apply(fun, args) =>
          println("--- enter Apply ---", s"[$fun]")
          fun collect { case ap@Apply(_, _) => itp(ap) }
          val argValues = args map itp
          val (className, funcName) = funcData(fun.toString())
          println(className, funcName)
          println("*************" + argValues)
          println(s"args  : $args")
          if (className.contains("super") || (funcName.contains("<") && funcName.contains(">"))) {
            // とりあえず何もしない
          } else {
            if (funcName == "apply" || funcName.contains("apply[")) {
              className match {
                case "Symbol" => Symbol(argValues.head.toString)
                case "List" => List(argValues: _*)
                case _ =>
              }
            }
          }

        case mat@Match(selector, cases) =>
          println("--- enter Match ---", s"line : ${mat.pos.line} range: ${mat.pos.column - (mat.pos.point - mat.pos.start) - 1} - ${mat.pos.end - mat.pos.start}")
          println(s"selector : $selector")
          print(s"cases    : --->")
          cases.foreach(itp(_))

        case caseDef@CaseDef(pat, guard, body) =>
          println("--- enter Match ---", s"line : ${caseDef.pos.line} range: ${caseDef.pos.column - (caseDef.pos.point - caseDef.pos.start) - 1} - ${caseDef.pos.end - caseDef.pos.start}")
          println(s"pat   : $pat")
          println(s"guard : $guard")
          print(s"body  : ---->")
          itp(body)
//        case imprt@Import(expr, selectors) =>
//          println("--- enter Import ---")
//          println(s"expr      : $expr")
//          println(s"selectors : $selectors")

        case lit@Literal(value) =>
          println("--- enter Literal ---", s"line : ${lit.pos.line} range: ${lit.pos.column - (lit.pos.point - lit.pos.start) - 1} - ${lit.pos.end - lit.pos.start}")
          println(s"value : $value")
          itp(value)

        case const@Constant(value) =>
          println("--- enter Constant ---", s"[$value]")
          val x = value.toString
          // とりあえず文字列か整数か判定
          if (x.toCharArray.exists(!_.isDigit)) x else x.toInt

        case id@Ident(ident) =>
          println("--- enter Ident ---", s"[$ident]")
          id

        case x =>
          println("--- enter Default ---", x)
          println(showRaw(x))
      }
    }

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
        print(s"--thenp : --->")
        prettyPrint(thenp, sp + 1)
        print(s"--elsep : --->")
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
        print(s"cases    : --->")
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

    new AnimationWindow(code.tree, source.toString)









    new File(s"F:/Temp/Seliot/src/main/scala/Out.scala").delete()
    c.literal(showRaw(code.tree))
  }

  def interpret[T](code: T)(raw: String): String = macro MyTraverser.interpretImpl[T]

}

