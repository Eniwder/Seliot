import java.io.{File, PrintWriter}

import util.{LoanPattern, ReflectionUtil}

import scala.io.Source
import scala.language.experimental.macros

object Runner extends App with LoanPattern with ReflectionUtil {
  val currentPath = "F:/Temp/Seliot/src/main/scala"
  val trgPath = new File(s"$currentPath/Test.scala")
  val trg = using(Source.fromFile(s"$trgPath")) { in =>
    in.getLines().mkString("\n")
  }.get // とりあえず

  val prefix = "Out"
  val suffix = "scala"
  val tmp = new File(s"$currentPath/$prefix.$suffix")

  val className = prefix

  using(new PrintWriter(s"$currentPath/$prefix.$suffix")) { o =>
    o.write( s"""|object $className{ val tree = MyTraverser.interpret{$trg
                 |}{\"\"\"$trg\"\"\"
                 |}}
    """.stripMargin)
  }

  //println(tb.parse(trg))


  /* tb.eval(tb.parse(s"""
                       |object $className extends App{
                       |macros.MyTraverser.interpret{
                       |$trg
                       |}
                       |}
     """.stripMargin))
 */

}
