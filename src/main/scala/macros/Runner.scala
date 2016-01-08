package macros

import java.io.{PrintWriter, File}

import util.{LoanPattern, ReflectionUtil}

import scala.io.Source
import scala.language.experimental.macros

object Runner extends App with LoanPattern with ReflectionUtil {
  val trgPath = new File("F:/Temp/Seliot/src/main/scala/macros/Test.scala")
  val trg = using(Source.fromFile(s"$trgPath")) { in =>
    in.getLines().mkString("\n")
  }.get // とりあえず

  val prefix = "Out"
  val suffix = "scala"
  val tmp = new File(s"F:/Temp/Seliot/src/main/scala/macros/$prefix.$suffix")

  val className = prefix

  using(new PrintWriter(s"F:/Temp/Seliot/src/main/scala/macros/$prefix.$suffix")) { o =>
    o.write( s"""|object $className{ val tree = macros.MyTraverser.interpret{$trg
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
