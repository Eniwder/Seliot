package reflection

import util.ReflectionUtil

import scala.reflect.runtime.{universe => uni}

object OldSample extends App with ReflectionUtil {

  def getType[A: uni.TypeTag](x: A): uni.Type = uni.typeOf[A]

    println(uni.typeOf[SampleClass])
    println(uni.typeOf[CCSample])
    println(getType((a: Int) => 1))
    println(getType((a: Int) => 1.0))

    println(uni.typeOf[SampleClass].member(uni.TermName("im")))
    println(uni.typeOf[SampleClass].member(uni.TermName("mu")))


    println(uni.typeOf[List[Int]])
    println(uni.typeOf[List[Int]].member(uni.TermName("map")))

    val sampleClass = new SampleClass()
    val runtimeMirror = uni.typeTag[SampleClass].mirror
    val instanceMirror = runtimeMirror.reflect(sampleClass)
    val muField = uni.typeOf[SampleClass]
      .member(uni.TermName("mu"))
      .asMethod
      .getter
      .asMethod

    val muFieldMirror = instanceMirror.reflectField(muField)
    println("muFieldMirror = " + muFieldMirror.get)

    invokeMethod(sampleClass, "sayHello", "Shizu", 5)
    println("im = " + getFieldValue(sampleClass, "im"))

}

class SampleClass {
  val im = 1
  val mu = "myu myu myu"

  def sayHello(name: String, n: Int) {
    println(s"Hello! $name " * n)
  }

}

case class CCSample(x: Int)