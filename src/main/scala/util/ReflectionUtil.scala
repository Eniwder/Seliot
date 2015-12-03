package util

import scala.reflect.ClassTag
import scala.reflect.runtime.{universe => uni}

trait ReflectionUtil {

  // メソッド呼び出し
  def invokeMethod[T: uni.TypeTag : ClassTag, B](instance: T, methodName: String, args: Any*): B = {
    val typeTag = uni.typeTag[T]
    val theType = typeTag.tpe
    val runtimeMirror = typeTag.mirror

    val instanceMirror = runtimeMirror.reflect(instance)

    val methodSymbol = theType.member(uni.TermName(methodName)).asMethod
    val methodMirror = instanceMirror.reflectMethod(methodSymbol)

    methodMirror(args: _*).asInstanceOf[B]
  }

  // フィールドの値取得
  def getFieldValue[T: uni.TypeTag : ClassTag, B](instance: T, fieldName: String): B = {
    val typeTag = uni.typeTag[T]
    val theType = uni.typeOf[T]
    val runtimeMirror = typeTag.mirror

    val instanceMirror = runtimeMirror.reflect(instance)

    val getterSymbol = theType.member(uni.TermName(fieldName)).asMethod.getter.asMethod
    val fieldMirror = instanceMirror.reflectField(getterSymbol)
    fieldMirror.get.asInstanceOf[B]
  }

  val cm = uni.runtimeMirror(getClass.getClassLoader)

}
