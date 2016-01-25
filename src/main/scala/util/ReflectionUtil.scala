package util

import scala.reflect.ClassTag
import scala.reflect.runtime.{universe => uni}
import scala.tools.reflect.ToolBox

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

  // メソッド呼び出し
  def invokeObjectMethod[T: uni.TypeTag : ClassTag, B](name:String, methodName: String, args: Any*): B = {
    val runtimeMirror = uni.runtimeMirror(getClass.getClassLoader)
    val module =  uni.typeOf[T].termSymbol.asModule
    val theType = uni.typeTag[T].tpe
    val mm = runtimeMirror.reflectModule(module)

    val instanceMirror = runtimeMirror.reflect(mm.instance)

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

  /** コンパニオンオブジェクトのメソッドを、リフレクション使って呼び出す
    *
    * @param className  呼び出すmethodがあるobject名
    * @param methodName メソッドの名前
    * @param params     (パラメータの型、実際のパラメータ) の要素2のTupleの可変長
    */
  def invokeCompanionMethod[B](className: String, methodName: String, params: (java.lang.Class[_], AnyRef)*): B = {
    val clazz = Class.forName(className + "$") //コンパニオンのクラスのClassを取得
    val obj = clazz.getField("MODULE$").get(null) // MODULE$というstaticなfieldにインスタンスが保持されているはずなので、それを取得
    val method = clazz.getMethod(methodName, params.map(_._1): _*) //呼び出すMethodのオブジェクトを取得
    method.invoke(obj, params.map(_._2): _*).asInstanceOf[B] //メソッドをリフレクション使って呼び出す
  }


  val cm = uni.runtimeMirror(getClass.getClassLoader)

  val tb = cm.mkToolBox()

}
