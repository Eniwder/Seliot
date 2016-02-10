class Class(name: String) {
  private val callStacks = scala.collection.mutable.Buffer[CallStack]()
  private var currentCallStackN = -1

  intoCallStack("global")

  def intoCallStack(path: String) {
    callStacks += new CallStack(path)
    currentCallStackN += 1
  }

  def outCallStack() {
    callStacks.remove(callStacks.length - 1)
    currentCallStackN -= 1
  }

  def currentCallStack: CallStack = callStacks(currentCallStackN)

  def allCallStacks: List[CallStack] = callStacks.toList

}

class CallStack(path: String) {
  private val scopes = scala.collection.mutable.Buffer[Scope]()
  private var currentScopeN = 0
  scopes += new Scope()

  def intoBlock() {
    scopes += new Scope()
    currentScopeN += 1
  }

  def outBlock() {
    scopes.remove(scopes.length - 1)
    currentScopeN -= 1
  }

  def currentScope: Scope = scopes(currentScopeN)

  def allScope: List[Scope] = scopes.toList

}

class Scope {
  // 変数名,値を保存する
  private val decls = scala.collection.mutable.Map[String, Any]()

  def putVariable(name: String, value: Any) {
    decls.put(name, value)
  }

  def declMap: Map[String, Any] = decls.toMap

}