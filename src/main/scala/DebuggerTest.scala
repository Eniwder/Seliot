import org.scaladebugger.api.debuggers.LaunchingDebugger

object DebuggerTest extends App {

  val launchingDebugger = LaunchingDebugger(className = "src.main.scala.Test")

  launchingDebugger.start { s =>
    println("Launched and connected to JVM: " + s.uniqueId)
  }

}








