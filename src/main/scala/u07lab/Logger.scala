package u07lab

trait Logger {
  def log(msg: String)
}

class BasicLogger(init: String = "") extends Logger {
  override def log(msg: String): Unit = println(init + msg)
}

object NullLogger extends Logger {
  override def log(msg: String): Unit = ()
}