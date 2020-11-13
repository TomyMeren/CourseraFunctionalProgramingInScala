package week4_3

class Var[T](expr: => T) extends Signal[T](expr) {
  override def update(expr: => T): Unit = super.update(expr) // Update ya esta implementada en Signal
}

object Var {
  def apply[T](expr: => T) = new Var(expr)
}
