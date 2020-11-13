package week4_3

class Signal[T](expr: => T) {
  import week4_3.Signal._

  var myValue: T = _
  var myExpr: () => T = _
  var observers: Set[Signal[_]] = Set()
  update(expr)

  protected def update(expr: => T): Unit = {
    myExpr = () => expr
    computeValue()
  }

  protected def computeValue(): Unit = {
    val newValue = caller.withValue(this)(myExpr())
    //si myExpr() depende de otros Signal, estos se almacenan en caller hasta que se evalua y se vacia

    if (newValue != myValue) {
      myValue = newValue
      val obs = observers
      observers = Set()
      obs.foreach(_.computeValue())
    }
  }

  def apply(): T = {
    observers += caller.value //El stack primero (this del Signal anterior) pasa a la lista de observables
    //Si solo es apply() No tiene sentido
    assert(!caller.value.observers.contains(this), "cycling signal definition")
    myValue
  }
}

object Signal {
  val caller = new StackableVariables[Signal[_]](NoSignal) //Es comun a todos
  def apply[T](expr: => T) = new Signal(expr)
}

object NoSignal extends Signal[Nothing](???) {
  override def computeValue() = ()
}