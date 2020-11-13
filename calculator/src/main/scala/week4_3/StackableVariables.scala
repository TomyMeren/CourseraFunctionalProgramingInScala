package week4_3

class StackableVariables[T](ini: T) {
  private var values: List[T] = List(ini)
  def value: T = values.head
  def withValue[R](newValue: T)(op: => R): R = {
    values = newValue :: values
    try op finally values = values.tail
  }
}
