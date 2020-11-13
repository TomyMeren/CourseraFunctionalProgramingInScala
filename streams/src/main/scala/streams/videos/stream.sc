trait StreamT[+A] extends Seq[A] {
  def isEmpty: Boolean

  def head: A

  def tail: StreamT[A]
}

object StreamT {
  def cons[T](ele: T, t1: => StreamT[T]) = new StreamT[T] {
    def isEmpty = false
    def head = ele
    def tail = t1
  }

  def empty = new StreamT[Nothing] {
    def isEmpty = true
    def head = throw new NoSuchElementException("")
    def tail = throw new NoSuchElementException("")
  }
}