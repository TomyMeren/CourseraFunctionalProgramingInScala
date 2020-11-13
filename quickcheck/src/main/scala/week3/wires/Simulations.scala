/*
package week3.wires

abstract class Simulations {

  /**
    * Accion que no revibe parametros y no devuelve nada. Solo Side Effect
    */
  type Action = () => Unit

  case class Event(time: Int, action: Action)

  private var curtime: Int = 0
  def currentTime: Int = curtime

  private var agenda: List[Event] = List()

  def insert(ag: List[Event], item: Event): List[Event] = ag match {
    case first :: rest if first.time <= item.time => first :: insert(rest, item) // ordenado el primero primero
    case _ => item :: ag
  }

  def afterDelay(delay: Int)(block: => Unit): Unit = {
    val item = Event(currentTime + delay, () => block)
    agenda = insert(agenda, item)
  }

  def run(): Unit = {
    afterDelay(0) {
      println(s"Esto empieza en $currentTime")
    }
    loop()
  }

  private def loop(): Unit = agenda match {
    case first :: rest =>
      agenda = rest
      curtime = first.time
      first.action()
      loop()
    case _ =>
  }
}
*/
