import week3.wires.{Circuits, Parameters}

object sim extends Circuits with Parameters {
  import sim._

  val ini1, ini2, sum, carry = new Wire

  halfAdder(ini1, ini2, sum, carry)
  probe("sum", sum)
  probe("carry", carry)
}
