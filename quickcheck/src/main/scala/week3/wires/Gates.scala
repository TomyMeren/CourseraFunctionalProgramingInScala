/*
package week3.wires

abstract class Gates extends Simulations {

  def InvertedDelay: Int
  def AndDelay: Int
  def OrDelay: Int

  class Wire {
    private var sigVal = false
    def getSigVal = sigVal

    private var action: List[Action] = List()

    def setSignal(s: Boolean) =
      if (s != sigVal) {
        sigVal = s
        action foreach (_())
      }

    def addAction(act: Action) = {
      action = act :: action
      act()
    }
  }

  def inverter(input: Wire, output: Wire): Unit = {
    def invertAccion(): Unit = {
      val inputSignal = input.getSigVal
      afterDelay(InvertedDelay) {
        output setSignal !inputSignal
      }
    }
    input addAction invertAccion
  }

  def andGate(input1: Wire, input2: Wire, output: Wire): Unit = {
    def andAccion(): Unit = {
      val inputSignal1 = input1.getSigVal
      val inputSignal2 = input2.getSigVal
      afterDelay(AndDelay) {
        output setSignal (inputSignal1 & inputSignal2)
      }
    }
    input1 addAction andAccion
    input2 addAction andAccion
  }

  /*def orGate(input1: Wire, input2: Wire, output: Wire): Unit = {
    def orAccion(): Unit = {
      val inputSignal1 = input1.getSigVal
      val inputSignal2 = input2.getSigVal
      afterDelay(OrDelay) {
        output setSignal (inputSignal1 | inputSignal2)
      }
    }
    input1 addAction orAccion
    input2 addAction orAccion
  }*/

  def orGate(in1: Wire, in2: Wire, output: Wire): Unit = {
    val notIn1, notIn2, notOut = new Wire
    inverter(in1, notIn1)
    inverter(in2, notIn2)
    andGate(notIn1, notIn2, notOut)
    inverter(notOut, output)
  }

  def probe(name:String,wire:Wire):Unit = {
    def actionChivata():Unit = {
      println(s"El wire $name tiene un resultado de ${wire.getSigVal} en el tiempo $currentTime")
    }
    wire addAction actionChivata
  }
}
*/
