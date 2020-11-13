package week4_1

class BankAccount extends Publisher {

  var dineros: Int = 0
  def currentDineros:Int = dineros

  def meter(ingreso: Int): Unit = {
    if (ingreso > 0) dineros = dineros + ingreso
    publicar
  }

  def sacar(cantidad: Int): Int = {
    if (cantidad > 0 && cantidad <= dineros) {
      dineros = dineros - cantidad
      publicar
      dineros
    }
    else throw new Error("No hay tanto dinero en su cuenta")
  }
}
