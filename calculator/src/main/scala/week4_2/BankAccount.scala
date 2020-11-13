package week4_2

import week4_3.Var

/**
 * 3 CASOS
 *
 *  val dinero = Var(0) => Crear nuevo Var de valor()
 *  dinero() => recupera el valor en formato Int
 *  dinero() = 4 => Actualiza el valor de dinero:Var[Int]
 */

class BankAccount  {

  val dineros:Var[Int] = Var.apply(0)

  def meter(ingreso: Int): Unit = {
    if (ingreso > 0) {
      val eur:Int = dineros.apply()
      dineros() = eur + ingreso // lo mismo que dineros.update(eur + ingreso)
    }
  }

  def sacar(cantidad: Int):Unit = {
    if (cantidad > 0 && cantidad <= dineros()) {
      val eur = dineros()
      dineros() = eur - cantidad
    }
    else throw new Error("No hay tanto dinero en su cuenta")
  }
}

