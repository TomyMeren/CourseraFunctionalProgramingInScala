package week4_2

import week4_3.Signal

/**
 * 1 CASOS +
 *
 * Signal(4)
 */

object accounts extends App {

  def consolidater(acct: List[BankAccount]): Signal[Int] = {
    Signal.apply(acct.map(_.dineros()).sum)
  }

  val a = new BankAccount
  val b = new BankAccount

  val acc: Signal[Int] = consolidater(List(a, b))

  a.meter(10)
  b.meter(30)

  val change = Signal(845.95)
  val inDolar = Signal(acc() * change())

  println(inDolar())

  b sacar 10

  println(inDolar())
}

/**
 * Al crearlo a traves del objeto Signal.apply creamos un nuevo Signal
 * a su vez hacemos el apply a los Signal de dentro a.dineros y b.dineros
 *
 * a.dineros => new Signal => update(expr) => computeValue => Actualizar myValue = newValue(atraves del caller)
 *                                                         => ejecutar observer => no hay
 *                                                         => vacia observer => se quedqa como esta
 *                                           => Actualizar myExpr
 *             => caller = new stackable
 *
 * b.dineros = a.dineros()
 *
 * a.dineros.apply() => Devuelve el valor
 *                   => Añade a los observer el primer valor de los caller => No hay nada
 */