import week4_3.Signal.caller
import week4_3.{NoSignal, Signal, StackableVariables, Var}

val a = Var.apply(1) // new var => Update(1)
/**
 * Se le asigna el valor y la funcion. No hay Stack ni observables
 */


val c = Var(a() + 2) //new var =>Update(a.apply() + 1)
/**
 * update =>  val newValue = caller.withValue(this)(a() + 2)
 *            Se añade c a la lista del stack y se ejecuta a()
 *            pasa c a la lista de observables de a y expr = (1 + 2)
 *            Se vacia el Stack
 *            my value = 3. c no tiene observables
 */

val caller2 = new StackableVariables[Signal[_]](NoSignal)
caller2.withValue(a)(2)
caller2.value

a.myValue
a.myExpr()
a.observers

caller.value

c.myValue
c.myExpr()
c.observers

a() = 3 //Update(3)
/**
 * val newValue = caller.withValue(this)(3) = 3
 * Se vacian los observer
 * Se ejecuta compute value en los observer c.computeValue()
 * val newValue = caller.withValue(this)(myExpr(a() + 2))
 * Se añade c a la lista del stack y se ejecuta a()
 *           pasa c a la lista de observables de a y expr = (3 + 2)
 *           Se vacia el Stack
 *           my valuye = 5. c no tiene observables
 */

c()
/**
 * No tiene observables
 * Se muestra el resutlado
 *
 */