class Polynomial(val terms0: Map[Int, Double]) {

  def this(binding: (Int, Double)*) = this(binding.toMap)

  val terms = terms0 withDefaultValue 0.0

  //Como prevalece el ultimo...
  def +(other: Polynomial): Polynomial = new Polynomial(terms ++ (other.terms map adjust))

  def adjust(ele: (Int, Double)): (Int, Double) = {
    val (exp, num) = ele
    exp -> (terms(exp) + num)
  }

  override def toString: String = (for ((coef, num) <- terms.toList.sorted.reverse) yield
    coef + "x^" + num) mkString (" + ")

  def ++(other: Polynomial): Polynomial = {
    new Polynomial((other.terms foldLeft terms) (addTerm))
  }

  def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
    val (exp, num) = term
    //terms.updated(exp, num + terms(exp))
    terms + (exp -> (terms(exp) + num))
  }
}

val p1 = new Polynomial(Map(1 -> 2.0, 3 -> 4.0, 5 -> 6.2))
val p2 = new Polynomial(Map(0 -> 3.0, 3 -> 7.0))

p1 + p2
p1 ++ p2
