package calculator

object Polynomial extends PolynomialInterface {
  def computeDelta(a: Signal[Double], b: Signal[Double],
    c: Signal[Double]): Signal[Double] = {
    Signal(b() * b() - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
    c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {

    val d = if (delta() < 0) 0 else delta()

    def formula(func: (Double, Double) => Double): Double = {
      func(-b(), math.sqrt(d)) / (2 * a())
    }

    Signal(Set(formula((x, y) => x + y), formula((x, y) => x - y)))
  }
}
