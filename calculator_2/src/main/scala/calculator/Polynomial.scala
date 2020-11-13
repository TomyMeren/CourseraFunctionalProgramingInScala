package calculator

object Polynomial extends PolynomialInterface {
  def computeDelta(a: Signal[Double], b: Signal[Double],
    c: Signal[Double]): Signal[Double] = {
    Signal(b() * b() - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
    c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {

    //val d = delta()

    def formula(func: (Double, Double) => Double): Double = {
      func(-b(), math.sqrt(delta())) / (2 * a())
    }

    Signal(
      if (delta() < 0.0) Set()
      else
        Set(formula((x, y) => x + y), formula((x, y) => x - y))
    )
  }
}
