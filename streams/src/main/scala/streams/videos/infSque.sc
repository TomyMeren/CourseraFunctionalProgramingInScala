def from(n: Int): Stream[Int] = n #:: from(n + 1)


val nats = from(2)
val n4s = nats.map(_ * 4)

n4s.take(100).toList


def sieve(s: Stream[Int]): Stream[Int] = {
  s.head #:: sieve(s.tail.filter(_ % s.head != 0))
}

sieve(nats).take(60).toList

def sqrtStream(n: Double): Stream[Double] = {
  def improve(d: Double) = (d + n / d) / 2
  lazy val guesses: Stream[Double] = 1 #:: (guesses map improve)
    guesses
}

sqrtStream(121).take(10).toList

val N = 4

val xs = from(1) map (_ * N)

val ys = from(1) filter (_ % N == 0)

xs.take(20).toList
ys.take(20).toList