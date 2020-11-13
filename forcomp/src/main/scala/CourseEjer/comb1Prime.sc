def isPrime(n: Int): Boolean = !(2 until n).exists(num => n % num == 0)

isPrime(6)

val a = List(1, 2, 3)
val b = List(4, 5, 6)

for (a1 <- a;
     b1 <- b) yield a1 * b1

for ((a1, b1) <- a zip b) yield a1 * b1
