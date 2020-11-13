trait Generator[+T] {

  self =>
  def generate: T

  def map[S](f: T => S): Generator[S] = new Generator[S] {
    def generate: S = f(self.generate)
  }

  def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
    def generate: S = f(self.generate).generate
  }
}

val integers: Generator[Int] = new Generator[Int] {
  val rand = new java.util.Random

  def generate = rand.nextInt
}

val booleans: Generator[Boolean] = new Generator[Boolean] {
  def generate: Boolean = integers.generate > 0
}

def single[T](x: T): Generator[T] = new Generator[T] {
  def generate: T = x
}

def choose(lo: Int, hi: Int): Generator[Int] =
  for (x <- integers) yield lo + x % (hi - lo)

def oneOf[T](xs: T*): Generator[T] = {
  choose(0, xs.length).map(xs(_))
}

oneOf(1,2).generate


/*def nonEmptyList: Generator[List[Int]] = for {
  integer <- integers
  nonEmpty <- lists
} yield integer :: nonEmpty*/

def lists: Generator[List[Int]] = for {
  boolean <- booleans
  list <- if (boolean) single(Nil) else for {
    integer <- integers
    nonEmpty <- lists
  } yield integer :: nonEmpty
} yield list

def pairs[T,U](x:Generator[T],y:Generator[U]):Generator[(T,U)] = for {
  xi <- x
  yi <- y
} yield (xi, yi)

def testRandom[T](x: Generator[T], numTimes: Int = 1000)
                 (f: T => Boolean) = {
  for (_ <- 0 until numTimes) yield
    assert(f(x.generate), "testfailed")
}

testRandom(pairs(lists,lists)){
  case (xi, xy) => (xi ++ xy).length >=
    xi.length
}