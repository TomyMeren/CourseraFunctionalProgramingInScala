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



trait Tree

case class Inner(left: Tree, right: Tree) extends Tree

case class Leaft(x: Int) extends Tree

def leafts: Generator[Leaft] = {
  for (integer <- integers) yield Leaft(integer)
}

def inners: Generator[Inner] = {
  for (leaftLeft <- tree;
       leaftRight <- tree)
    yield Inner(leaftLeft, leaftRight)
}

def tree: Generator[Tree] = {
  for (boolean <- booleans;
       tree <- if (boolean) leafts else inners)
    yield tree
}

tree.generate
tree.generate
tree.generate
tree.generate
tree.getClass

