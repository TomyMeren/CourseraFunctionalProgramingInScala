import java.util.Random

import week3.week1Scala2._

trait Tree

case class Inner(left: Tree, right: Tree) extends Tree

case class Leaft(n: Int) extends Tree

val integers: Generator[Int] = new Generator[Int] {
  val rand = new Random()
  def generate: Int = rand.nextInt()
}

val booleans = for (int <- integers) yield int > 0

def leaft: Generator[Leaft] = for {
  int <- integers
} yield Leaft(int)

def inner: Generator[Inner] = for {
  leaft1 <- tree
  leaft2 <- tree
} yield Inner(leaft1,leaft2)

def tree: Generator[Tree] = for {
  leaftOrInner <-  booleans
  raiz <- if(leaftOrInner) leaft else inner
} yield raiz

tree.generate
tree.generate
inner.generate

