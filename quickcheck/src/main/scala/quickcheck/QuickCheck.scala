package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  //lazy val genHeap: Gen[H] = const(empty)

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      k <- arbitrary[A]
      m <- oneOf(const(empty), genHeap)
    } yield insert(k, m)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min2") = forAll { (num1: Int, num2: Int) =>
    val min = math.min(num1, num2)
    findMin(insert(num2, insert(num1, empty))) == min
  }

  property("isEmpty") = forAll { (num: Int) =>
    isEmpty(deleteMin(insert(num, empty)))
  }

  property("Drop recursively") = forAll { (h: H) =>
    def isOrder(h: => H): Boolean = {
      if (isEmpty(h)) true
      else {
        val next = deleteMin(h)
        val minimo = findMin(h)

        if (isEmpty(next)) true
        else minimo <= findMin(next) && isOrder(next)
      }
    }
    isOrder(h)
  }

  property("less of two") = forAll { (h1: H, h2: H) =>
    if (isEmpty(h1) || isEmpty(h2)) true
    else {
      val min1 = findMin(h1)
      val min2 = findMin(h2)

      val minTot = findMin(meld(h1, h2))

      minTot == min1 || minTot == min2
    }
  }

  property("the union of two is the union of one with the othe other with de min") = forAll { (h1: H, h2: H) =>
    if (isEmpty(h1)) true
    else {
      val min = findMin(h1)
      val h1Tail = deleteMin(h1)
      val h2Prima = insert(min,h2)

      meld(h1Tail,h2Prima) == meld(h1,h2)
    }
  }
}

