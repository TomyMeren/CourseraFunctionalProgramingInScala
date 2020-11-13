package patmat

import Huffman._

object Main extends App {
  /*  println(Huffman.times(List('x', 't', 'e', 'x', 't', 'x', 'p')))
    println(Huffman.times(List('x', 'x')))*/

/*  val forklist: List[CodeTree] = List(
    Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3),
    Leaf('x', 4),
    Fork(Leaf('f', 3), Leaf('k', 2), List('f', 'k'), 5),
    Leaf('j', 10))*/

  println(decodedSecret)

  println(secret)
  println(encode(frenchCode)(decodedSecret))
  println(quickEncode(frenchCode)(decodedSecret))

  println(codeBits(convert(frenchCode))('h') ::: codeBits(convert(frenchCode))('u'))

  combine(List(Leaf('e',1)))
}
