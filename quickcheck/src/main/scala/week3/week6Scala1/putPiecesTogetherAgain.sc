import scala.io.Source

val in = Source.fromURL("https://raw.githubusercontent.com/glaydston/progfun/master/assignments/forcomp/src/main/resources/forcomp/linuxwords.txt")

val word = in.getLines().toSeq filter (letra => letra forall (_.isLetter))

val mnem: Map[Char, String] = Map('2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
  '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")

val charCode: Map[Char, Char] = mnem.flatMap { case (num, palabra) =>
  palabra.map(letra => (letra, num))
}

//Te devuelve el numero si le pasas una palabra
def wordCode(word: String): String = for (letra <- word) yield charCode(letra.toUpper)

//Libreria con todas las palabras asociadas a cada numero "54566" -> java, pala, caba,..
val wordsForNum: Map[String, Seq[String]] = word groupBy wordCode  withDefaultValue Seq()


// Se le pasa un numero y te pasa todas las posibilades de frases sin tener en cuenta espacios
def encode(number: String): Set[List[String]] = {
  if (number.isEmpty) Set(List())
  else {
    (for (num <- 1 to number.length;
          nomCabecera: String <- wordsForNum(number.take(num));
          nomAtras: List[String] <- encode(number.takeRight(number.length - num))
      //nomAtras:String <- wordsForNum(number.takeRight(num))
          ) yield nomCabecera :: nomAtras).toSet
  }
}
encode("7225247386")
//pack, rack,sack
//res0: Set[List[String]] = Set(List(rack, ah, re, to), List(sack, ah, re, to), List(Scala, ire, to), List(sack, air, fun), List(rack, air, fun), List(rack, bird, to), List(pack, air, fun), List(pack, ah, re, to), List(pack, bird, to), List(Scala, is, fun), List(sack, bird, to))
//res0: Set[List[String]] = Set(List(rack, ah, re, to), List(sack, ah, re, to), List(Scala, ire, to), List(rack, bird, to), List(pack, air, fun), List(pack, ah, re, to), List(pack, bird, to), List(Scala, is, fun), List(sack, bird, to), List(sack, air, fun), List(rack, air, fun))