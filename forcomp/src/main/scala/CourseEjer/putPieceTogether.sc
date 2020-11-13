import scala.io.Source

val in = Source.fromURL("https://raw.githubusercontent.com/glaydston/progfun/master/assignments/forcomp/src/main/resources/forcomp/linuxwords.txt")

val word = in.getLines().toSeq filter (letra => letra forall (_.isLetter))

val mnem: Map[Char, String] = Map('2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
  '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")

val charCode: Map[Char, Char] =
  for ((num, word) <- mnem;
       letter <- word) yield letter -> num

//Te devuelve el numero si le pasas una letra
def wordCode(word: String): String =
  word.toUpperCase map charCode

//Libreria con todas las palabras asociadas a cada numero
val wordsForNum: Map[String, Seq[String]] =
  word groupBy wordCode withDefaultValue Seq()


// Se le pasa un numero y te pasa todas las posibilades de frases sin tener en cuenta espacios
def encode(number: String): Set[List[String]] = {
  if (number.isEmpty) Set(List())
  else {
    for {split <- 1 to number.length;
         word: String <- wordsForNum(number.take(split))
         rest: List[String] <- encode(number drop split)
         } yield {
      word :: rest // concatear nada a una lsita devuelve siempre nada
    }: List[String]
  }.toSet
}


def encodeTomy(number: String): Set[List[String]] = {

  def combNum(num: String): Set[List[String]] = {
    if (num.isEmpty) Set(List())
    else {
      for (split <- 1 to num.length;
           rest: List[String] <- combNum(num.drop(split)))
        yield {
          num.take(split) :: rest
        }
    }.toSet
  }

  def comb(input: List[Seq[String]]): Seq[List[String]] = {
    if (input.isEmpty) List(List())
    else {
      for (elem <- input.head;
           resto <- comb(input.tail))
        yield elem :: resto
    }
  }

  def phaseGen(setListaNumeros: Set[List[String]]): Set[List[String]] = {
    if (setListaNumeros.isEmpty) Set(List())
    else {
      comb(setListaNumeros.head.map(wordsForNum(_))).toSet ++
        phaseGen(setListaNumeros.tail)
    }
  }

  def phaseGen_old(setListaNumeros: Set[List[String]]): Set[List[String]] = {
    println("a")
    if (setListaNumeros.isEmpty) Set(List())
    else {
      for (numero: String <- setListaNumeros.head;
           letras: String <- wordsForNum(numero);
           rest <- phaseGen(setListaNumeros.tail)
           ) yield {
        letras :: rest
      }
    }.toSet
  }

  phaseGen(combNum(number))
}


encode("7225247386")
encode("7225")

//encodeTomy("7225")
encodeTomy("7225247386")

/**
 * ¿Por que el el encode original seleciona mensajes de todos los numeros?
 */