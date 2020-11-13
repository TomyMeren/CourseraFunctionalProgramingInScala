import forcomp.Anagrams._

val sentence = List("Yes", "man")
//val result = List(List(en, as, my), List(en, my, as), List(man, yes), List(men, say), List(as, en, my), List(as, my, en), List(sane, my), List(Sean, my), List(my, en, as), List(my, as, en), List(my, sane), List(my, Sean), List(say, men), List(yes, man))

def sentenceAnagrams(sentence: Sentence): List[Sentence] = {

  def aux(ocu: Occurrences): List[Sentence] = {
    if (ocu.isEmpty) List(List())
    else {
      for {ocurr: Occurrences <- combinations(ocu)
           sent: Word <- dictionaryByOccurrences(ocurr)
           prev <- aux(subtract(ocu, ocurr))
           } yield sent :: prev
    }
  }
  val sentOcurr: Occurrences = sentenceOccurrences(sentence)
  aux(sentOcurr)
}

sentenceAnagrams(List("Yes", "man"))

def sentenceAnagramsMemo(sentence: Sentence): List[Sentence] = {

  def aux(ocu: Occurrences, reps:List[(Occurrences, List[Word])]): List[Sentence] = {

    val repsMap = reps.toMap
    if (ocu.isEmpty) List(List())
    else {
      for {ocurr: Occurrences <- combinations(ocu)
           sent: Word <- if(reps.contains(ocurr)) repsMap(ocurr) else  dictionaryByOccurrences(ocurr)
           prev <- aux(subtract(ocu, ocurr),(ocurr,dictionaryByOccurrences(ocurr)) :: reps)
           } yield sent :: prev
    }
  }
  val sentOcurr: Occurrences = sentenceOccurrences(sentence)
  aux(sentOcurr,Nil)
}

sentenceAnagramsMemo(List("Yes", "man"))