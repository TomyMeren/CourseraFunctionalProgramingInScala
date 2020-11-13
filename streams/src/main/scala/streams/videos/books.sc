case class Book(title: String, authores: List[String])

val biblio: Set[Book] = Set(Book("El perro valiente", List("Tomy", "Cris")),
  Book("El gatuno moruno", List("Bego","Tomy")),
  Book("La gacela alegre", List("Bego", "Tomy", "Guille")))

for (book <- biblio;
     autor <- book.authores
     if autor.startsWith("T")) yield book.title

biblio.flatMap(libro =>
  libro.authores.filter(autor => autor.startsWith("T"))
    .map(_ => libro.title))

for (book <- biblio
     if book.title contains "El") yield book.title

for (book1 <- biblio;
     book2 <- biblio
     if book1.title < book2.title;
     autor1 <- book1.authores;
     autor2 <- book2.authores
     if autor1 == autor2) yield autor1