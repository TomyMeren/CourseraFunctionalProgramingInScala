import scala.concurrent.Future

//Recursion
def retry[T](nTimes: Int)(block: => Future[T]): Future[T] = {
  println(nTimes)
  if (nTimes == 0) Future.failed(new Exception("meh"))
  else {
    block fallbackTo {
      retry(nTimes - 1)(block)
    }
  }
}

//FoldLeft
def retryFoldLeft[T](nTimes: Int)(block: => Future[T]): Future[T] = {
  val lista = (0 to nTimes).toList
  val listFuture = lista.map(_ => block)
  val failed: Future[T] = Future.failed(new Exception("meh"))

  listFuture.foldLeft(failed)((l, block) => l fallbackTo block)
}

def retryFoldLeftCurso[T](nTimes: Int)(block: => Future[T]): Future[T] = {
  val lista = (1 to nTimes).toList
  val listFuture = lista.map(_ => () => block)
  val failed: Future[T] = Future.failed(new Exception("meh"))

  listFuture.foldLeft(failed)((l, block) => l fallbackTo {
    block()
  }) // recoverWith
}

//foldRight
def retryFoldRight[T](nTimes: Int)(block: => Future[T]): Future[T] = {
  val lista = (1 to nTimes).toList
  val listFuture = lista.map(_ => () => block)
  val failed: Future[T] = Future.failed(new Exception("meh"))

  listFuture.foldRight(() => failed)((l, block) => () => { block() fallbackTo { l() } })()
}

retryFoldRight(10)(Future.successful(2))
retryFoldRight(10)(Future.failed(new Exception("Te jodes")))

