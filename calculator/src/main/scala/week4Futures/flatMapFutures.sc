import scala.util.{Try,Success,Failure}

trait Future[T] { self =>
  def flatMap[S](f: T => Future[S]): Future[S] = {
    new Future[S] {
      def onComplete(callback: Try[S] => Unit): Unit = {
        self onComplete { //self
          case Success(x) => f(x).onComplete(callback)
          case Failure(e) => callback(Failure(e))
        }
      }
    }
  }
}