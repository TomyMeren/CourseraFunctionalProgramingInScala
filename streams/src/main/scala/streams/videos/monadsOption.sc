import scala.util.{Try,Failure, Success}
val mapaVacio = Map("b" -> 1)
val mapaVacio2 = Map(1 -> "@")

val b = Success(3)
//val c = Failure(Exception)
val e = Try(6/0)

b.map(_*2)
//c.map(x => x*2)


Try(6/0).flatMap(x=> Success(x))
Try(6/0)