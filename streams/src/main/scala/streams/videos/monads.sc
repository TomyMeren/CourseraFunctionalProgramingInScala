import com.sun.net.httpserver.Authenticator.Failure

import scala.util.{Failure, Success, Try}

val a = List(1,2,3,4,5)

a.map(x => x * 2)

def mapFalso(f: Int => Int)(l:List[Int]):List[Int] = {
  l.flatMap(num => List(f(num)))
}

def mapFalsoOpt(f: Int => Int)(l:Option[Int]):Option[Int] = {
  l.flatMap(num => Option(f(num)))
}

mapFalso(x=> x* 2)(a)
mapFalsoOpt(x=> x* 2)(None)

List(5).flatMap(x => (1 to x).toList)
  .flatMap(y => (1 to y).toList) ==
  List(5).flatMap(x =>(1 to x).toList
    .flatMap(y => (1 to y).toList))

List(5).flatMap(x => (1 to x).toList)

List(5).flatMap(x => List(x))