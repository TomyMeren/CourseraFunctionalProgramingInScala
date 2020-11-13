package funsets

object Main extends App {
  import FunSets._

  println(contains(singletonSet(1), 1))
  val s1 = singletonSet(1)
  val s2 = singletonSet(2)
  val s3 = singletonSet(3)
  val s = union(union(s1, s2), s3)
  println(FunSets.toString(s))
  //println(forall(s,_<3))
  println(FunSets.toString(map(s,(x: Int) => x * 2)))
}
