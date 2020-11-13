import streams.videos.waterPuring.Pouring

object test {
  val problem = new Pouring(Vector(4, 7))
  problem.moves
  //problem.pathSets(5)
  problem.solution(6)

  //Pruebas Tomy

  val estado = Vector(4, 6, 1)

  problem.proudOn(0,1).change(estado)
  problem.proudOn(1,0).change(estado)
  problem.proudOn(2,1).change(estado)


}
