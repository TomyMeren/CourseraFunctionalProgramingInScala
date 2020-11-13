package week4_1

class Consolidator(cuentas:List[BankAccount]) extends Subscriber {

  cuentas.foreach(_.subscribirse(this))

  compute

  private def compute = cuentas.map(_.currentDineros).sum

  def handler = compute

  def totalBalance = compute
}
